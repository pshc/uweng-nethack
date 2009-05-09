{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
             NoMonomorphismRestriction, RelaxedPolyRec #-}
module Level where

import Control.Monad
import Data.Array
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P

-- Model
type Rect = (Int, Int, Int, Int)
type Pos = (Int, Int)

data Level = Level { levelName :: String, levelTiles :: Array Pos Char,
                     prevLevels, nextLevels :: [Level],
                     levelGeometry :: (Geometry, Geometry),
                     levelObjs :: Map ObjPos [Obj],
                     levelRegions :: [(Region, Rect)],
                     levelSpeRegions :: [(LevRegion, LevRegion, SpeRegion)],
                     levelFlags :: [LevelFlag], levelRandomPlaces :: [Pos],
                     levelRandomObjs, levelRandomMons :: [Char] }

initLevel = Level { levelName = "untitled",
                    levelTiles = listArray ((0, 0), (79, 21)) (repeat ' '),
                    prevLevels = [], nextLevels = [],
                    levelGeometry = (Center, Center),
                    levelObjs = Map.empty,
                    levelRegions = [], levelSpeRegions = [],
                    levelFlags = [], levelRandomPlaces = [],
                    levelRandomObjs = [], levelRandomMons = [] }

data Rand a = Fixed a | Random | RandomIndex Int deriving (Eq, Ord)
data LevRegion = Rect Rect | LevRegion Rect

data LevelFlag = NoTeleport | HardFloor deriving (Bounded, Enum, Show)
data Region = Region (Rand Lighting) RegionType (Maybe (Fill, Maybe Bool))
              | NonDiggable -- | Branch Unknown
data Lighting = Lit | Unlit deriving (Bounded, Enum, Show)
data Fill = Filled | Unfilled deriving (Bounded, Enum, Show)
data RegionType = Ordinary | Morgue | Barracks | Throne
                  deriving (Bounded, Enum, Show)
data SpeRegion = Stair StairDir | TeleportRegion StairDir

type ObjPos = Rand Pos
data Obj = Obj (Rand Char) (Rand String)
               (Maybe (Rand Blessing, Maybe (Spe, Maybe String)))
           | Container (Rand Char) (Rand String) [Obj]
           | Monst (Rand Char) (Rand String) [Behaviour]
           | Trap (Rand TrapType)
           | Engraving (Rand Ink) String
           | Door (Rand DoorType) | Drawbridge Dir (Rand DoorType)
           | Fountain -- Must be last

instance Enum Obj where
    fromEnum (Obj _ _ _)      = 0
    fromEnum (Monst _ _ _)    = 1
    fromEnum (Trap _)         = 2
    fromEnum (Engraving _ _)  = 3
    fromEnum (Door _)         = 4
    fromEnum (Drawbridge _ _) = 5
    fromEnum Fountain         = 6
    toEnum 6 = Fountain
    toEnum _ = error "No Obj toEnum!"

instance Bounded Obj where
    minBound = error "No Obj minBound!"
    maxBound = Fountain

objChar (Obj ch _ _)    = case ch of Fixed c -> c; otherwise -> 'R'
objChar (Monst sym _ _) = case sym of Fixed c -> c; otherwise -> 'M'
objChar o               = "  ^ ~++{" !! fromEnum o

type Spe = Int
data Blessing = Blessed | Uncursed | Cursed deriving (Bounded, Enum, Show)
data Behaviour = Hostile | Peaceful | Asleep deriving (Bounded, Enum, Show)
data TrapType = TrapDoor | Pit deriving (Bounded, Enum, Show)
data StairDir = Up | Down deriving (Bounded, Enum, Show)
data Ink = Burn | Blood deriving (Bounded, Enum, Show)
data Dir = North | East | South | West deriving (Bounded, Enum, Show)
data DoorType = Open | Closed | Locked deriving (Bounded, Enum, Show)
data Geometry = Center deriving (Bounded, Enum, Show)

levelSize (Level { levelTiles = ts }) = let (_, (x, y)) = bounds ts
                                        in (x + 1, y + 1)

class Save a where
    save :: a -> ShowS

commas f (x:xs) = foldl (\ss y -> ss . comma' . f y) (f x) xs
commas f []     = id

comma' = showString ", "
nl' = showString "\n"

saveLevels :: FilePath -> Level -> IO Bool
saveLevels f l = do let ls = reverse (prevLevels l) ++ [l] ++ nextLevels l
                        t  = foldl (\ss l -> ss . save l)
                                   ("# Generated by level editor\n\n" ++) ls ""
                    catch (writeFile f t >> return True) (const $ return False)

instance Monad (Either String) where
    (Right x) >>= f = f x
    (Left s)  >>= _ = Left s
    return          = Right

data LoadState = NoState | LoadState Level (Maybe ContainerIndex)
type LevelParser = GenParser Char LoadState
type ContainerIndex = (ObjPos, Int)

putLevel f = updateState $ \(LoadState lev c) -> LoadState (f lev) c

parseEnum :: (Bounded a, Enum a, Show a) => LevelParser a
parseEnum = choice [reserved (show c) >> return c | c <- [minBound..maxBound]]

quotedEnum :: (Bounded a, Enum a, Show a) => LevelParser a
quotedEnum = do e <- stringLiteral
                maybe (error $ "Unexpected " ++ show e) return
                      (find (equals e) [minBound .. maxBound])
  where
    (a:as) `equals` b = let (c:cs) = show b in a == toLower c && go as cs
    go (' ':a:as) (b:bs) | isUpper b = a == toLower b && go as bs
    go (a:as) (b:bs)     | isLower b = a == b && go as bs
    go [] [] = True
    go _  _  = False

loadLevels :: FilePath -> IO (Either String Level)
loadLevels fp = catch (runParser parseLevels NoState fp `fmap` readFile fp
                        >>= return . either (fail . show) return)
                      (return . fail . show)
  where
    parseLevels = do (m:ms) <- whiteSpace >> many readMaze
                     eof
                     return $ m { nextLevels = ms }
    readMaze = do nm <- reserved "MAZE" >> colon >> commaed stringLiteral
                  c <- rand charLiteral
                  whiteSpace
                  setState $ LoadState (initLevel { levelName = nm }) Nothing
                  many1 $ choice lineParsers
                  LoadState lev _ <- getState
                  return lev

    lineParsers = readMap : [reserved r >> colon >> f >> whiteSpace
                             | (r, f) <- reservedParsers]

readMap = do try (string "MAP" >> newline)
             (lines, w, h) <- readLines
             guard (w <= 80 && h <= 22)
             let m = listArray ((0, 0), (w-1, h-1)) (concat (transpose lines))
             putLevel (\l -> l { levelTiles = m })
  where
    readLines = (reserved "ENDMAP" >> return ([], 0, 0)) <|> do
                    l <- many (noneOf "\r\n")
                    (ls, w', h) <- newline >> readLines
                    let lw = length l
                        w  = if null ls then lw else if lw == w' then w' else
                                error "Mismatched tile line length!"
                    return (l:ls, w, h + 1)

reservedParsers :: [(String, LevelParser ())]
reservedParsers = [
    ("GEOMETRY", do g1 <- commaed parseEnum; g2 <- parseEnum
                    putLevel (\l -> l { levelGeometry = (g1, g2) })),
    ("FLAGS", do flags <- parseEnum `sepBy` comma
                 putLevel (\l -> l { levelFlags = flags })),
    ("RANDOM_PLACES", do ps <- tuple2 `sepBy` comma
                         putLevel (\l -> l { levelRandomPlaces = ps })),
    ("RANDOM_MONSTERS", do ms <- charLiteral `sepBy` comma
                           putLevel (\l -> l { levelRandomMons = ms })),
    ("RANDOM_OBJECTS", do os <- charLiteral `sepBy` comma
                          putLevel (\l -> l { levelRandomObjs = os })),
    ("TELEPORT_REGION", speRegion TeleportRegion),
    ("STAIR",           speRegion Stair),
    ("FOUNTAIN",   withPos Fountain),
    ("DOOR",       commaed (rand parseEnum) >>= withPos . Door),
    ("DRAWBRIDGE", do addObj <- commaed objPos
                      dir <- commaed parseEnum
                      st <- rand parseEnum
                      addObj (Drawbridge dir st)),
    ("TRAP",       commaed (rand quotedEnum) >>= withPos . Trap),
    ("OBJECT",     do typ <- commaed (randIndex "object" charLiteral)
                      nm <- commaed (rand stringLiteral)
                      misc <- maybeBoth (commaed (rand parseEnum))
                              $ maybeBoth (commaed decimal)
                                          (optionMaybe (commaed stringLiteral))
                      withPos $ Obj typ nm misc),
    ("CONTAINER",  do typ <- commaed (randIndex "object" charLiteral)
                      nm <- commaed (rand stringLiteral)
                      withPos $ Container typ nm []),
    ("ENGRAVING",  do addObj <- commaed objPos
                      typ <- commaed (rand parseEnum)
                      text <- stringLiteral
                      addObj (Engraving typ text))]
  where
    speRegion f = do r1 <- commaed levRegion; r2 <- commaed levRegion
                     spe <- f `fmap` parseEnum
                     putLevel (\l -> l { levelSpeRegions = (r1, r2, spe)
                                         : levelSpeRegions l })

    withPos obj = objPos >>= ($ obj)

    maybeBoth a b = optionMaybe $ a >>= \r -> b >>= \s -> return (r, s)

    setContainer p (LoadState l _) = let v = Map.lookup p (levelObjs l)
                                     in LoadState l (Just (p,maybe 0 length v))

reservedNames = "MAZE" : "MAP" : "ENDMAP"
                : "random" : "place" : "object" : "contained"
                : map fst reservedParsers

lexer = P.makeTokenParser $ emptyDef { P.commentLine = "#",
                                       P.caseSensitive = False,
                                       P.reservedNames = reservedNames }

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
squares = P.squares lexer
comma = P.comma lexer
colon = P.colon lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
decimal = fromIntegral `fmap` P.decimal lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

commaed f = f >>= \a -> comma >> return a

tuple2 = parens $ do a <- commaed decimal; b <- decimal
                     return (a, b)

-- Consumes an object's position and returns a LevelParser that doesn't
-- parse anything but inserts the given object into the saved position
objPos :: LevelParser (Obj -> LevelParser ())
objPos = levelInsert `fmap` randIndex "place" tuple2
         <|> (reserved "contained" >> return contInsert)
  where
    levelInsert p o = do LoadState l c <- getState
                         let dest = Map.findWithDefault [] p (levelObjs l)
                             n    = length dest
                             c'   = case o of Container _ _ _ -> Just (p, n)
                                              otherwise       -> c
                             os'  = Map.insert p (o:dest) (levelObjs l)
                         setState $ LoadState (l { levelObjs = os' }) c'

    contInsert o = do LoadState l (Just (p, n)) <- getState
                      let os' = Map.adjust (insertAt n o) p (levelObjs l)
                      putLevel (\l -> l { levelObjs = os' })

    insertAt n o ss = let (bef, (Container a b os):aft) = splitAt n ss
                      in bef ++ Container a b (o:os) : aft

tuple4 = parens $ do [a, b, c] <- count 3 (commaed decimal); d <- decimal
                     return (a, b, c, d)

levRegion = (reserved "levregion" >> tuple4 >>= return . LevRegion)
            <|> (tuple4 >>= return . Rect)

rand c = (reserved "random" >> return Random) <|> (c >>= return . Fixed)
randIndex s c = (reserved s >> squares decimal >>= return . RandomIndex)
                <|> rand c

instance Save Level where
    save lv = showString "MAZE: " . save (levelName lv)
              . (if null (levelFlags lv) then id else
                 showString ",' '\nFLAGS: " . commas save (levelFlags lv))
              . showString "\nGEOMETRY: center,center"
              . ("\nMAP\n" ++) . (showTiles (levelSize lv) (levelTiles lv) ++)
              . showString "ENDMAP\n"
              . permute "RANDOM_PLACES: " levelRandomPlaces
              . permute "RANDOM_MONSTERS: " levelRandomMons
              . permute "RANDOM_OBJECTS: " levelRandomObjs
              . foldl (\ss r -> ss . save r) ("" ++) (levelSpeRegions lv)
              . foldl (\ss o -> ss . save o) (showString "# Objects\n")
                      (concat sortedObjs)
      where
        -- Unfortunately `elems` is top-to-bottom, left-to-right
        showTiles (w, h) ts = go (0, 0)
          where
            go (x, y) | x < w && y < h = ts ! (x, y) : go (x+1, y)
                      | y < h          = '\n' : go (0, y+1)
                      | otherwise      = []

        permute nm f | null (f lv) = id
                     | otherwise   = showString nm . commas save (f lv) . nl'

        sortedObjs = let os  = concat [map ((,) p) ol
                                       | (p, ol) <- Map.assocs (levelObjs lv)]
                         acc = replicate (fromEnum (maxBound :: Obj) + 1) []
                     in foldr sortObj acc os
        sortObj o acc = let (bef, (os:aft)) = splitAt (fromEnum (snd o)) acc
                        in bef ++ [o:os] ++ aft

instance Save (LevRegion, LevRegion, SpeRegion) where
    save (r1, r2, spe) = let (f, dir) = saveSpe spe
                         in f . save r1 . comma' . save r2 . comma' . dir . nl'
      where
        saveSpe (Stair dir)          = (showString "STAIR: ", save dir)
        saveSpe (TeleportRegion dir) = (("TELEPORT_REGION: " ++), save dir)

instance Save LevRegion where
    save (LevRegion reg) = showString "levregion" . save reg
    save (Rect reg)      = save reg

instance Save (ObjPos, Obj) where
    save (p, o) = case o of
      Obj sym nm _     -> showString "OBJECT: " . save sym . comma' . save nm
                          . comma' . save p . nl' -- TODO
      Monst sym nm bh  -> showString "MONSTER: " . save sym . comma' . save nm
                          . comma' . save p . comma' . commas save bh
      Trap typ         -> showString "TRAP: " . save typ . q
      Fountain         -> showString "FOUNTAIN: " . save p . nl'
      Engraving ink s  -> ("ENGRAVING: " ++) . save ink . comma' . save s . q
      Door typ         -> showString "DOOR: " . save typ . q
      Drawbridge dir t -> ("DRAWBRIDGE: " ++) . save dir . comma' . save t . q
     where q = comma' . save p . nl'

showsLower :: Show a => a -> ShowS
showsLower = showString . map toLower . show

showsQuoted :: Show a => a -> ShowS
showsQuoted o = let (s:ss) = show o
                in (['"', toLower s] ++) . foldr quote ('"':) ss
  where
    quote c | c == '"'  = (("\\\"" ++) .)
            | isUpper c = (([' ', toLower c] ++) .)
            | otherwise = ((c :) .)

instance Save Blessing where save = showsLower
instance Save Behaviour where save = showsLower
instance Save TrapType where save = showsQuoted
instance Save StairDir where save = showsLower
instance Save Ink where save = showsLower
instance Save Dir where save = showsLower
instance Save DoorType where save = showsLower

-- TODO: Not sure if the escaping convention is the same...
instance Save Pos where save = shows
instance Save Rect where save = shows
instance Save String where save = shows
instance Save Char where save = shows

saveRandom _ Random          = showString "random"
saveRandom _ (Fixed x)       = save x
saveRandom s (RandomIndex i) = showString s . ('[':) . shows i . (']':)

instance Save a => Save (Rand a) where
    save Random          = showString "random"
    save (Fixed x)       = save x
    save (RandomIndex i) = error "Unknown RandomIndex prefix"

instance Save LevelFlag where
    save NoTeleport = showString "noteleport"
    save HardFloor  = showString "hardfloor"

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
