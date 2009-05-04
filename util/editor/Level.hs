{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
module Level where

import Control.Monad
import Data.Array
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
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
                     levelFlags :: [LevelFlag], levelRandomPlaces :: [Pos],
                     levelRandomObjs, levelRandomMons :: [Char] }

initLevel = Level { levelName = "untitled",
                    levelTiles = listArray ((0, 0), (79, 21)) (repeat ' '),
                    prevLevels = [], nextLevels = [],
                    levelGeometry = (Center, Center),
                    levelObjs = Map.empty,
                    levelRegions = [],
                    levelFlags = [], levelRandomPlaces = [],
                    levelRandomObjs = [], levelRandomMons = [] }

data Rand a = Random | Fixed a

data LevelFlag = NoTeleport | HardFloor deriving (Bounded, Enum, Show)
data Region = Region (Rand Lighting) RegionType (Maybe (Fill, Maybe Bool))
              | NonDiggable -- | Branch Unknown
data Lighting = Lit | Unlit deriving (Bounded, Enum, Show)
data Fill = Filled | Unfilled deriving (Bounded, Enum, Show)
data RegionType = Ordinary | Morgue | Barracks | Throne
                  deriving (Bounded, Enum, Show)

data ObjPos = ObjPos Pos | RandomPos | RandomPosIndex Int | Contained
              deriving (Eq, Ord)
data Obj = Obj ObjSym (Rand String)
               (Maybe (Rand Blessing, Maybe (Spe, Maybe String)))
           | Monst MonstSym (Rand String) [Behaviour]
           | Trap (Rand TrapType)
           | Stair StairDir | Engraving Ink String
           | Door (Rand DoorType) | Drawbridge Dir (Rand DoorType)
           | Fountain -- Must be last

instance Enum Obj where
    fromEnum (Obj _ _ _)      = 0
    fromEnum (Monst _ _ _)    = 1
    fromEnum (Trap _)         = 2
    fromEnum (Stair _)        = 3
    fromEnum (Engraving _ _)  = 4
    fromEnum (Door _)         = 5
    fromEnum (Drawbridge _ _) = 6
    fromEnum Fountain         = 7
    toEnum 7 = Fountain
    toEnum _ = error "No Obj toEnum!"

instance Bounded Obj where
    minBound = error "No Obj minBound!"
    maxBound = Fountain

objChar (Obj ch _ _)    = case ch of ObjChar c -> c; otherwise -> 'R'
objChar (Monst sym _ _) = case sym of MonstChar c -> c; otherwise -> 'M'
objChar (Stair dir)     = case dir of Up -> '<'; Down -> '>'
objChar o               = "  ^ ~++{" !! fromEnum o

-- WTF levregion
data ObjSym = ObjChar Char | RandomObj | RandomObjIndex Int
type Spe = Int
data MonstSym = MonstChar Char | RandomMonst | RandomMonstIndex Int
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

loadLevels :: FilePath -> IO (Either String Level)
loadLevels fp = flip catch (return . Left . show) $ do
                   r <- readFile fp >>= return . go 1 . lines
                   case r of Right (l:ls) -> let l' = l { nextLevels = ls }
                                             in return (Right l')
                             Right []     -> return (Left "No levels loaded")
                             Left err     -> return (Left err)
  where
    go n ls = let (skip, start) = break (isFirstToken "MAZE") ls
              in case start of
        []     -> return []
        (m:ms) -> do let (this, next) = break (isFirstToken "MAZE") ms
                     (lev, n') <- loadLevel fp (n + length skip) (m : this)
                     rest <- go n' next
                     return (lev : rest)

isFirstToken tok = (== tok) . fst . head . lex

type LevelParser = GenParser Char Level

parseEnum :: (Bounded a, Enum a, Show a) => LevelParser a
parseEnum = choice [try (identifier >>= guard . equals ctor >> return ctor)
                    | ctor <- [minBound .. maxBound]]
  where
    a `equals` b = map toLower (show a) == map toLower b

loadLevel :: (Monad m) => FilePath -> Int -> [String] -> m (Level, Int)
loadLevel fp = parseLines initLevel
  where
    parseLines lev n []     = return (lev, n)
    parseLines lev n (l:ls) = let ts = getTokens l
      in if null ts then parseLines lev (n + 1) ls else case head ts of
        "MAP" -> do let (tiles, ls') = break (isFirstToken "ENDMAP") ls
                        n'           = n + length tiles + 2
                    tileArray <- readTiles tiles
                    parseLines (lev { levelTiles = tileArray }) n' (tail ls')
        t     -> case runParser (parseLine n) lev fp l of
                     Left err   -> fail (show err)
                     Right lev' -> parseLines lev' (n + 1) ls

    readTiles ls = let w = length (head ls)
                       h = length ls
                       b = ((0, 0), (w - 1, h - 1))
                   in if w > 80 || h > 22 then fail ("Map is too large: "
                                                     ++ show (w, h))
                      else return (listArray b (concat (transpose ls)))

    parseLine :: Int -> LevelParser Level
    parseLine lineNum = do srcPos <- getPosition
                           setPosition (setSourceLine srcPos lineNum)
                           whiteSpace
                           i <- identifier >>= got
                           getState

    got "MAZE" = do nm <- colon >> stringLiteral
                    return ()
                    --ch <- comma >> randChar
    got "GEOMETRY" = do g1 <- colon >> parseEnum
                        g2 <- comma >> parseEnum
                        updateState (\l -> l { levelGeometry = (g1,g2) })
    got "FLAGS" = do flags <- colon >> parseEnum `sepBy` comma
                     updateState (\l -> l { levelFlags = flags })
    got "RANDOM_PLACES" = do ps <- colon >> tuple2 `sepBy` comma
                             updateState (\l -> l { levelRandomPlaces = ps })
    got "RANDOM_MONSTERS" = do ms <- colon >> charLiteral `sepBy` comma
                               updateState (\l -> l { levelRandomMons = ms })
    got "RANDOM_OBJECTS" = do objs <- colon >> charLiteral `sepBy` comma
                              updateState (\l -> l { levelRandomObjs = objs })
    got tok = fail ("Unknown token " ++ tok)

    getTokens s = reverse $ go (s, [])
      where
        go (s, ts) | null s    = ts
                   | otherwise = let (t, s') = head (lex s)
                                 in if "#" `isPrefixOf` t then ts
                                                          else go (s', t:ts)


lexer = P.makeTokenParser $ emptyDef { P.commentLine = "#",
                                       P.caseSensitive = False }

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
squares = P.squares lexer
comma = P.comma lexer
colon = P.colon lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
decimal = P.decimal lexer
identifier = P.identifier lexer

tuple2 = parens $ do a <- decimal
                     b <- comma >> decimal
                     return (fromIntegral a, fromIntegral b)

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

instance Save (ObjPos, Obj) where
    save (p, o) = case o of
      Obj sym nm _     -> showString "OBJECT: " . save sym . comma' . save nm
                          . comma' . save p . nl' -- TODO
      Monst sym nm bh  -> showString "MONSTER: " . save sym . comma' . save nm
                          . comma' . save p . comma' . commas save bh
      Trap typ         -> showString "TRAP: " . save typ . q
      Fountain         -> showString "FOUNTAIN: " . save p . nl'
      Stair dir        -> showString "STAIR: " . save dir . q
      Engraving ink s  -> ("ENGRAVING: " ++) . save ink . comma' . save s . q
      Door typ         -> showString "DOOR: " . save typ . q
      Drawbridge dir t -> ("DRAWBRIDGE: " ++) . save dir . comma' . save t . q
     where q = comma' . save p . nl'

instance Save ObjPos where
    save (ObjPos p)         = save p
    save RandomPos          = showString "random"
    save (RandomPosIndex i) = showString "places[" . shows i . (']':)
    save Contained          = showString "contained"

instance Save ObjSym where
    save (ObjChar c)        = save c
    save RandomObj          = showString "random"
    save (RandomObjIndex i) = showString "objects[" . shows i . (']':)

instance Save MonstSym where
    save (MonstChar c) = save c
    save RandomMonst  = showString "random"
    save (RandomMonstIndex i) = showString "monsters[" . shows i . (']':)

showsLower :: Show a => a -> ShowS
showsLower = showString . map toLower . show

instance Save Blessing where save = showsLower
instance Save Behaviour where save = showsLower
instance Save TrapType where save = showsLower
instance Save StairDir where save = showsLower
instance Save Ink where save = showsLower
instance Save Dir where save = showsLower
instance Save DoorType where save = showsLower

-- TODO: Not sure if the escaping convention is the same...
instance Save Pos where save = shows
instance Save Rect where save = shows
instance Save String where save = shows
instance Save Char where save = shows

instance Save a => Save (Rand a) where
    save Random    = showString "random"
    save (Fixed x) = save x

instance Save LevelFlag where
    save NoTeleport = showString "noteleport"
    save HardFloor  = showString "hardfloor"

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
