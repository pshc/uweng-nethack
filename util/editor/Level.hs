{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
             NoMonomorphismRestriction, RelaxedPolyRec,
             NamedFieldPuns, RecordWildCards, OverlappingInstances #-}
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

data Level = Level { levelName :: String, levelFilling :: Rand Char,
                     levelTiles :: Array Pos Char,
                     prevLevels, nextLevels :: [Level],
                     levelGeometry :: (HJustif, VJustif),
                     levelObjs :: Map (Rand Pos) [Obj],
                     levelRegions :: [(Region, Rect)],
                     levelSpeRegions :: [(LevRegion, LevRegion, SpeRegion)],
                     levelFlags :: [LevelFlag], levelRandomPlaces :: [Pos],
                     levelRandomObjs, levelRandomMons :: [Char] }

initLevel = Level { levelName = "untitled", levelFilling = Fixed ' ',
                    levelTiles = listArray ((0, 0), (79, 21)) (repeat ' '),
                    prevLevels = [], nextLevels = [],
                    levelGeometry = (HCenter, Center),
                    levelObjs = Map.empty,
                    levelRegions = [], levelSpeRegions = [],
                    levelFlags = [], levelRandomPlaces = [],
                    levelRandomObjs = [], levelRandomMons = [] }

data Rand a = Fixed a | Random | RandomIndex Int deriving (Eq, Ord)

data LevRegion = Rect Rect | LevRegion Rect

data LevelFlag = NoTeleport | HardFloor | NoMMap | Arboreal | ShortSighted
                 deriving (Bounded, Enum, Show)
data Region = Region (Rand Lighting) RegionType (Maybe (Fill, Maybe Bool))
              | NonDiggable | NonPassWall
data Lighting = Lit | Unlit deriving (Bounded, Enum, Show)
data Fill = Filled | Unfilled deriving (Bounded, Enum, Show)
data RegionType = Ordinary | Throne | Swamp | Vault | Beehive | Morgue
                  | Barracks | Zoo | Delphi | Temple | LeprechaunHall
                  | CockatriceNest | Anthole | ArmorShop | ScrollShop
                  | PotionShop | WeaponShop | FoodShop | RingShop | WandShop
                  | ToolShop | BookShop | CandleShop
                  deriving (Bounded, Enum, Show)
data SpeRegion = StairRegion UpDown | TeleportRegion (Maybe UpDown)
                 | PortalRegion String | BranchRegion
data HJustif = HLeft | HalfLeft | HCenter | HalfRight | HRight
               deriving (Bounded, Enum)
data VJustif = Top | Center | Bottom deriving (Bounded, Enum, Show)

instance Show HJustif where
    show HLeft = "left"; show HalfLeft = "half-left"; show HCenter = "center"
    show HalfRight = "right"; show HRight = "right"

data Obj = Obj (Maybe Chance) (Rand Char) (Rand String)
               (Maybe (Maybe (Rand Blessing), Maybe String, Spe, Maybe String))
           | Container (Maybe Chance) (Rand Char) (Rand String) [Obj]
           | Mon (Maybe Chance) (Rand Char) (Rand String)
                 (Maybe String, Maybe Attitude, Maybe Alertness,
                  Maybe (Rand Align), Maybe (Appearance, String))
           | Trap (Maybe Chance) (Rand TrapType)
           | Engraving (Rand Ink) String
           | Door (Rand DoorType) | Drawbridge Dir (Rand DoorType)
           | Altar (Rand Align) (Rand AltarType)
           | Gold (Rand Int)
           | Stair UpDown | Ladder UpDown
           | Sink | Pool | Fountain -- Must be last

instance Enum Obj where
    fromEnum (Obj _ _ _ _)       = 0
    fromEnum (Container _ _ _ _) = 1
    fromEnum (Mon _ _ _ _)       = 2
    fromEnum (Trap _ _)          = 3
    fromEnum (Engraving _ _)     = 4
    fromEnum (Door _)            = 5
    fromEnum (Drawbridge _ _)    = 6
    fromEnum (Altar _ _)         = 7
    fromEnum (Gold _)            = 8
    fromEnum (Stair _)           = 9
    fromEnum (Ladder _)          = 10
    fromEnum Sink                = 11
    fromEnum Pool                = 12
    fromEnum Fountain            = 13
    toEnum 13 = Fountain
    toEnum _ = error "No Obj toEnum!"

instance Bounded Obj where
    minBound = error "No Obj minBound!"
    maxBound = Fountain

objChar (Obj _ c _ _)       = maybeRand 'R' id c
objChar (Container _ c _ _) = maybeRand 'C' id c
objChar (Mon _ c _ _)       = maybeRand 'M' id c
objChar (Stair ud)          = dirChar ud
objChar (Ladder ud)         = dirChar ud
objChar o                   = "   ^~++_$  #}{" !! fromEnum o

dirChar Up   = '<'
dirChar Down = '>'

maybeRand _   f (Fixed x)       = f x
maybeRand def _ Random          = def
maybeRand _   _ (RandomIndex _) = undefined

type Spe = Int
type Chance = Int
data Blessing = Blessed | Uncursed | Cursed deriving (Bounded, Enum, Show)
data Attitude = Peaceful | Hostile deriving (Bounded, Enum, Show)
data Alertness = Awake | Asleep deriving (Bounded, Enum, Show)
data Appearance = M_Feature | M_Monster | M_Object
                  deriving (Bounded, Enum, Show)
data TrapType = ArrowTrap | DartTrap | FallingRockTrap | SqueakyBoard
                | BearTrap | LandMine | RollingBoulderTrap | SleepingGasTrap
                | RustTrap | FireTrap | Pit | SpikedPit | Hole | TrapDoor
                | TeleportationTrap | LevelTeleporter | MagicPortal | Web
                | StatueTrap | MagicTrap | Anti_MagicField | PolymorphTrap
                deriving (Bounded, Enum, Show)
data UpDown = Up | Down deriving (Bounded, Enum, Show)
data Ink = Dust | Engrave | Burn | Mark deriving (Bounded, Enum, Show)
data Dir = North | East | South | West deriving (Bounded, Enum, Show)
data DoorType = Open | Closed | Locked | NoDoor | Broken
                deriving (Bounded, Enum, Show)
data Align = NoAlign | Law | Neutral | Chaos | Coaligned | NonCoaligned
             deriving (Bounded, Enum, Show)
data AltarType = Sanctum | Shrine | ALTAR deriving (Bounded, Enum, Show)

levelSize (Level {..}) = let (_, (x, y)) = bounds levelTiles
                         in (x + 1, y + 1)

class Save a where
    save :: a -> ShowS

commas f (x:xs) = foldl (\ss y -> ss . comma' . f y) (f x) xs
commas _ []     = id

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
type ContainerIndex = (Rand Pos, Int)

putLevel f = updateState $ \(LoadState lev c) -> LoadState (f lev) c

parseEnum :: (Bounded a, Enum a, Show a) => LevelParser a
parseEnum = choice [reserved (show c) >> return c | c <- [minBound..maxBound]]

quotedEnum :: (Bounded a, Enum a, Show a) => LevelParser a
quotedEnum = do e <- stringLiteral
                maybe (error $ "Unexpected " ++ show e) return
                      (find (equals e) [minBound .. maxBound])
  where
    a `equals` b = let (c:cs) = show b in head a == toLower c && go (tail a) cs

    go (' ':a:as) (b:bs) | isUpper b = a == toLower b && go as bs
    go (a:as) (b:bs)     | isLower b = a == b && go as bs
    go ('-':as) ('_':bs)             = go as bs
    go [] []                         = True
    go _  _                          = False

loadLevels :: FilePath -> IO (Either String Level)
loadLevels fp = catch (runParser parseLevels NoState fp `fmap` readFile fp
                        >>= return . either (fail . show) return)
                      (return . fail . show)
  where
    parseLevels = do (m:nextLevels) <- whiteSpace >> many readMaze
                     eof
                     return $ m { nextLevels }
    readMaze = do levelName <- reserved "MAZE" >> colon>> commaed stringLiteral
                  levelFilling <- rand charLiteral
                  whiteSpace
                  setState $ LoadState (initLevel { levelName,
                                                    levelFilling }) Nothing
                  many1 $ choice lineParsers
                  LoadState lev _ <- getState
                  return lev

    lineParsers = readMap : [reserved r >> sep >> f >> whiteSpace
                             | (r, f) <- reservedParsers,
                               let sep = if r `elem` chances then return ()
                                         else colon >> return ()]

    chances = ["MONSTER", "OBJECT", "CONTAINER", "TRAP"]

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
    ("FLAGS", do levelFlags <- parseEnum `sepBy` comma
                 putLevel (\l -> l { levelFlags })),
    ("RANDOM_PLACES",   do levelRandomPlaces <- tuple2 `sepBy` comma
                           putLevel (\l -> l { levelRandomPlaces })),
    ("RANDOM_MONSTERS", do levelRandomMons <- charLiteral `sepBy` comma
                           putLevel (\l -> l { levelRandomMons })),
    ("RANDOM_OBJECTS",  do levelRandomObjs <- charLiteral `sepBy` comma
                           putLevel (\l -> l { levelRandomObjs })),
    ("TELEPORT_REGION", speRegion $ TeleportRegion `fmap`
                                    optionMaybe (comma >> parseEnum)),
    ("PORTAL", speRegion $ comma >> PortalRegion `fmap` stringLiteral),
    ("BRANCH", speRegion $ return BranchRegion),
    ("STAIR",  speRegion (comma >> StairRegion `fmap` parseEnum)
               <|> do addObj <- commaed objPos
                      parseEnum >>= addObj . Stair),
    ("LADDER", do addObj <- commaed objPos
                  parseEnum >>= addObj . Ladder),
    ("SINK",       withPos Sink),
    ("POOL",       withPos Pool),
    ("FOUNTAIN",   withPos Fountain),
    ("ALTAR",      do addObj <- commaed objPos
                      align <- commaed $ randIndex "align" parseEnum
                      typ <- rand parseEnum
                      addObj (Altar align typ)),
    ("GOLD",       commaed (rand decimal) >>= withPos . Gold),
    ("DOOR",       commaed (rand parseEnum) >>= withPos . Door),
    ("DRAWBRIDGE", do addObj <- commaed objPos
                      dir <- commaed parseEnum
                      st <- rand parseEnum
                      addObj (Drawbridge dir st)),
    ("TRAP",       do ch <- chance
                      commaed (rand quotedEnum) >>= withPos . Trap ch),
    ("OBJECT",     do ch <- chance
                      typ <- commaed (randIndex "object" charLiteral)
                      nm <- commaed (rand stringLiteral)
                      addObj <- objPos
                      misc <- optionMaybe $ comma >> do
                          curs <- optionMaybe (commaed (rand parseEnum))
                          mon <- optionMaybe (commaed stringLiteral)
                          spe <- decimal
                          nm <- (Just `fmap` try (comma >> stringLiteral)) <|>
                                (comma >> reserved "none" >> return Nothing)
                                <|> return Nothing
                          return (curs, mon, spe, nm)
                      addObj $ Obj ch typ nm misc),
    ("MONSTER",    do ch <- chance
                      sym <- commaed (randIndex "monster" charLiteral)
                      nm <- commaed (rand stringLiteral)
                      addObj <- objPos
                      extras <- monExtras (Nothing, Nothing, Nothing, Nothing,
                                           Nothing)
                      addObj (Mon ch sym nm extras)),
    ("CONTAINER",  do ch <- chance
                      typ <- commaed (randIndex "object" charLiteral)
                      nm <- commaed (rand stringLiteral)
                      withPos $ Container ch typ nm []),
    ("ENGRAVING",  do addObj <- commaed objPos
                      typ <- commaed (rand parseEnum)
                      text <- stringLiteral
                      addObj (Engraving typ text))]
  where
    speRegion f = do r1 <- commaed levRegion; r2 <- levRegion
                     spe <- f
                     putLevel (\l -> l { levelSpeRegions = (r1, r2, spe)
                                         : levelSpeRegions l })

    withPos obj = objPos >>= ($ obj)

    monExtras x@(a, b, c, d, e) = (comma >> choice [
        stringLiteral               >>= \a -> monExtras (Just a, b, c, d, e),
        parseEnum                   >>= \b -> monExtras (a, Just b, c, d, e),
        parseEnum                   >>= \c -> monExtras (a, b, Just c, d, e),
        randIndex "align" parseEnum >>= \d -> monExtras (a, b, c, Just d, e),
        do e1 <- parseEnum; e2 <- stringLiteral
           monExtras (a, b, c, d, Just (e1, e2))
        ]) <|> return x

    chance = do ch <- optionMaybe $ squares $ do d <- decimal; char '%'
                                                 return d
                colon
                return ch

reservedNames = "MAZE" : "MAP" : "ENDMAP" : "random" : "none" : "contained"
                : "place" : "object" : "monster" : "align"
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
                             c'   = case o of Container _ _ _ _ -> Just (p, n)
                                              _                 -> c
                             os'  = Map.insert p (o:dest) (levelObjs l)
                         setState $ LoadState (l { levelObjs = os' }) c'

    contInsert o = do LoadState l (Just (p, n)) <- getState
                      let os' = Map.adjust (insertAt n o) p (levelObjs l)
                      putLevel (\l -> l { levelObjs = os' })

    insertAt n o ss = let (bef, (Container c a b os):aft) = splitAt n ss
                      in bef ++ Container c a b (o:os) : aft

tuple4 = parens $ do [a, b, c] <- count 3 (commaed decimal); d <- decimal
                     return (a, b, c, d)

levRegion = (reserved "levregion" >> tuple4 >>= return . LevRegion)
            <|> (tuple4 >>= return . Rect)

rand c = (reserved "random" >> return Random) <|> (c >>= return . Fixed)
randIndex s c = (reserved s >> squares decimal >>= return . RandomIndex)
                <|> rand c

instance Save Level where
    save lv@(Level {..}) =
              showString "MAZE: " . save levelName . comma' . save levelFilling
              . (if null levelFlags then id else
                 showString "\nFLAGS: " . commas save levelFlags)
              . showString "\nGEOMETRY: " . save levelGeometry . nl'
              . ("MAP\n" ++) . (showTiles (levelSize lv) levelTiles ++)
              . showString "ENDMAP\n"
              . permute "RANDOM_PLACES: " levelRandomPlaces
              . permute "RANDOM_MONSTERS: " levelRandomMons
              . permute "RANDOM_OBJECTS: " levelRandomObjs
              -- TODO: Regions
              . foldl (\ss r -> ss . save r) id levelSpeRegions
              . foldl (\ss o -> ss . save o) (showString "# Objects\n")
                      (concat sortedObjs)
      where
        -- Unfortunately `elems` is top-to-bottom, left-to-right
        showTiles (w, h) ts = go (0, 0)
          where
            go (x, y) | x < w && y < h = ts ! (x, y) : go (x+1, y)
                      | y < h          = '\n' : go (0, y+1)
                      | otherwise      = []

        permute nm x | null x    = id
                     | otherwise = showString nm . commas save x . nl'

        sortedObjs = let os  = concat [map ((,) p) ol
                                       | (p, ol) <- Map.assocs levelObjs]
                         acc = replicate (fromEnum (maxBound :: Obj) + 1) []
                     in foldr sortObj acc os
        sortObj o acc = let (bef, (os:aft)) = splitAt (fromEnum (snd o)) acc
                        in bef ++ [o:os] ++ aft

instance Save (HJustif, VJustif) where
    save (h, v) = shows h . comma' . showsLower v

instance Save (LevRegion, LevRegion, SpeRegion) where
    save (r1, r2, spe) = let (r, f) = saveSpe spe
                         in r . save r1 . comma' . save r2 . f . nl'
      where
        saveSpe spe = case spe of
          StairRegion dir    -> (showString "STAIR: ", comma' . save dir)
          TeleportRegion dir -> (showString "TELEPORT_REGION: ",
                                 maybe id ((comma' .) . save) dir)
          PortalRegion s     -> (showString "PORTAL: ", comma' . save s)
          BranchRegion       -> (showString "BRANCH: ", id)

instance Save LevRegion where
    save (LevRegion reg) = showString "levregion" . save reg
    save (Rect reg)      = save reg

instance Save (Rand Pos, Obj) where save (p, o) = saveObj (save p) o

saveObj p o = case o of
    Obj ch sym nm misc  -> showString "OBJECT" . chance ch
                           . saveRandom shows "object" sym . comma' . save nm
                           . comma' . p . maybe id objMisc misc . nl'
    Container ch c s os -> showString "CONTAINER" . chance ch
                           . saveRandom shows "object" c . comma' . save s . q
                           . foldl (\s o -> s . saveObj ("contained" ++) o)
                                   id os
    Mon ch c s misc     -> showString "MONSTER" . chance ch
                           . saveRandom shows "monster" c . comma' . save s
                           . comma' . p . monMisc misc
    Trap ch typ         -> showString "TRAP" . chance ch . save typ . q
    Stair dir           -> showString "STAIR: " . save dir . q
    Ladder dir          -> showString "LADDER: " . save dir . q
    Sink                -> showString "SINK: " . p . nl'
    Pool                -> showString "POOL: " . p . nl'
    Fountain            -> showString "FOUNTAIN: " . p . nl'
    Altar a t           -> showString "ALTAR: " . save a . comma' . save t . q
    Gold n              -> showString "GOLD: " . save n . q
    Engraving ink s     -> ("ENGRAVING: " ++) . save ink . comma' . save s . q
    Door typ            -> showString "DOOR: " . save typ . q
    Drawbridge dir t    -> ("DRAWBRIDGE: " ++) . save dir . comma' . save t . q
  where
    q = comma' . p . nl'
    chance = maybe (": " ++) $ \c -> ('[':) . shows c . ("%]: " ++)
    commaMaybe = maybe id ((comma' .) . save)
    objMisc (curs, mon, spe, nm) = commaMaybe curs . commaMaybe mon
                                   . comma' . save spe . commaMaybe nm
    monMisc (a, b, c, d, e) = let f = commaMaybe
                              in f a . f b . f c . f d . f e . nl'

showsLower :: Show a => a -> ShowS
showsLower = showString . map toLower . show

showsQuoted :: Show a => a -> ShowS
showsQuoted o = let (s:ss) = show o
                in (['"', toLower s] ++) . foldr quote ('"':) ss
  where
    quote c | c == '"'  = (("\\\"" ++) .)
            | isUpper c = (([' ', toLower c] ++) .)
            | otherwise = ((c :) .)

instance Save (Rand Pos) where save = saveRandom shows "place"
instance Save (Rand Align) where save = saveRandom showsLower "align"
instance Save (Rand TrapType) where save = saveRandom showsQuoted "trap"

instance Save Blessing where save = showsLower
instance Save Attitude where save = showsLower
instance Save Alertness where save = showsLower
instance Save RegionType where save = showsQuoted
instance Save UpDown where save = showsLower
instance Save Ink where save = showsLower
instance Save Dir where save = showsLower
instance Save DoorType where save = showsLower
instance Save LevelFlag where save = showsLower
instance Save AltarType where save = showsLower

instance Save (Appearance, String) where
    save (a, s) = showsLower a . (' ':) . save s

instance Save Pos where save = shows
instance Save Rect where save = shows
instance Save String where save = shows
instance Save Char where save = shows
instance Save Int where save = shows
instance Save Bool where save = showsLower

saveRandom _ _ Random          = showString "random"
saveRandom f _ (Fixed x)       = f x
saveRandom _ s (RandomIndex i) = showString s . ('[':) . shows i . (']':)

instance Save a => Save (Rand a) where
    save = maybeRand (showString "random") save

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
