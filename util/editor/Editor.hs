import Control.Monad.State
import Control.Exception
import Data.Array
import Data.Char
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper (start, end, getKey)

-- Model
type Rect = (Int, Int, Int, Int)
type Pos = (Int, Int)

data Map = Map { mapName :: String, mapTiles :: Array Pos Char }
type MapIO = StateT Map IO

mapSize :: Map -> (Int, Int)
mapSize (Map { mapTiles = ts }) = let (_, (x, y)) = bounds ts
                                  in (x + 1, y + 1)

initMap = Map { mapName = "test",
                mapTiles = listArray ((0, 0), (79, 21)) (repeat ' ') }

data Editor = Editor { edRunning :: Bool, edCursor :: Pos }
type EditorIO = StateT Editor MapIO

initEditor = Editor { edRunning = True,
                      edCursor  = (0, 0) }

-- View
inDisplay :: IO a -> IO a
inDisplay f = bracket (start >> startColor) (const end) (const f)

renderMap :: Map -> IO ()
renderMap mp = return ()

redraw :: EditorIO ()
redraw = do mp <- lift get
            lift2 $ renderMap mp

-- Control
movementKeys = zip "hjkl" [(-1, 0), (0, 1), (0, -1), (1, 0)]
            ++ zip "yubn" [(-1, -1), (1, -1), (-1, 1), (1, 1)]
            ++ [(toUpper c, (x*8, y*8)) | (c, (x, y)) <- take 8 movementKeys]

tileKeys = " .-|+<>#\\S"

-- There's gotta be a better way to do this:
snapshotEditorIO :: EditorIO a -> EditorIO (IO a)
snapshotEditorIO f = do snapEd <- get; snapMap <- lift get
                        return $ evalStateT (evalStateT f snapEd) snapMap

editMap :: EditorIO ()
editMap = do lift2 refresh
             k <- lift2 . getKey =<< snapshotEditorIO redraw
             case k of KeyChar 'Q' -> return ()
                       KeyChar c   -> cmd c >> editMap
                       _           -> editMap
  where
    cmd :: Char -> EditorIO ()
    cmd c | c `elem` tileKeys = do gets edCursor >>= lift . flip setTile c
                                   moveCursor (1, 0)
          | otherwise         = case lookup c movementKeys of
                                    Just dir -> moveCursor dir
                                    Nothing  -> return ()

moveCursor :: (Int, Int) -> EditorIO ()
moveCursor (dx, dy) = do ed <- get
                         (w, h) <- lift $ gets mapSize
                         let (x, y) = (clamp 0 w (fst (edCursor ed) + dx),
                                       clamp 0 h (snd (edCursor ed) + dy))
                         put $ ed { edCursor = (x, y) }
                         lift2 $ move y x

setTile :: (Int, Int) -> Char -> MapIO ()
setTile (x, y) c = do mp <- get
                      put $ mp { mapTiles = (mapTiles mp) // [((x, y), c)] }
                      lift $ mvAddCh y x (fromIntegral $ ord c)

clamp lower upperPlusOne = max lower . min (upperPlusOne - 1)
lift2 = lift . lift

main = inDisplay $ execStateT (execStateT editMap initEditor) initMap

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
