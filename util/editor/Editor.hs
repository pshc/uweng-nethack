{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Control.Monad.State
import Control.Exception (bracket_)
import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Level
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data Editor = Editor { edRunning :: Bool, edCursor :: Pos,
                       edFilename :: String, edSaved :: Bool,
                       edHistory, edFuture :: [(Level, Pos)] }

initEditor = Editor { edRunning  = True,
                      edCursor   = (0, 0),
                      edFilename = "", edSaved  = True,
                      edHistory  = [], edFuture = [] }

type LevelIO = StateT Level IO
type EditorIO = StateT Editor LevelIO

-- View
inDisplay :: IO a -> IO a
inDisplay f = bracket_ (start >> startColor) end f

renderLevel :: Level -> IO ()
renderLevel mp = let ts = levelTiles mp
                     os = levelObjs mp
                 in go ts os (snd $ bounds ts)
  where
    go ts os (w, h) = renderRow 0
      where
        renderRow y | y < h     = do mvWAddStr stdScr y 0 (map tile [0..w])
                                     renderRow (y+1)
                    | otherwise = return ()
          where
            tile x = maybe (ts ! (x, y)) (objChar . head)
                           (Fixed (x, y) `Map.lookup` os)

redraw :: EditorIO ()
redraw = do mp <- lift get
            liftIO $ renderLevel mp
            drawStatus

-- Control
movementKeys = zip "hjkl" [(-1, 0), (0, 1), (0, -1), (1, 0)]
            ++ zip "yubn" [(-1, -1), (1, -1), (-1, 1), (1, 1)]
            ++ [(toUpper c, (x*8, y*8)) | (c, (x, y)) <- take 8 movementKeys]

tileKeys = " .-|+<>#\\S"

runEditor :: EditorIO ()
runEditor = do drawStatus
               editLevel

editLevel :: EditorIO ()
editLevel = do k <- liftIO $ refresh >> getKey refresh
               case k of KeyChar c   -> cmd c
                         _           -> return ()
               running <- gets edRunning
               if running then editLevel else return ()
  where
    cmd :: Char -> EditorIO ()
    cmd 'Q' = do ed <- get
                 quit <- if edSaved ed then return True
                         else fmap (== 'y') $ liftIO $ promptChar $
                              "You have not saved! "
                              ++ "Are you sure you want to quit?"
                 if quit then put $ ed { edRunning = False }
                         else drawStatus
    cmd 'S' = do ed <- get
                 fnm <- liftIO $ prompt "Save as: " (edFilename ed)
                 unless (null fnm) $ do
                     let fnm' = if '.' `elem` fnm then fnm else fnm ++ ".des"
                     lev <- lift get
                     b <- liftIO (saveLevels fnm' lev)
                     if b then put (ed { edFilename = fnm', edSaved = True })
                          else do liftIO (promptChar "Unable to save!")
                                  return ()
                 drawStatus
    cmd 'L' = do fnm <- liftIO $ prompt "Load file: " ""
                 if null fnm then drawStatus else do
                   let fnm' = if '.' `elem` fnm then fnm else fnm ++ ".des"
                   l <- liftIO $ loadLevels fnm'
                   case l of Left msg  -> do liftIO (promptChar msg)
                                             drawStatus
                             Right lev -> do liftIO (promptChar "Loaded!")
                                             doLoad fnm' lev
    cmd 'z' = do ed <- get
                 let (hist, futr) = (edHistory ed, edFuture ed)
                 unless (null hist) $ do
                     nowLevel <- lift get
                     let now           = (nowLevel, edCursor ed)
                         (old, oldPos) = head hist
                     lift $ put old
                     put $ ed { edHistory = tail hist, edFuture = now:futr,
                                edSaved = False, edCursor = oldPos }
                     redraw
    cmd 'x' = do ed <- get
                 let (hist, futr) = (edHistory ed, edFuture ed)
                 unless (null futr) $ do
                     nowLevel <- lift get
                     let now           = (nowLevel, edCursor ed)
                         (new, newPos) = head futr
                     lift $ put new
                     put $ ed { edFuture = tail futr, edHistory = now:hist,
                                edSaved = False, edCursor = newPos }
                     redraw
    cmd c | c `elem` tileKeys = do modifying
                                   gets edCursor >>= lift . flip setTile c
                                   moveCursor (1, 0)
          | otherwise         = case lookup c movementKeys of
                                    Just dir -> moveCursor dir
                                    Nothing  -> return ()

    doLoad fnm lev = do lift (put lev)
                        ed <- get
                        put (ed { edFilename = fnm, edSaved = True,
                                  edHistory = [], edFuture = [] })
                        redraw

moveCursor :: Pos -> EditorIO ()
moveCursor (dx, dy) = do ed <- get
                         (w, h) <- lift $ gets levelSize
                         let (x, y)   = edCursor ed
                             (x', y') = (clamp 0 w (x+dx), clamp 0 h (y+dy))
                         put $ ed { edCursor = (x', y') }
                         liftIO $ move y' x'

setTile :: Pos -> Char -> LevelIO ()
setTile (x, y) c = do mp <- get
                      put $ mp { levelTiles = (levelTiles mp) // [((x,y), c)] }
                      lift $ mvAddCh y x (toChType c)

toChType = fromIntegral . ord

-- Should be called *before* any modification
modifying :: EditorIO ()
modifying = do ed <- get
               lev <- lift get
               let now = (lev, edCursor ed)
               put $ ed { edHistory = take 20 (now : edHistory ed),
                          edSaved = False, edFuture = [] }

promptChar :: String -> IO Char
promptChar msg = do (y, x) <- getYX stdScr
                    statusLine1 (msg ++ repeat ' ')
                    drawCursor (22, length msg) (0, 0)
                    refresh
                    go
  where
    go = do k <- getKey refresh
            case k of KeyChar c -> return c
                      otherwise -> go

prompt :: String -> String -> IO String
prompt q initial = do statusLine1 (q ++ initial ++ repeat ' ')
                      nl True
                      b <- getChars (length q) (reverse initial)
                      nl False
                      return b
  where
    getChars nq s = do drawCursor (22, nq) (0, length s)
                       refresh
                       getKey refresh >>= gotKey
      where
        gotKey KeyEnter                      = return (reverse s)
        gotKey KeyBackspace | null s         = next ""
                            | otherwise      = do let x = nq + length s - 1
                                                  mvWAddStr stdScr 22 x " "
                                                  next (tail s)
        gotKey (KeyChar c) | c `elem` "\r\n" = gotKey KeyEnter
                           | c == '\DEL'     = gotKey KeyBackspace
                           | c >= ' ' &&
                             c <= '~'        = do waddch stdScr (toChType c)
                                                  next (c:s)
        gotKey otherwise                     = next s

        next = getChars nq

drawStatus :: EditorIO ()
drawStatus = do nm <- lift $ gets levelName
                fnm <- ((" (" ++) . (++ ")")) `fmap` gets edFilename
                (x, y) <- gets edCursor
                liftIO $ do statusLine1 $ "Level \"" ++ nm ++ "\""
                                          ++ fnm ++ repeat '-'
                            wMove stdScr y x

statusLine1 = mvWAddStr stdScr 22 0 . take 80

clamp lower upperPlusOne = max lower . min (upperPlusOne - 1)

main = inDisplay $ execStateT (execStateT runEditor initEditor) initLevel

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
