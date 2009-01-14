{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Control.Monad.State
import Control.Exception (bracket_)
import Data.Array
import Data.Char
import Level
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data Editor = Editor { edRunning :: Bool, edCursor :: Pos,
                       edFilename :: String }

initEditor = Editor { edRunning  = True,
                      edCursor   = (0, 0),
                      edFilename = "" }

type MapIO = StateT Map IO
type EditorIO = StateT Editor MapIO

-- View
inDisplay :: IO a -> IO a
inDisplay f = bracket_ (start >> startColor) end f

renderMap :: Map -> IO ()
renderMap mp = return ()

redraw :: EditorIO ()
redraw = do mp <- lift get
            liftIO $ renderMap mp

-- Control
movementKeys = zip "hjkl" [(-1, 0), (0, 1), (0, -1), (1, 0)]
            ++ zip "yubn" [(-1, -1), (1, -1), (-1, 1), (1, 1)]
            ++ [(toUpper c, (x*8, y*8)) | (c, (x, y)) <- take 8 movementKeys]

tileKeys = " .-|+<>#\\S"

-- There's gotta be a better way to do this:
snapshotEditorIO :: EditorIO a -> EditorIO (IO a)
snapshotEditorIO f = do snapEd <- get; snapMap <- lift get
                        return $ evalStateT (evalStateT f snapEd) snapMap

runEditor :: EditorIO ()
runEditor = do drawStatus
               editMap

editMap :: EditorIO ()
editMap = do liftIO refresh
             k <- liftIO . getKey =<< snapshotEditorIO redraw
             case k of KeyChar c   -> cmd c
                       _           -> return ()
             running <- gets edRunning
             if running then editMap else return ()
  where
    cmd :: Char -> EditorIO ()
    cmd 'Q' = do ed <- get
                 put $ ed { edRunning = False }
    cmd 'S' = do ed <- get
                 fnm <- liftIO $ prompt "Filename: " (edFilename ed)
                 b <- if null fnm then return False else lift $ saveLevel fnm
                 when b $ put (ed { edFilename = fnm })
                 drawStatus
    cmd c | c `elem` tileKeys = do gets edCursor >>= lift . flip setTile c
                                   moveCursor (1, 0)
          | otherwise         = case lookup c movementKeys of
                                    Just dir -> moveCursor dir
                                    Nothing  -> return ()

moveCursor :: (Int, Int) -> EditorIO ()
moveCursor (dx, dy) = do ed <- get
                         (w, h) <- lift $ gets mapSize
                         let (x, y)   = edCursor ed
                             (x', y') = (clamp 0 w (x+dx), clamp 0 h (y+dy))
                         put $ ed { edCursor = (x', y') }
                         liftIO $ move y' x'

setTile :: (Int, Int) -> Char -> MapIO ()
setTile (x, y) c = do mp <- get
                      put $ mp { mapTiles = (mapTiles mp) // [((x, y), c)] }
                      lift $ mvAddCh y x (toChType c)

toChType = fromIntegral . ord

saveLevel :: String -> MapIO Bool
saveLevel fnm = do lev <- get
                   liftIO $ catch (writeFile fnm (save lev "") >> return True)
                                  (const $ return False)

prompt :: String -> String -> IO String
prompt q initial = do (y, x) <- getYX stdScr
                      statusLine1 (q ++ initial ++ repeat ' ')
                      nl True
                      b <- promptChar (length q) (reverse initial)
                      nl False
                      wMove stdScr y x
                      return b

promptChar nq s = do drawCursor (22, nq) (0, length s)
                     refresh
                     getKey refresh >>= gotKey
  where
    gotKey KeyEnter                 = return (reverse s)
    gotKey KeyBackspace | null s    = next ""
                        | otherwise = do let x = nq + length s - 1
                                         mvWAddStr stdScr 22 x " "
                                         next (tail s)
    gotKey (KeyChar c) | c `elem` "\r\n"      = gotKey KeyEnter
                       | c == '\DEL'          = gotKey KeyBackspace
                       | c >= ' ' && c <= '~' = do waddch stdScr (toChType c)
                                                   next (c:s)
    gotKey otherwise                = next s

    next = promptChar nq

drawStatus :: EditorIO ()
drawStatus = do nm <- lift $ gets mapName
                fnm <- ((" (" ++) . (++ ")")) `fmap` gets edFilename
                liftIO $ do (y, x) <- getYX stdScr
                            statusLine1 $ "Level \"" ++ nm ++ "\""
                                          ++ fnm ++ repeat '-'
                            wMove stdScr y x

statusLine1 = mvWAddStr stdScr 22 0 . take 80

clamp lower upperPlusOne = max lower . min (upperPlusOne - 1)

main = inDisplay $ execStateT (execStateT runEditor initEditor) initMap

-- vi: set sw=4 ts=4 sts=4 tw=79 ai et nocindent:
