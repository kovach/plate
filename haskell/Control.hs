module Control where

-- FFI
import Foreign.C
import qualified Data.Vector.Storable as SV

-- WAV support
import Data.WAVE

-- opengl
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- io
import Data.IORef
import System.IO.Unsafe


-- stuff
import Control.Monad


un = undefined

--dvFromList :: SV.Storable a => [a] -> SV.Vector a
--dvFromList list = SV.generate (length list) (list !!)
--type BufferId = Int

{- FFI declarations -}
-- frequency control
foreign import ccall "set_freq" c_set_freq :: CDouble -> IO ()
setFreq :: Double -> IO ()
setFreq f = c_set_freq (realToFrac f)
foreign import ccall "get_freq" c_get_freq :: IO CDouble
getFreq :: IO Double
getFreq = do c_get_freq >>= (return . realToFrac)
-- jack init/close
foreign import ccall "jack_init" c_jack_init :: IO ()
foreign import ccall "jack_close" c_jack_close :: IO ()
foreign import ccall "print_test" c_print_test :: IO ()

foreign import ccall "get_sr" c_get_sr :: IO CInt
foreign import ccall "new_buffer" c_new_buffer :: CInt -> IO CInt
newBuffer samples =
  c_new_buffer samples

terminate_program = do
  c_jack_close
  leaveMainLoop

{- Main -}
main = do 

  -- init monad
  --glutref <- newIORef $ GlutState (Size 0 0) (0, 0) M.empty
  --gameref <- newIORef $ GameState A (45, 66) (80, 90)

  -- initialize c stuff
  c_jack_init

  -- read wav file
  WAVE header samples <- getWAVEFile "mouthbreather.wav"


  -- window
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "min"
  displayCallback $= display 
  reshapeCallback $= Just (reshape)
  keyboardMouseCallback $= Just (keymouse)
  motionCallback $= Just (mousemove)
  mainLoop


{- GLUT functions -}
display = do
  clear [ColorBuffer]
  loadIdentity
  swapBuffers

redraw = postRedisplay Nothing

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

keymouse key state modifiers position =
  if state == Up then return () else
  let base = 100 in
  case key of
    Char 'q' -> terminate_program
    Char 'a' -> setFreq base 
    Char 'b' -> setFreq (base*2)
    Char 'c' -> setFreq (base*3)
    Char 'p' -> getFreq >>= print
    Char 'n' -> c_get_sr >>= newBuffer >>= print
    other -> return ()

--keymouse key state modifiers (Position x' y') = do
--  gl' <- readIORef gl
--  gr' <- readIORef gr
--  let (Size w h) = winsize gl'
--  let (x,y) = normalize' x' y' w h
--  changeMouse gl (x, y)
--  changeKey gl key state
--  runEvents gl' gr' >>= writeIORef (gr :: IORef GameState)
  

mousemove position = redraw
--mousemove (Position x' y') = do
--  gl' <- readIORef gl
--  gr' <- readIORef gr
--  let (Size w h) = winsize gl'
--  let (x,y) = normalize' x' y' w h
--  changeMouse gl (x, y)
--  (runEvents gl' gr') >>= writeIORef gr
--  redraw
