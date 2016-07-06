import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Foreign.Marshal.Array
import Control.Monad.IO.Class (liftIO)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import Data.List

import Util.Vec3
import Core.Phys

--data Obj = Obj (GLfloat,GLfloat,GLfloat) (GLfloat,GLfloat,GLfloat)
--  deriving (Show)

unit :: GLfloat
unit = 0.01

csz :: GLfloat
csz = 0.1

side :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) 
     -> (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> IO ()
side br bl tl tr = do
  let ((xbr,ybr,zbr),(xbl,ybl,zbl),(xtl,ytl,ztl),(xtr,ytr,ztr)) = (br,bl,tl,tr)
  glVertex3f xbr ybr zbr
  glVertex3f xbl ybl zbl
  glVertex3f xtl ytl ztl
  glVertex3f xtr ytr ztr

circle :: (GLfloat,GLfloat,GLfloat) -> GLfloat -> IO ()
circle (x,y,z) r = do
  let pts = map ((* (2*pi)) . (* (1/20))) [0,1..20]
  glVertex3f x z 0
  mapM_ (\t -> glVertex3f ((cos t)*r) ((sin t)*r) 0) pts

degRad :: GLfloat -> GLfloat
degRad = (*) pi . flip (/) 180

initGL win = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  (w,h) <- K.getFramebufferSize win
  resizeScene win w h

resizeScene win w 0 = resizeScene win w 1
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  glOrtho (-1) 1 (-1) 1 (-1) 1
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene (Obj (x,y,z) _ _) _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glBegin gl_QUADS
  glColor3f 1.0 0.0 0.0
  side (-x+1,-z+1,0) (-x-1,-z+1,0) (-x-1,-z-1,0) (-x+1,-z-1,0)
  glEnd
  glBegin gl_TRIANGLE_FAN
  glColor3f 0 1 0
  circle (0,0,0) 0.1
  glEnd

shutdown win = do
  K.destroyWindow win
  K.terminate
  _ <- exitWith ExitSuccess
  return ()

isPressed :: K.KeyState -> Bool
isPressed K.KeyState'Pressed = True
isPressed K.KeyState'Repeating = True
isPressed _ = False

toFLT :: IO Bool -> IO GLfloat
toFLT a = do
  x <- a
  return $ if x then 1 else 0

getInput :: K.Window -> IO (GLfloat, GLfloat, GLfloat, GLfloat)
getInput win = do
  x <- mapM (toFLT . fmap isPressed . K.getKey win) [K.Key'Left,K.Key'Right,K.Key'Down,K.Key'Up,
                                                     K.Key'A,K.Key'D,K.Key'S,K.Key'W]
  let (x0:x1:x2:x3:x4:x5:x6:x7:_) = x
  return (x1-x0,x3-x2,x5-x4,x7-x6)

parseInput :: Obj -> K.Window -> IO (Obj)
parseInput (Obj (xt,yt,zt) v f) win = do
  (x,z,_,_) <- getInput win
  return $ Obj (x*unit+xt,0,z*unit+zt) v f

runGame player win = do
  K.pollEvents
  player' <- parseInput player win
  drawScene player' win
  K.swapBuffers win
  runGame player' win

main = do
  True <- K.init
  Just win <- K.createWindow 800 800 "Lietuva" Nothing Nothing
  let player = Obj (0,0,0) (0,0,0) []
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene player))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame player win
