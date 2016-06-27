import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import Data.List

data Player = Player (GLfloat,GLfloat,GLfloat) (GLfloat,GLfloat,GLfloat)
  (GLfloat,GLfloat) Int deriving (Show)
data Camera = Camera (GLfloat,GLfloat,GLfloat) deriving (Show)

unit :: GLfloat
unit = 0.01

initGL win = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- K.getFramebufferSize win
  resizeScene win w h

resizeScene win w 0 = resizeScene win w 1
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene (Player (_,_,_) (x,y,z) (_,_) _) (Camera (cx,cy,_)) _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity
  glTranslatef 0 0 (-1.5)
  glRotatef cx 0 1 0
  glRotatef cy 1 0 0
  glBegin gl_QUADS
  glColor3f 0.0 1.0 0.0
  glVertex3f x z (-y)
  glVertex3f (x+0.1) z (-y)
  glVertex3f (x+0.1) (z+0.1) (-y)
  glVertex3f x (z+0.1) (-y)
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

parseInput :: Player -> Camera -> K.Window -> IO (Player,Camera)
parseInput (Player (xa,ya,za) (xt,yt,zt) (_,_) _) (Camera (cxt,cyt,_)) win = do
  (x,z,cx,cy) <- getInput win
  --putStrLn $ "x: "++(show xt)
  --putStrLn $ "y: "++(show yt)
  let (nx,nz) = moveA (x,z) (xt,zt) (cx+cxt,cy+cyt)
  return (Player (xa,ya,za) (nx,0,nz) (0,0) 0,
    Camera (cx+cxt,cy+cyt,0))

moveA :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) ->
  (GLfloat,GLfloat)
moveA (x,z) (xt,zt) (cx,cy) = (x*unit+xt,z*unit+zt)

runGame player cam win = do
  K.pollEvents
  (player',cam') <- parseInput player cam win
  drawScene player' cam' win
  K.swapBuffers win
  runGame player' cam' win

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "Lietuva" Nothing Nothing
  let player = Player (0,0,0) (0,0,0) (0,0) 0
      cam = Camera (0,0,0)
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene player cam))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame player cam win
