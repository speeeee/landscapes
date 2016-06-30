import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Foreign.Marshal.Array
import Control.Monad.IO.Class (liftIO)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import Data.List

data Player = Player (GLfloat,GLfloat,GLfloat) (GLfloat,GLfloat,GLfloat)
  (GLfloat,GLfloat) Int deriving (Show)
data Camera = Camera (GLfloat,GLfloat,GLfloat) deriving (Show)

unit :: GLfloat
unit = 0.01

norm3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
norm3 (a,b,c) = (a/l',b/l',c/l') where len = sqrt(a*a+b*b+c*c)
                                       l' = if len == 0 then 1 else len
cross3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
       -> (GLfloat,GLfloat,GLfloat)
cross3 (ax,ay,az) (bx,by,bz) = (ay*bz-az*by,bx*az-bz*ax,ax*by-ay*bx)
sub3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
     -> (GLfloat,GLfloat,GLfloat)
sub3 (a,b,c) (d,e,f) = (a-d,b-e,c-f)

side :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) 
     -> (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> IO ()
side br bl tl tr = do
  let (nx,ny,nz) = norm3 $  (br `sub3` tl) `cross3` (bl `sub3` tr)
      ((xbr,ybr,zbr),(xbl,ybl,zbl),(xtl,ytl,ztl),(xtr,ytr,ztr)) = (br,bl,tl,tr)
  glNormal3f (-nx) (-ny) (-nz)
  glVertex3f xbr ybr zbr
  glVertex3f xbl ybl zbl
  glVertex3f xtl ytl ztl
  glVertex3f xtr ytr ztr

cube :: (GLfloat,GLfloat,GLfloat) -> GLfloat -> IO ()
cube (x,y,z) sz = do
  side (x+sz,y,z) (x,y,z) (x,y+sz,z) (x+sz,y+sz,z)             -- Front
  side (x,y,z) (x,y,z-sz) (x,y+sz,z-sz) (x,y+sz,z)             -- Left
  side (x+sz,y,z-sz) (x+sz,y,z) (x+sz,y+sz,z) (x+sz,y+sz,z-sz) -- Right
  side (x,y,z-sz) (x+sz,y,z-sz) (x+sz,y+sz,z-sz) (x,y+sz,z-sz) -- Back
  side (x+sz,y+sz,z) (x,y+sz,z) (x,y+sz,z-sz) (x+sz,y+sz,z-sz) -- Top
  side (x,y,z) (x+sz,y,z) (x+sz,y,z-sz) (x,y,z-sz)             -- Bottom

degRad :: GLfloat -> GLfloat
degRad = (*) pi . flip (/) 180

initGL win = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  glEnable gl_LIGHTING
  glEnable gl_LIGHT0
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
  --let (xx,yx,zx) = (cos $ degRad cx,sin $ degRad cy,sin $ degRad cx)
  --    (xz,yz,zz) = (sin $ degRad cx,sin $ degRad cy,cos $ degRad cx)
  --(x',y',z') <- (cos $ degRad cx,sin $ degRad cy,sin $ degRad cx) `cross3` 
  --              (sin $ degRad cx,sin $ degRad cy,cos $ degRad cx)
  -- new axis of rotation.
  --glTranslatef (-x+z*sin (degRad cx)) (-y-z*sin (degRad cy)) 
  --             (-1.5+z*cos (degRad cy)*cos (degRad cx))
  glTranslatef 0 0 (-1.5)
  --putStrLn $ show $ sin $ degRad cy
  glRotatef cx 0 (cos (degRad cy)) (sin (degRad cy))
  glRotatef cy 1 0 0
  glBegin gl_QUADS
  --glColor3f 0.0 1.0 0.0
  --col <- newArray [0::GLfloat,1,0]
  withArray [0::GLfloat,1,0] $ glMaterialfv gl_FRONT gl_DIFFUSE
  {-glNormal3f 0 0 1
  glVertex3f x z (-y)
  glVertex3f (x+0.1) z (-y)
  glVertex3f (x+0.1) (z+0.1) (-y)
  glVertex3f x (z+0.1) (-y)-}
  cube (-0.05,0,0.05) 0.1
  withArray [1::GLfloat,0,0] $ glMaterialfv gl_FRONT gl_DIFFUSE
  side (-x+5,y,z+5) (-x-5,y,z+5) (-x-5,y,z-5) (-x+5,y,z-5)
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
    Camera (-cx+cxt,cy+cyt,0))

moveA :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) ->
  (GLfloat,GLfloat)
moveA (dx,dz) (xt,zt) (cx,cy) = let (s,c) = (sin $ degRad cx,cos $ degRad cx)
                                    (x',z',_) = norm3 (dx,dz,0)
                                    (x,z) = (x'*unit,z'*unit) in
  (x*c+z*s+xt,-x*s+z*c+zt)

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
