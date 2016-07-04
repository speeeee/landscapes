module Core.Phys (Force(..),acc,opf,newv,newp) where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)

import Util.Vec3

-- Force   = vector time
data Force = Force Pt Int deriving (Show,Eq)

acc :: Force -> GLfloat -> Pt
acc (Force pt t) m = op3s (flip (/)) m pt

opf :: (GLfloat -> GLfloat -> GLfloat) -> Pt -> Force -> Force
opf op p (Force pt t) = (Force (op3 op p pt) t)

newv :: (Force,Pt) -> (Force,Pt)
newv (Force pt t) v = (Force pt (t-1), op3 (+) pt v)

newp :: Pt -> Pt -> Pt
newp = op3 (+)
