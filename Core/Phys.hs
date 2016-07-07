module Core.Phys (Force(..),Obj(..),Camera(..),acc,opf,nextp) where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)

import Util.Vec3

-- Force    =       vector time
data Force  = Force Pt Int deriving (Show,Eq)
-- Obj      =     pos velocity [forces]
data Obj    = Obj Pt Pt [Force] deriving (Show,Eq)
-- Camera   =        pos theta
data Camera = Camera Pt GLfloat deriving (Show,Eq)
vsum :: [Pt] -> Pt
vsum = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)

acc :: Force -> GLfloat -> Pt
acc (Force pt t) m = op3s (flip (/)) m pt

opf :: (GLfloat -> GLfloat -> GLfloat) -> Pt -> Force -> Force
opf op p (Force pt t) = (Force (op3 op p pt) t)

--nextv :: (Force,Pt) -> Pt -> GLfloat -> (Force,Pt)
--nextv (Force pt t) v m = (Force pt (t-1), op3 (+) (acc (Force pt t) m) v)

nextp :: Pt -> Pt -> Pt
nextp = op3 (+)

appf :: [Force] -> Pt -> ([Force],Pt)
appf fs v = let fs' = map (\(Force pt t) -> Force pt (t-1)) fs in
  (fs',op3 (+) (vsum (map (\(Force pt _) -> pt) fs')) v)

rem0 :: [Force] -> [Force]
rem0 = filter (\(Force _ t) -> t /= 0)

-- next frame assuming no collisions
nextf :: Obj -> Obj
nextf (Obj p v fs) = Obj (nextp p v) v' (rem0 fs') where (fs',v') = appf fs v

     -- object before collision -> vector normal to colliding surface
  -- -> distance from surface on next frame -> object after  collision
{-coll :: Obj -> Pt -> GLfloat -> Obj
coll (Obj p v fs) n d = 
  Obj -}
