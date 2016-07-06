module Util.Vec3 (Pt(..),norm3,cross3,sub3,op3,op3s,op3e,op3es) where

import Graphics.Rendering.OpenGL.Raw

type Pt = (GLfloat,GLfloat,GLfloat)

norm3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
norm3 (a,b,c) = (a/l',b/l',c/l') where len = sqrt(a*a+b*b+c*c)
                                       l' = if len == 0 then 1 else len
cross3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
       -> (GLfloat,GLfloat,GLfloat)
cross3 (ax,ay,az) (bx,by,bz) = (ay*bz-az*by,bx*az-bz*ax,ax*by-ay*bx)
--dot3 :: Pt -> Pt -> GLfloat
--dot3 = opV (+) . op3 (*)


sub3 :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
     -> (GLfloat,GLfloat,GLfloat)
sub3 (a,b,c) (d,e,f) = (a-d,b-e,c-f)
opV :: (GLfloat -> GLfloat -> GLfloat) -> Pt -> GLfloat
opV op (x,y,z) = x`op`y`op`z
op3 :: (GLfloat -> GLfloat -> GLfloat) -> Pt -> Pt -> Pt
op3 op (a,b,c) (d,e,f) = (a`op`d,b`op`e,c`op`f)
op3s :: (GLfloat -> GLfloat -> GLfloat) -> GLfloat -> Pt -> Pt
op3s op a (d,e,f) = (a`op`d,a`op`e,a`op`f)
op3e :: (GLfloat -> GLfloat -> GLfloat) -> Pt -> (Pt,Pt,Pt) -> (Pt,Pt,Pt)
op3e  op v (pa,pb,pc) = (op3 op v pa, op3 op v pb, op3 op v pc)
op3es op s = op3e op (s,s,s)
