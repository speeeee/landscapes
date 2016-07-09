module Core.Rules where

import Graphics.Rendering.OpenGL.Raw
import qualified Data.ByteString as B
import Text.Read (readMaybe)

-- essentially the data that will be manipulated when passing through certain bounds.
-- an example is possibly a waterfall, that when entered adds a new rule to decrease
-- the y-val of the object.

--                 id  fields           id  Sid   funs/field
data Rule = Struct Int [GLfloat] | Tile Int Int [GLfloat -> GLfloat]

-- example program: 000002 0700000000 0700000000 ; Struct 0 x y
--                  010001                       ; Tile   0 (S0) (0)
--                  0201 0701000000 03 03          ; Call   1"-" 1 END END
--                  0400 0700000000 0701000000 0703000000 0701000000 0703000000 0700000000
--                  ; place triangle at points (T0)
--                  0500 0700000000 0700000000 ; place player at points (S0)
-- when object passes through triangle placed, it will fall at 1 spixel/frame.

-- when defining tile behaviors, they can behave differently to different objects
-- by using the same tile id but a different struct id.

-- 00: Struct   
-- 01: Tile
-- 02: Call
-- 03: End
-- 04: PlaceTri
-- 05: PlaceObj
-- 06: Int
-- 07: GLfloat

tokenize :: [B.ByteString] -> [B.ByteString]
tokenize l = if B.null $ last l then init l else 
  tokenize (case B.head $ last l of
    0 -> e 3
    1 -> e 2
    2 -> e 2
    3 -> e 1
    4 -> e 2
    5 -> e 2
    6 -> e 5
    7 -> e 5
    _ -> l++[B.empty]) where e x = init l++[a,b] where (a,b) = B.splitAt x $ last l
