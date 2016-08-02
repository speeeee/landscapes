import System.Process
import System.Environment (getArgs)
import GHC.IO.Handle
import Control.Applicative
import Data.List.Split
import Data.List (find,intercalate)
import Data.ByteString.Char8 (unpack)

import Debug.Trace

-- TODO: write-up imports (static-only) .nincs files.

data Img = Img Int Int [[[Int]]] deriving (Show,Eq)
data Frame = Frame Int Int Int Int deriving (Show,Eq)
data Fun = Fun Img [Char] Frame [Int] Bool deriving (Show,Eq) -- the Bool is to confirm dead-end

funs = ["+",":","I@","ERROR_NO_FUNCTION"]

main = do
  a <- getArgs
  e <- splitOn " @" <$> (readFile $ head a)
  (_,Just plst,_,_) <-
    createProcess (proc "./char_read" (tail a)) { std_out = CreatePipe }
  c <- hGetContents plst
  -- e <- splitOn "@" <$> readFile "fs.nincs"
  -- let (g:b:_) = toImgs $ words c
  --    e = imgCmpS g (extr (1,1,3,3) b) [0,249,0,255]
  let ii = toImgs $ words c
  putStrLn $ eval (last ii) (Frame 4 2 3 3) (mkFuns e $ init ii)

concatS :: [[Char]] -> [Char]
concatS = intercalate " "

mkFuns :: [[Char]] -> [Img] -> [Fun]
mkFuns cc = map (\(c,i) -> Fun i (concatS $ tail $ words c)
                             (Frame 1 1 1 1) (map (\e -> read e::Int) $ splitOn "," $ head $ words c) False) . zip cc

-- WIP
-- function will be cleaned.
{-eval :: Img -> Frame -> [Fun] -> [Char]
eval i f fs = eval' i f fs i
eval' :: Img -> Frame -> [Fun] -> Img -> [Char]
eval' = uncurry execF . uncurry toF . flip extrf-}
eval :: Img -> Frame -> [Fun] -> [Char]
eval i f fs = uncurry execF (uncurry toF (extrf f i) fs) i

toF :: Frame -> Img -> [Fun] -> (Fun,[Fun])
toF (Frame fx fy fw fh) (Img w h d) fs =
  case find (\(Fun i _ _ c _) -> imgCmpS i (Img w h d) c) fs of
    Just (Fun (Img wi hi _) p f c g) ->
      (Fun (Img w h d) p (fsc (w`div`wi) (h`div`hi) (Frame fx fy fw fh)) c g,fs)
    Nothing -> (Fun (Img 0 0 []) "ERROR_NO_FUNCTION" (Frame 0 0 0 0) [] False,fs)

{-evalArg :: Fun -> [Fun] -> Img -> Frame -> [Char]
evalArg (Fun _ _ _ _ False) _ _ _ = f
evalArg (Fun _ f arg (sx,sy) _) fs i fr = concat [f," ",eval i arg fs]-}

-- + : I@ -1 0 1 1 list I@ 1 0 1 1 <- reverse
execF :: Fun -> [Fun] -> Img -> [Char]
execF (Fun _ p f _ _) fs bg = concatS $ callF fs f bg $ words p

callF :: [Fun] -> Frame -> Img -> [[Char]] -> [[Char]]
callF fs fr bg =
  foldr (\k n -> if k`elem`funs then prim k n fs bg fr else k:n) []

prim :: [Char] -> [[Char]] -> [Fun] -> Img -> Frame -> [[Char]]
prim k n fs i (Frame x y w h) {- pos -} = case k of
  "+"  -> let (q:p:_) = map (\e -> read e::Int) $ take 2 n
          in (show $ q+p):(drop 2 n)
  ":"  -> (concat [n!!0,",",n!!1]):(drop 2 n)
  "I@" -> let (xi:yi:wi:hi:_) = map (\e -> read e::Int) $ take 4 n
          in eval i (Frame (xi*w+x) (yi*h+y) (wi*w) (hi*h)) fs:drop 4 n
  _    -> n

-- + (: (I@ -1 0 1 1 (list I@ 1 0 1 1)))
--groupF :: [[Char]] -> [[Char]]
--groupF = foldr (\k (n:ns) -> if k`elem`funs then []:n:ns else (k:n):ns) []

fsc :: Int -> Int -> Frame -> Frame
fsc w h (Frame xi yi wi hi) = (Frame xi yi w h)

toImgs :: [[Char]] -> [Img]
toImgs = flip toImgs' []

toImgs' :: [[Char]] -> [Img] -> [Img]
toImgs' [] i = i
toImgs' (w:h:s) i = let a  = wi*hi
                        wi = (read w::Int)
                        hi = (read h::Int)
  in toImgs' (drop a s) (i++[Img wi hi (mk2D wi (toCols (take a s)))])
toImgs' _ i = i

toCols :: [[Char]] -> [[Int]]
toCols = map (\k -> map (\e -> read e :: Int) (splitOn "," k))

mk2D :: Int -> [a] -> [[a]]
mk2D' :: Int -> [a] -> [[a]] -> [[a]]
mk2D w d = mk2D' w d []
mk2D' w [] n = reverse n
mk2D' w d n = mk2D' w (drop w d) (take w d:n)

toInts :: [Bool] -> [Int]
toInts = map (\k -> if k then 1 else 0)

extr' :: (Int,Int,Int,Int) -> [[[Int]]] -> [[[Int]]]
extr' (x,y,w,h) = map (\k -> take w $ drop x k) . take h . drop y

extr :: (Int, Int, Int, Int) -> Img -> Img
extr (x,y,w,h) (Img bw bh dat) = Img w h (extr' (x,y,w,h) dat)

extrf :: Frame -> Img -> (Frame,Img)
extrf (Frame x y w h) = ((,) (Frame x y w h)) . extr (x,y,w,h)

mtob :: Maybe Bool -> Bool
mtob (Just a) = a
mtob Nothing = False

everyNth :: Int -> [a] -> [a]
everyNth i d = map head $ mk2D i d

-- the first argument is the base.
eq :: Img -> Img -> Bool
eq (Img _ _ da) (Img _ _ db) =
  all (\(a,b) -> (a!!3) == 0 || take 3 a == take 3 b) $ zip (concat da) (concat db)

scale :: Img -> (Int,Int) -> Maybe Img
scale (Img w h d) (wh,hh) =
  if w`mod`wh==0&&h`mod`hh==0
  then Just (Img wh hh (map (everyNth (w`div`wh)) (everyNth (h`div`hh) d)))
  else Nothing

--imgCmp :: Img -> Img -> Bool
--imgCmp (Img w h d) = mtob . (eq (Img w h d) <$> (flip scale (w,h)))
imgCmpS :: Img -> Img -> [Int] -> Bool
imgCmpS (Img w h d) ib c = 
  mtob ((==Img w h d) <$> (scale (silh ib c) (w,h)))

-- to be better written soon
silh :: Img -> [Int] -> Img
silh (Img w h d) e = 
  Img w h $ mk2D w $ map (\k -> if e==k then k else [255,255,255,255]) $ concat d
