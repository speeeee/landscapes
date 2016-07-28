import System.Process
import GHC.IO.Handle
import Control.Applicative
import Data.List.Split

data Img = Img Int Int [[[Int]]] deriving (Show,Eq)
data Frame = Int Int Int Int deriving (Show,Eq)
data Fun = Img [Char] Frame Bool deriving (Show,Eq) -- the Bool is to confirm dead-end

main = do
  (_,Just plst,_,_) <-
    createProcess (proc "./char_read" []) { std_out = CreatePipe }
  c <- hGetContents plst
  let (g:b:_) = toImgs $ words c
      e = imgCmpS g (extr (1,1,3,3) b) [0,249,0,255]
  putStrLn $ show e

-- function will be cleaned.
eval :: Img -> Frame -> [Fun] -> [Char]
eval i f fs = eval' i f fs i
eval' :: Img -> Frame -> [Fun] -> Img -> [Char]
eval' = execF . uncurry evalArg . toF . flip extrf

toF :: Img -> [Fun] -> (Fun,[Fun])
toF i fs = case find (flip imgCmpS i) fs of
  Just a -> (a,fs)
  Nothing -> (Fun (Img 0 0 []) "ERROR_NO_FUNCTION" (Frame 0 0 0 0) False,fs)

evalArg :: Fun -> [Fun] -> Img -> [Char]
evalArg (Fun _ _ _ False) _ _ = f
evalArg (Fun _ f arg _) fs i = concat [f," ",eval i arg fs]

execF :: [Char] -> [Char]
execF = id

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

extrf :: Frame -> Img -> Img
extrf (Frame x y w h) = extr (x,y,w,h)

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
