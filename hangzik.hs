{-# LANGUAGE MultiWayIf #-}

import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split
import Data.List (find,intercalate,union)
import System.Directory (doesFileExist)

import Debug.Trace

data Fun = Fun { na :: [Char], fn :: [[Char]], p :: ([[Char]] -> [[Char]]) }

spc :: [Char]
spc = " \n\r\t"

prims :: [Fun] -> [Fun]
prims fs = 
  [Fun "op" [] (\(o:a:b:n) -> case find ((o==) . fst) 
                                     [("+'",(+)),("-'",(-)),("*'",(*)),("/'",div)] of
                              Just (_,f) -> (show $ f (read a::Int) (read b::Int)):n
                              Nothing -> n)
  ,Fun "!" [] (\(q:n) -> snd $ parse (tok q) (fs,n))
  ,Fun "?" [] (\(qt:qf:c:n) -> (if c=="True" then qt else qf):n)
  ,Fun "=" [] (\(a:b:n) -> (show $ a==b):n)
  ,Fun "sig" [] (\(a:n) -> (show $ signum (read a::Int)):n)
  ,Fun "sw" [] (\(a:b:n) -> b:a:n), Fun "dr" [] tail
  ,Fun "du" [] (\(a:n) -> a:a:n), Fun "pick" [] (\(a:b:c:n) -> c:a:b:c:n)
  ,Fun "DEF" [] id]

tok :: [Char] -> [[Char]]
tok = reverse . lexe . replace '|' '"'

main = do
  (o:i:_) <- getArgs
  --imps <- getImports [i]
  (intercalate " " <$> reverse <$> snd <$> inF (i++".hz")) >>= writeFile o

getImports :: [[Char]] -> IO [[Char]]
--getImports i = do
--  return if doesFileExist i then getImports' <$> words <$> readFile i else []
getImports f = do
  a <- foldl1 union <$> mapM
    (\k -> do 
      let k' = k++".imp"
      e <- doesFileExist k'
      if e then flip (++) [k] <$> (words <$> readFile k')
           else return [k]) f
  if a==f then return a else getImports a

inF :: [Char] -> IO ([Fun],[[Char]])
inF i = flip parse (prims [],[]) <$> reverse <$> lexe <$> readFile i

drip :: a -> [b] -> [(a,b)]
drip f = map (\k -> (f,k))

shl :: ([a],[a]) -> ([a],[a])
shl (a,e:b) = (a++[e],b)

lexe :: [Char] -> [[Char]]
lexe = filter (not . null) .
  chop (\xl@(x:xs) -> if | x`elem`spc  -> ("",tail xl)
                         | x=='"'  -> let (a,b) = span ('"'/=) xs in (a,tail b)
                         | otherwise -> span (' '/=) xl)

-- function to be removed.
replace :: Char -> Char -> [Char] -> [Char]
replace s r = map (\c -> if c==s then r else c)

parse :: [[Char]] -> ([Fun],[[Char]]) -> ([Fun],[[Char]])
--parse = foldr (\k (f,n) -> appf n <$> find (k==) f >>= (f,k:n)) [[],[]]
parse a b = foldr (\k (f,n) -> case find ((k==) . na) (f++prims f) of 
                                    (Just a) -> appf f a n
                                    _ -> (f,k:n)) b a

appf :: [Fun] -> Fun -> [[Char]] -> ([Fun],[[Char]])
appf fs (Fun "DEF" _ _) (name:q:n) = ((Fun name (tok q) id:fs),n)
appf fs (Fun _ o s) n = if null o then (fs,s n) else parse o (fs,n)
