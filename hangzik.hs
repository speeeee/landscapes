{-# LANGUAGE MultiWayIf #-}

import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split
import Data.List (find)

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
  ,Fun "!" [] (\(q:n) -> parse (tok q) (fs,n))
  ,Fun "?" [] (\(qt:qf:c:n) -> (if c=="True" then qt else qf):n)
  ,Fun "sw" [] (\(a:b:n) -> b:a:n), Fun "dr" [] tail
  ,Fun "du" [] (\(a:n) -> a:a:n), Fun "pick" [] (\(a:b:c:n) -> c:a:b:c:n)]

tok :: [Char] -> [[Char]]
tok = reverse . lexe . replace '|' '"'

main = do
  (i:o:_) <- getArgs
  (concat <$> (flip parse (prims [],[]) <$> 
    (reverse <$> (lexe <$> readFile i))))
    >>= writeFile o

drip :: a -> [b] -> [(a,b)]
drip f = map (\k -> (f,k))

lexe :: [Char] -> [[Char]]
lexe = filter (not . null) .
  chop (\xl@(x:_) -> if | x`elem`spc  -> ("",tail xl)
                        | x=='"'  -> span ('"'==) xl
                        | otherwise -> span (' '/=) xl)

-- function to be removed.
replace :: Char -> Char -> [Char] -> [Char]
replace s r = map (\c -> if c==s then r else c)

parse :: [[Char]] -> ([Fun],[[Char]]) -> [[Char]]
--parse = foldr (\k (f,n) -> appf n <$> find (k==) f >>= (f,k:n)) [[],[]]
parse a b = snd $ foldr (\k (f,n) -> case find ((k==) . na) (f++prims f) of 
                                     (Just a) -> appf f a n
                                     _ -> (f,k:n)) b a

appf :: [Fun] -> Fun -> [[Char]] -> ([Fun],[[Char]])
appf fs (Fun "DEF" _ _) (name:q:n) = ((Fun name (tok q) id:fs),n)
appf fs (Fun _ o s) n = if null o then (fs,s n) else (fs,parse o (fs,n))
