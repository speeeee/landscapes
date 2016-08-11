import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split

import Debug.Trace

data Fun = Fun { na :: [Char], fn :: [[Char]], p :: ([[Char]] -> [[Char]]) }

prims :: [Fun] -> [Fun]
prims fs = 
  [Fun "op" [] (\(o:a:b:n) -> case find ((o==) . fst) 
                                     [("+'",(+)),("-'",(-)),("*'",(*)),("/'",div)] of
                              Just (_,f) -> str $ f (read a::Int) (read b::Int):n
                              Nothing -> n)
  ,Fun "!" [] (\(q:n) -> parse (reverse $ lex $ replace '|' '"' q) $ zip [fs] n)
  ,Fun "?" [] (\(qt:qf:c:n) -> (if c=="True" then qt else qf):n)
  ,Fun "sw" [] (\(a:b:n) -> b:a:n), Fun "dr" [] tail
  ,Fun "du" [] (\(a:n) -> a:a:n), Fun "pick" [] (\(a:b:c:n) -> c:a:b:c:n)]

main = do
  (i:o) <- getArgs
  flip parse (prims []) <$> (reverse <$> (lex <$> readFile i)) >>= writeFile o

lex :: [Char] -> [[Char]]
lex = chop (\xl@(x:_) -> case x of ' '  -> span (' '==) xl
                                   '"'  -> span ('"'==) xl
                                   _    -> span (' '/=) xl)

parse :: [[Char]] -> [([Fun],[[Char]])] -> [[Char]]
--parse = foldr (\k (f,n) -> appf n <$> find (k==) f >>= (f,k:n)) [[],[]]
parse = flip foldr (\k (f,n) -> case find (k==) (map na (f++prims f)) of 
                                (Just a) -> appf f a n
                                _ -> (f,k:n))

appf :: [Fun] -> Fun -> ([Fun],[[Char]])
appf fs (Fun "DEF" _ _) (name:q:n) = ((Fun name q id:fs),n)
appf fs (Fun _ o s) n = if empty o then (fs,s n) else (fs,parse o $ zip [fs] n)
