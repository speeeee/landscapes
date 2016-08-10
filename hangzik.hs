import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split

import Debug.Trace

data Fun = Fun [[Char]] ([[Char]] -> [[Char]])

prims :: [Fun]
prims = []

main = do
  (i:o) <- getArgs
  flip parse prims <$> (reverse <$> (lex <$> readFile i)) >>= writeFile o

lex :: [Char] -> [[Char]]
lex = chop (\xl@(x:_) -> case x of ' '  -> span (' '==) xl
                                   '"'  -> span ('"'==) xl
                                   _    -> span (' '/=) xl)

parse :: [[Char]] -> [([Fun],[Char])] -> [[Char]]
--parse = foldr (\k (f,n) -> appf n <$> find (k==) f >>= (f,k:n)) [[],[]]
parse = flip foldr(\k (f,n) -> case find (k==) f of (Just a) -> appf f a n
                                                     _ -> (f,k:n))

appf :: [Fun] -> Fun -> [[Char]]
appf fs (Fun o s) n = if empty o then s n else parse o $ zip [fs] n
