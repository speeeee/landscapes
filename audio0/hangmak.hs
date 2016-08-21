import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Applicative
import Data.List.Split
import Data.Foldable (foldlM)
import Data.List (find,intercalate,union,uncons)
import Text.Regex

import Debug.Trace

data Macro = Macro { patt :: Regex, 
                     rep :: (([[Char]],[Macro],[Char]) -> ([[Char]],[Macro],[Char])) }
-- (storage (ex. used by stack initialization), macro list, target string)

-- this is a very simple/naive implementation, will fix later.
-- for now, this implementation is fairly slow.

-- temporary
thd (_,_,x) = x

main = do
  (o:i:_) <- getArgs 
  (thd <$> parse [] mlst <$> readFile (i++".cz")) >>= writeFile o

allIn :: [[Char]] -> IO ([[Char]],[Macro],[Char])
allIn = foldlM (\n k -> inF (k++".cz") n) ([],[],[])

inF :: [Char] -> ([[Char]],[Macro],[Char]) -> IO ([[Char]],[Macro],[Char])
inF i (s,m,_) = parse s m <$> readFile i

qexp :: [Char]
qexp = "`((\\\\.|[^\\\\`])*)`"
qexp' = concat [qexp,"[[:space:]]*",qexp,"[[:space:]]*DEF"]

-- "stk_fun" -> "stk = stk-fun(stk);\n"
-- "[A-Za-z0-9_]" -> stk = $$(stk);\n"

-- expects the first subexpression as what is wanted.
getOccs  :: Regex -> [Char] -> [[Char]]
getOccs' :: Regex -> [Char] -> [[Char]] -> [[Char]]
getOccs r k = getOccs' r k []
getOccs' r k q = case matchRegexAll r k of
                 Just (_,_,_,d) -> d
                 Nothing -> []

subRS :: [Char] -> [Char] -> [Char]
subRS o k = foldl (\n e -> subRegex (mkRegex e) n (drop (read [(last e)]::Int) k))
  o (map (("\\$\\$"++) . show) [0,1..9])

mlst :: [Macro]
mlst = [Macro (mkRegex " hallo ") (\(s,m,k) -> (s,m,subRegex (mkRegex "a") k "e"))
       ,Macro (mkRegex qexp')
              (\(s,m,k) -> let (i:_:o:_) = getOccs (mkRegex qexp') k
                           in (s,Macro (mkRegex i) 
                            (\(s',m',k') -> (s',m',subRS o (tail k'))):m,""))]

parse :: [[Char]] -> [Macro] -> [Char] -> ([[Char]],[Macro],[Char])
parse s m c = replac (s,m,c)

replac :: ([[Char]],[Macro],[Char]) -> ([[Char]],[Macro],[Char])
replac (s,m,c) = let q = filter ((c/=) . thd) $ map (\k -> findRepla (k,c) m s) m
  in if null q then (s,m,c) else replac $ head q

findRepla :: (Macro,[Char]) -> [Macro] -> [[Char]] -> ([[Char]],[Macro],[Char])
findRepla (m,c) ms s = trace (show c) $ case matchRegexAll (patt m) c of
                       Just (a,b,c,_) -> let (s',m',b') = (rep m) (s,ms,b)
                                         in (s',m',concat [a,b',c])
                       Nothing -> (s,ms,c)

getImports :: [[Char]] -> IO [[Char]]
getImports f = do
  a <- foldl1 union <$> mapM
    (\k -> do 
      let k' = k++".imp"
      e <- doesFileExist k'
      if e then flip (++) [k] <$> (words <$> readFile k')
           else return [k]) f
  if a==f then return a else getImports a
