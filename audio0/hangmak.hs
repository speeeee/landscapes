import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split
import Data.List (find,intercalate,intersect,uncons)
import Text.Regex

data Macro = Macro { patt :: Regex, rep :: (([Macro],[Char]) -> ([Macro],[Char])) }

-- this is a very simple/naive implementation, will fix later.
-- for now, this implementation is fairly slow.

main = do
  (o:i:_) <- getArgs 
  (snd <$> parse mlst <$> readFile (i++".cz")) >>= writeFile o

mlst :: [Macro]
mlst = [Macro (mkRegex " hallo ") (\(m,k) -> (m,subRegex (mkRegex "a") "e" k))]

parse :: [Macro] -> [Char] -> ([Macro],[Char])
parse = curry replac

replac :: ([Macro],[Char]) -> ([Macro],[Char])
replac (m,c) = let q = filter ((c/=) . snd) $ map (\k -> findRepla (k,c) m) m
  in if null q then (m,c) else replac $ head q

findRepla :: (Macro,[Char]) -> [Macro] -> ([Macro],[Char])
findRepla (m,c) ms = case matchRegexAll (patt m) c of
                     Just (a,b,c,_) -> let (m',b') = (rep m) (ms,b)
                                       in (m',concat [a,b',c])
                     Nothing -> (ms,c)
