import System.Environment (getArgs)
import Control.Applicative
import Data.List.Split
import Data.List (find,intercalate,union,uncons)
import Text.Regex

data Macro = Macro { patt :: Regex, rep :: (([Macro],[Char]) -> ([Macro],[Char])) }

-- this is a very simple/naive implementation, will fix later.
-- for now, this implementation is fairly slow.

main = do
  (o:i:_) <- getArgs 
  (parse <$> readFile $ i++".cz") >>= writeFile o

mlst :: [Macro]
mlst = [Macro (mkRegex " hallo ") (subRegex (mkRegex "a") "e")]

parse :: [Char] -> ([Macro],[Char])
parse = snd . replac . (,) mlst

replac :: ([Macro],[Char]) -> ([Macro],[Char])
replac s = let q = intersect s $ map (flip findRepla) s
  in if null q then s else replac $ head q

findRepla :: (Macro,[Char]) -> [Macro] -> ([Macro],[Char])
findRepla (m,c) ms = case regexMatchAll m c of
                     Just (a,b,c,_) -> let (m',b') = (rep m) ms b
                                       in (m',concat [a,b',c]
                     Nothing -> (m,c)
