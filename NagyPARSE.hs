import System.Process
import GHC.IO.Handle

main = do
  (_,Just plst,_,_) <- 
    createProcess (proc "./char_read" []) { std_out = CreatePipe }
  c <- hGetContents plst
  putStrLn c
