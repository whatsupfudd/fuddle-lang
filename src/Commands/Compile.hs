module Commands.Compile where

import qualified ElmMain as Tm
import qualified Options.Runtime as Rto

compileCmd :: [String] -> Rto.RunOptions -> IO ()
compileCmd args rtOpts = do
  putStrLn "@[compileCmd] starting."
  Tm.main args
