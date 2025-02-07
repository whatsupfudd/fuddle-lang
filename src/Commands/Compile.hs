module Commands.Compile where

import qualified ElmMain as Tm
import qualified Options.Runtime as Rto
import Data.List (intercalate)

compileCmd :: [String] -> Rto.RunOptions -> IO ()
compileCmd args rtOpts = do
  putStrLn $ "@[compileCmd] starting: " <> intercalate ", " args
  Tm.main $ args <> ["--output", "out.js"]
