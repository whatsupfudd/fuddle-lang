{-# LANGUAGE OverloadedStrings #-}
module ApiGen
  ( Flags(..)
  , Output(..)
  , ReportType(..)
  , run
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Generate
import qualified Generate.Html as Html
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import Terminal (Parser(..))



-- FLAGS


data Flags =
  Flags
    { _debug :: Bool
    , _optimize :: Bool
    , _output :: Maybe Output
    , _report :: Maybe ReportType
    , _docs :: Maybe FilePath
    }


data Output
  = JS FilePath
  | Html FilePath
  | C FilePath FilePath
  | DevNull


data ReportType
  = Json



-- RUN


type Task a = Task.Task Exit.Make a


run :: [FilePath] -> () -> IO ()
run paths flags =
  putStrLn $ "@[ApiGen.run] starting."

