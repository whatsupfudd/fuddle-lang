module Options  (
  module Options.Cli
  , module Options.ConfFile
  , module R
  , mergeOptions
 )
where

import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT


import Options.Cli
import Options.ConfFile
import Options.Runtime as R


mergeOptions :: CliOptions -> FileOptions -> EnvOptions -> RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  let
    defO = R.defaultRun
    -- Update from config file:
    fileO =
      let
        dbgO = case file.debug of
          Nothing -> defO
          Just aVal -> defO { R.debug = aVal }
        {- HERE: add additional configurations:
        Eg: rootO = case file.rootDir of
          Nothing -> dbO
          Just aVal -> dbO { Rt.root = DT.pack aVal }
        -}
      in
      dbgO
    -- TODO: update from CLI options
    cliO = case cli.debug of
      Nothing -> fileO
      Just aVal -> fileO { R.debug = aVal }
    -- TODO: update from ENV options
    envO = cliO
  in
  envO
