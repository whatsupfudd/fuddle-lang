{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text)
import Options.Applicative


data EnvOptions = EnvOptions {
    home :: Maybe Text
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)

data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  }

data Command =
  HelpCmd
  | VersionCmd
  | CompileCmd Text
  deriving stock (Show)

{- HERE: Additional structures for holding new command parameters:
Eg:
data ImportOpts = ImportOpts {
    taxonomy :: Text
    , path :: Text
  }
-}

parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "fuddle." <> header "fuddle - ."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "fuddleCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.fudd/fuddle.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )


commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("compile", compileOpts, "Starts a compilation.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> (cmdBuilder aCmd) <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))

compileOpts :: Parser Command
compileOpts =
  CompileCmd <$> strArgument (metavar "TMP_ELM" <> help "command for compiler.")
