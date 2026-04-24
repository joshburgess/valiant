module Valiant.CLI.App
  ( run
  ) where

import Valiant.CLI.Command.Check (runCheck)
import Valiant.CLI.Command.Generate (GenerateOpts (..), runGenerate)
import Valiant.CLI.Command.Prepare (runPrepare)
import Valiant.CLI.Command.Types (runTypes)
import Valiant.CLI.Command.Watch (runWatch)
import Valiant.CLI.Config (AppEnv (..), resolveEnv)
import Options.Applicative
import System.Exit (ExitCode, exitWith)

-- | Entry point for the @valiant@ CLI.
run :: IO ()
run = do
  opts <- execParser parserInfo
  env <- resolveEnv (optSqlDir opts) (optCacheDir opts) (optDatabaseUrl opts) (optVerbose opts)
  code <- dispatch env (optCommand opts)
  exitWith code

-- CLI option types --------------------------------------------------------

data Opts = Opts
  { optSqlDir :: FilePath
  , optCacheDir :: FilePath
  , optDatabaseUrl :: Maybe String
  , optVerbose :: Bool
  , optCommand :: Command
  }

data PrepareOpts = PrepareOpts
  { prepInlineDirs :: [FilePath]
  -- ^ Directories to scan for inline @query "..."@ calls in @.hs@ files.
  }

data Command
  = CmdPrepare PrepareOpts
  | CmdCheck
  | CmdTypes (Maybe FilePath)
  | CmdGenerate GenerateOpts
  | CmdWatch

-- Dispatch ----------------------------------------------------------------

dispatch :: AppEnv -> Command -> IO ExitCode
dispatch env = \case
  CmdPrepare opts -> runPrepare env (prepInlineDirs opts)
  CmdCheck -> runCheck env
  CmdTypes mFile -> runTypes env mFile
  CmdGenerate opts -> runGenerate env opts
  CmdWatch -> runWatch env

-- Parsers -----------------------------------------------------------------

parserInfo :: ParserInfo Opts
parserInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> header "valiant — compile-time checked SQL for Haskell"
        <> progDesc "Validate .sql files against a live Postgres database and cache type metadata."
    )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption
      ( long "sql-dir"
          <> metavar "DIR"
          <> value "sql"
          <> showDefault
          <> help "Directory containing .sql files"
      )
    <*> strOption
      ( long "cache-dir"
          <> metavar "DIR"
          <> value ".valiant"
          <> showDefault
          <> help "Directory for cached query metadata"
      )
    <*> optional
      ( strOption
          ( long "database-url"
              <> metavar "URL"
              <> help "PostgreSQL connection URL (overrides DATABASE_URL env var)"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose output"
      )
    <*> commandParser

commandParser :: Parser Command
commandParser =
  subparser
    ( command "prepare"
        ( info
            ( CmdPrepare
                <$> ( PrepareOpts
                        <$> many
                          ( strOption
                              ( long "inline"
                                  <> metavar "DIR"
                                  <> help "Scan .hs files in DIR for inline query \"...\" calls"
                              )
                          )
                    )
            )
            (progDesc "Prepare all .sql files (and inline queries) against the database")
        )
        <> command "check" (info (pure CmdCheck) (progDesc "Check that all cache files are current"))
        <> command
          "types"
          ( info
              ( CmdTypes
                  <$> optional (argument str (metavar "FILE" <> help "Show types for a specific .sql file"))
              )
              (progDesc "Print inferred Haskell types for queries")
          )
        <> command
          "generate"
          ( info
              ( CmdGenerate
                  <$> ( GenerateOpts
                          <$> strOption
                            ( long "module-prefix"
                                <> metavar "PREFIX"
                                <> value "Queries"
                                <> showDefault
                                <> help "Module name prefix for generated modules"
                            )
                          <*> strOption
                            ( long "output-dir"
                                <> metavar "DIR"
                                <> value "src"
                                <> showDefault
                                <> help "Output directory for generated Haskell files"
                            )
                      )
              )
              (progDesc "Generate Haskell binding modules from .sql files")
          )
        <> command "watch" (info (pure CmdWatch) (progDesc "Watch .sql files for changes and re-prepare"))
    )
