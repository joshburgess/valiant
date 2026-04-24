module Valiant.CLI.Output
  ( printHeader
  , printProgress
  , printOk
  , printFailed
  , printStale
  , printMissing
  , printSummary
  , printLn
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Console.ANSI
  ( Color (..)
  , ColorIntensity (..)
  , ConsoleLayer (..)
  , SGR (..)
  , hSupportsANSIColor
  , setSGRCode
  )
import System.IO (stdout)

-- | Print a section header.
printHeader :: Text -> IO ()
printHeader hdr = TIO.putStrLn $ "valiant: " <> hdr

-- | Print progress line without trailing newline.
--
-- @printProgress 3 10 "sql/users/find_by_id.sql"@ produces:
--
-- >  [3/10] sql/users/find_by_id.sql ...
printProgress :: Int -> Int -> FilePath -> IO ()
printProgress n total path = do
  let pad = length (show total)
      idx = leftPad pad (show n)
  putStr $ "  [" <> idx <> "/" <> show total <> "] " <> path <> " ... "

-- | Print green "ok" with newline.
printOk :: IO ()
printOk = withColor Green $ TIO.putStrLn "ok"

-- | Print red "FAILED" with detail on the next line.
printFailed :: Text -> IO ()
printFailed detail = do
  withColor Red $ TIO.putStrLn "FAILED"
  TIO.putStrLn $ "    " <> detail

-- | Print yellow "STALE" with newline.
printStale :: IO ()
printStale = withColor Yellow $ TIO.putStrLn "STALE"

-- | Print yellow "MISSING" with newline.
printMissing :: IO ()
printMissing = withColor Yellow $ TIO.putStrLn "MISSING"

-- | Print a summary line.
printSummary :: Int -> Int -> IO ()
printSummary ok total
  | ok == total = do
      printLn ""
      withColor Green . TIO.putStrLn $
        "  All " <> T.pack (show total) <> " queries valid."
  | otherwise = do
      printLn ""
      withColor Red . TIO.putStrLn $
        "  "
          <> T.pack (show ok)
          <> " of "
          <> T.pack (show total)
          <> " queries valid. "
          <> T.pack (show (total - ok))
          <> " failed."

-- | Print a line of text.
printLn :: Text -> IO ()
printLn = TIO.putStrLn

-- Helpers -----------------------------------------------------------------

leftPad :: Int -> String -> String
leftPad n s = replicate (n - length s) ' ' <> s

withColor :: Color -> IO () -> IO ()
withColor color action = do
  supportsColor <- hSupportsANSIColor stdout
  if supportsColor
    then do
      putStr (setSGRCode [SetColor Foreground Vivid color])
      action
      putStr (setSGRCode [Reset])
    else action
