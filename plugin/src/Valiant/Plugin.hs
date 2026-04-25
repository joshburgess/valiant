-- | GHC source plugin for compile-time SQL validation.
--
-- Usage:
--
-- > {-# OPTIONS_GHC -fplugin=Valiant.Plugin
-- >                 -fplugin-opt=Valiant.Plugin:sql-dir=sql #-}
module Valiant.Plugin
  ( plugin
  ) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import GHC.Driver.Env.Types (Hsc)
import GHC.Driver.Plugins (ParsedResult (..))
import GHC.Hs (HsParsedModule (..))
import GHC.Unit.Module.ModSummary (ModSummary)
import GHC.Plugins
  ( CommandLineOption
  , GenLocated (..)
  , Plugin (..)
  , PluginRecompile (..)
  , defaultPlugin
  )
import GHC.Tc.Types (TcGblEnv (..), TcM)
import Valiant.Plugin.Rewrite (rewriteModule)
import GHC.Utils.Fingerprint (fingerprintFingerprints, fingerprintString)
import Valiant.Plugin.Cache (findCacheFile)
import Valiant.Plugin.Compat (addFileDependency)
import Valiant.Plugin.Config (PluginConfig (..), parseOptions, resolveConfig)
import Valiant.Plugin.Errors (errCacheStaleOrMissing, errSqlFileNotFound)
import Valiant.Plugin.Hash (sha256Hex)
import Valiant.Plugin.Traverse (QueryFileCall (..), findQueryFileCalls)
import Valiant.Plugin.Verify (verifyQueryFile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Data.List (sort)
import Control.Monad.IO.Class (liftIO)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import System.FilePath.Posix (makeRelative)

-- | The valiant GHC source plugin.
plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = valiantRewrite
    , typeCheckResultAction = valiantTypeCheck
    , pluginRecompile = valiantRecompile
    }

-- | Before typechecking, rewrite queryFile calls to mkStatement calls.
valiantRewrite :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
valiantRewrite opts _modSummary parsedResult = do
  let config = parseOptions opts
      hpm = parsedResultModule parsedResult
      L loc hsmod = hpm_module hpm
  (hsmod', _anyRewritten) <- rewriteModule config hsmod
  let hpm' = hpm {hpm_module = L loc hsmod'}
  pure parsedResult {parsedResultModule = hpm'}

-- | After typechecking, walk the AST and validate queryFile calls.
valiantTypeCheck :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
valiantTypeCheck opts _modSummary tcEnv = do
  config <- liftIO $ resolveConfig opts
  let calls = findQueryFileCalls tcEnv

  forM_ calls $ \call -> do
    processQueryFileCall config call

  pure tcEnv

-- | Process a single queryFile call: validate the file, load cache, verify types.
-- In offline mode, skip all validation (only register file dependencies).
processQueryFileCall :: PluginConfig -> QueryFileCall -> TcM ()
processQueryFileCall config call
  | pcOffline config = do
      -- Offline mode: skip all validation. The rewrite phase already handles
      -- missing cache files gracefully. Just register the dependency if the
      -- file exists so GHC recompiles when it appears.
      let sqlPath = pcSqlDir config </> qfcFilePath call
      sqlExists <- liftIO $ doesFileExist sqlPath
      if sqlExists
        then addFileDependency sqlPath
        else pure ()
  | otherwise = do
      let sqlDir = pcSqlDir config
          cacheDir = pcCacheDir config
          relPath = qfcFilePath call
          sqlPath = sqlDir </> relPath
          srcSpan = qfcSrcSpan call

      -- 1. Check that the .sql file exists
      sqlExists <- liftIO $ doesFileExist sqlPath
      if not sqlExists
        then do
          suggestions <- liftIO $ findSimilarFiles sqlDir relPath
          errSqlFileNotFound srcSpan relPath suggestions
        else do
          -- 2. Register as a file dependency for recompilation
          addFileDependency sqlPath

          -- 3. Read and hash the SQL content
          sqlContent <- liftIO $ BS.readFile sqlPath
          let sqlHash = sha256Hex sqlContent

          -- 4. Find the matching cache file
          mCacheMeta <- liftIO $ findCacheFile cacheDir relPath sqlHash
          case mCacheMeta of
            Nothing ->
              errCacheStaleOrMissing srcSpan relPath
            Just entry ->
              -- 5. Verify types
              verifyQueryFile call entry

-- | Determine recompilation based on .valiant/ cache directory fingerprint.
valiantRecompile :: [CommandLineOption] -> IO PluginRecompile
valiantRecompile opts = do
  let config = parseOptions opts
      cacheDir = pcCacheDir config
  exists <- doesDirectoryExist cacheDir
  if not exists
    then pure ForceRecompile
    else do
      files <- sort <$> listDirectory cacheDir
      let fingerprint = fingerprintFingerprints [fingerprintString f | f <- files]
      pure (MaybeRecompile fingerprint)

-- | Find similar .sql files for "did you mean" suggestions.
findSimilarFiles :: FilePath -> FilePath -> IO [FilePath]
findSimilarFiles sqlDir target = do
  exists <- doesDirectoryExist sqlDir
  if not exists
    then pure []
    else do
      allFiles <- findSqlFiles sqlDir
      let distances = [(f, levenshteinDistance defaultEditCosts target f) | f <- allFiles]
          sorted = map fst $ filter (\(_, d) -> d <= 5) $ sortOn snd distances
      pure (take 3 sorted)
  where
    sortOn f = map snd . sort . map (\x -> (f x, x))

findSqlFiles :: FilePath -> IO [FilePath]
findSqlFiles dir = do
  entries <- listDirectory dir
  let go acc [] = pure acc
      go acc (e : es) = do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        if isDir
          then do
            subFiles <- findSqlFiles path
            go (acc ++ subFiles) es
          else
            if ".sql" `isSuffixOf` e
              then go (makeRelative dir path : acc) es
              else go acc es
  go [] entries
  where
    isSuffixOf suffix s = drop (length s - length suffix) s == suffix
