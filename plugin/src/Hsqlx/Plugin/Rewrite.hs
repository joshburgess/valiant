{-# LANGUAGE CPP #-}

-- | Parsed-AST rewrite: replace @queryFile "path.sql"@ with @mkStatement ...@.
module Hsqlx.Plugin.Rewrite
  ( rewriteModule
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Driver.Env.Types (Hsc)
import GHC.Data.FastString (fsLit, unpackFS)
import GHC.Hs
import GHC.Plugins (GenLocated (..), Outputable, ppr, showSDocUnsafe)
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Unit.Types (IsBootInterface (..))
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrQual, rdrNameOcc)
import GHC.Types.SourceText (SourceText (..), mkIntegralLit)
import Hsqlx.Plugin.Cache (CacheColumn (..), CacheEntry (..), CacheParam (..), findCacheFile, findCacheBySqlHash)
import Hsqlx.Plugin.Config (PluginConfig (..))
import Hsqlx.Plugin.Hash (sha256Hex)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Rewrite a parsed module, replacing queryFile calls with mkStatement calls.
-- Returns the modified module and whether any rewrites were made.
rewriteModule :: PluginConfig -> HsModule GhcPs -> Hsc (HsModule GhcPs, Bool)
rewriteModule config hsmod = do
  (decls', anyRewritten) <- rewriteDecls config (hsmodDecls hsmod)
  let hsmod' =
        if anyRewritten
          then hsmod
            { hsmodDecls = decls'
            , hsmodImports = addImport (stripQueryFileImports (hsmodImports hsmod))
            }
          else hsmod
  pure (hsmod', anyRewritten)

-- | Rewrite all declarations, tracking whether any rewrites occurred.
rewriteDecls :: PluginConfig -> [LHsDecl GhcPs] -> Hsc ([LHsDecl GhcPs], Bool)
rewriteDecls config decls = do
  results <- mapM (rewriteDecl config) decls
  let decls' = map fst results
      anyRewritten = any snd results
  pure (decls', anyRewritten)

rewriteDecl :: PluginConfig -> LHsDecl GhcPs -> Hsc (LHsDecl GhcPs, Bool)
rewriteDecl config (L loc (ValD x bind)) = do
  (bind', rewritten) <- rewriteBind config bind
  pure (L loc (ValD x bind'), rewritten)
rewriteDecl _ decl = pure (decl, False)

rewriteBind :: PluginConfig -> HsBind GhcPs -> Hsc (HsBind GhcPs, Bool)
rewriteBind config fb@FunBind {fun_matches = mg} = do
  (mg', rewritten) <- rewriteMatchGroup config mg
  pure (fb {fun_matches = mg'}, rewritten)
rewriteBind _ bind = pure (bind, False)

rewriteMatchGroup :: PluginConfig -> MatchGroup GhcPs (LHsExpr GhcPs) -> Hsc (MatchGroup GhcPs (LHsExpr GhcPs), Bool)
rewriteMatchGroup config mg = case mg_alts mg of
  L altsLoc alts -> do
    results <- mapM (rewriteLMatch config) alts
    let alts' = map fst results
        anyRewritten = any snd results
    pure (mg {mg_alts = L altsLoc alts'}, anyRewritten)

rewriteLMatch :: PluginConfig -> LMatch GhcPs (LHsExpr GhcPs) -> Hsc (LMatch GhcPs (LHsExpr GhcPs), Bool)
rewriteLMatch config (L loc match) = do
  (grhss', rewritten) <- rewriteGRHSs config (m_grhss match)
  pure (L loc match {m_grhss = grhss'}, rewritten)

rewriteGRHSs :: PluginConfig -> GRHSs GhcPs (LHsExpr GhcPs) -> Hsc (GRHSs GhcPs (LHsExpr GhcPs), Bool)
rewriteGRHSs config grhss = do
  results <- mapM (rewriteGRHS config) (grhssGRHSs grhss)
  let grhss' = map fst results
      anyRewritten = any snd results
  pure (grhss {grhssGRHSs = grhss'}, anyRewritten)

rewriteGRHS :: PluginConfig -> LGRHS GhcPs (LHsExpr GhcPs) -> Hsc (LGRHS GhcPs (LHsExpr GhcPs), Bool)
rewriteGRHS config (L loc (GRHS x guards body)) = do
  (body', rewritten) <- rewriteLExpr config body
  pure (L loc (GRHS x guards body'), rewritten)

rewriteLExpr :: PluginConfig -> LHsExpr GhcPs -> Hsc (LHsExpr GhcPs, Bool)
rewriteLExpr config (L loc expr) = do
  (expr', rewritten) <- rewriteExpr config expr
  pure (L loc expr', rewritten)

rewriteExpr :: PluginConfig -> HsExpr GhcPs -> Hsc (HsExpr GhcPs, Bool)
rewriteExpr config (HsApp appAnn (L fLoc func) (L aLoc arg))
  -- queryFile "path.sql" — load SQL from file, look up cache by path+hash
  | isQueryFileRdr func
  , Just path <- extractParsedStringLit arg = do
      mEntry <- liftIO $ loadCacheForPath config path
      case mEntry of
        Just entry -> do
          let replacement = buildMkStatementCall entry
          pure (replacement, True)
        Nothing -> do
          let placeholder = buildPlaceholderCall path
          pure (placeholder, True)
  -- query "SELECT ..." — inline SQL, look up cache by SQL hash directly
  | isQueryInlineRdr func
  , Just sqlText <- extractParsedStringLit arg = do
      mEntry <- liftIO $ loadCacheForInlineSql config sqlText
      case mEntry of
        Just entry -> do
          let replacement = buildMkStatementCall entry
          pure (replacement, True)
        Nothing -> do
          -- No cache for this SQL text. Use a placeholder that includes
          -- the SQL itself as the "path" so the typecheck phase can report it.
          let placeholder = buildPlaceholderInline sqlText
          pure (placeholder, True)
rewriteExpr _ expr = pure (expr, False)

-- Builders ----------------------------------------------------------------

-- | Build a placeholder @mkStatement "" [] [] path@ for when cache is missing.
-- This avoids the HsqlxPluginRequired TypeError while letting the typecheck
-- phase detect the call and emit a proper HSQLX-001/002 error.
buildPlaceholderCall :: String -> HsExpr GhcPs
buildPlaceholderCall path =
  let mkSt = mkQualVar "Hsqlx.Statement" "mkStatement"
      sql = mkStrLit ""
      oids = mkIntList []
      cols = mkStrList []
      pathLit = mkStrLit path
   in unLoc (mkSt `app` sql `app` oids `app` cols `app` pathLit)

-- | Build a placeholder for inline SQL when cache is missing.
buildPlaceholderInline :: String -> HsExpr GhcPs
buildPlaceholderInline sqlText =
  let mkSt = mkQualVar "Hsqlx.Statement" "mkStatement"
      sql = mkStrLit sqlText
      oids = mkIntList []
      cols = mkStrList []
      pathLit = mkStrLit "<inline>"
   in unLoc (mkSt `app` sql `app` oids `app` cols `app` pathLit)

-- | Build: @Hsqlx.Statement.mkStatement sqlStr oids colNames path@
buildMkStatementCall :: CacheEntry -> HsExpr GhcPs
buildMkStatementCall entry =
  let mkSt = mkQualVar "Hsqlx.Statement" "mkStatement"
      sql = mkStrLit (T.unpack (ceSql entry))
      oids = mkIntList [fromIntegral (cpPgOid p) | p <- ceParams entry]
      cols = mkStrList [T.unpack (ccName c) | c <- ceColumns entry]
      path = mkStrLit (ceFile entry)
   in unLoc (mkSt `app` sql `app` oids `app` cols `app` path)

mkQualVar :: String -> String -> LHsExpr GhcPs
mkQualVar modName varName =
  noLocA $ HsVar noExtField (noLocA (mkRdrQual (mkModuleName modName) (mkVarOcc varName)))

mkStrLit :: String -> LHsExpr GhcPs
mkStrLit s = noLocA $ HsLit noExtField (HsString NoSourceText (fsLit s))

mkIntList :: [Int] -> LHsExpr GhcPs
mkIntList xs = noLocA $ ExplicitList noAnn [mkIntLit (fromIntegral x) | x <- xs]

mkIntLit :: Integer -> LHsExpr GhcPs
mkIntLit n = noLocA $ HsOverLit noExtField (mkHsIntegral (mkIntegralLit n))

mkStrList :: [String] -> LHsExpr GhcPs
mkStrList xs = noLocA $ ExplicitList noAnn [mkStrLit s | s <- xs]

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app f x = noLocA $ HsApp noExtField f x

unLoc :: GenLocated l a -> a
unLoc (L _ a) = a

-- Helpers -----------------------------------------------------------------

isQueryFileRdr :: HsExpr GhcPs -> Bool
isQueryFileRdr (HsVar _ (L _ rdr)) =
  let s = occNameString (rdrNameOcc rdr)
   in s == "queryFile" || s == "queryFileAs"
isQueryFileRdr _ = False

isQueryInlineRdr :: HsExpr GhcPs -> Bool
isQueryInlineRdr (HsVar _ (L _ rdr)) =
  occNameString (rdrNameOcc rdr) == "query"
isQueryInlineRdr _ = False

extractParsedStringLit :: HsExpr GhcPs -> Maybe String
extractParsedStringLit (HsLit _ (HsString _ fs)) = Just (unpackFS fs)
extractParsedStringLit _ = Nothing

loadCacheForPath :: PluginConfig -> String -> IO (Maybe CacheEntry)
loadCacheForPath config path = do
  let sqlPath = pcSqlDir config </> path
  exists <- doesFileExist sqlPath
  if not exists
    then pure Nothing
    else do
      content <- BS.readFile sqlPath
      let hash = sha256Hex content
      findCacheFile (pcCacheDir config) path hash

loadCacheForInlineSql :: PluginConfig -> String -> IO (Maybe CacheEntry)
loadCacheForInlineSql config sqlText = do
  let sqlBs = TE.encodeUtf8 (T.pack sqlText)
      hash = sha256Hex sqlBs
  findCacheBySqlHash (pcCacheDir config) hash

-- | Remove @queryFile@ and @queryFileAs@ from explicit import lists.
-- After the plugin rewrites these calls to @mkStatement@, the imports
-- would be redundant and trigger @-Wunused-imports@.
-- Uses 'showPpr' to convert IE items to strings for robust matching
-- across GHC versions.
stripQueryFileImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
stripQueryFileImports = map stripImportDecl
  where
    stripImportDecl (L loc decl) = case ideclImportList decl of
      Just (listType, L listLoc ies) ->
        let ies' = filter (not . isQueryFileIE) ies
         in L loc decl { ideclImportList = Just (listType, L listLoc ies') }
      Nothing -> L loc decl

    isQueryFileIE :: LIE GhcPs -> Bool
    isQueryFileIE (L _ ie) =
      let s = showPprUnsafe ie
       in s == "queryFile" || s == "queryFileAs" || s == "query"

    showPprUnsafe :: (Outputable a) => a -> String
    showPprUnsafe = showSDocUnsafe . ppr

-- | Add @import qualified Hsqlx.Statement@ (implicit) to the import list.
addImport :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
addImport imports = mkStatementImport : imports

mkStatementImport :: LImportDecl GhcPs
mkStatementImport =
  noLocA
    ImportDecl
      { ideclExt = XImportDeclPass noAnn NoSourceText True
      , ideclName = noLocA (mkModuleName "Hsqlx.Statement")
      , ideclPkgQual = NoRawPkgQual
      , ideclSource = NotBoot
      , ideclSafe = False
      , ideclQualified = QualifiedPre
      , ideclAs = Nothing
      , ideclImportList = Nothing
      }
