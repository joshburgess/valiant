-- | Parsed-AST rewrite: replace @queryFile "path.sql"@ with @mkStatement ...@.
module Hsqlx.Plugin.Rewrite
  ( rewriteModule
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Driver.Env.Types (Hsc)
import GHC.Data.FastString (fsLit, unpackFS)
import GHC.Hs
import GHC.Parser.Annotation (noAnn, noLocA)
import GHC.Plugins (GenLocated (..))
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Hs.ImpExp (ImportDeclQualifiedStyle (..))
import GHC.Unit.Types (IsBootInterface (..))
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrQual, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..), SourceText (..), mkIntegralLit)
import GHC.Unit.Module.Name (mkModuleName)
import Hsqlx.Plugin.Cache (CacheColumn (..), CacheEntry (..), CacheParam (..), findCacheFile)
import Hsqlx.Plugin.Config (PluginConfig (..))
import Hsqlx.Plugin.Hash (sha256Hex)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Rewrite a parsed module, replacing queryFile calls with mkStatement calls.
-- Returns the modified module and whether any rewrites were made.
rewriteModule :: PluginConfig -> HsModule -> Hsc (HsModule, Bool)
rewriteModule config hsmod = do
  (decls', anyRewritten) <- rewriteDecls config (hsmodDecls hsmod)
  let hsmod' =
        if anyRewritten
          then hsmod {hsmodDecls = decls', hsmodImports = addImport (hsmodImports hsmod)}
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
  | isQueryFileRdr func
  , Just path <- extractParsedStringLit arg = do
      mEntry <- liftIO $ loadCacheForPath config path
      case mEntry of
        Just entry -> do
          let replacement = buildMkStatementCall entry
          pure (replacement, True)
        Nothing ->
          -- Cache not found; leave queryFile as-is.
          -- typeCheckResultAction will emit HSQLX-002.
          pure (HsApp appAnn (L fLoc func) (L aLoc arg), False)
rewriteExpr _ expr = pure (expr, False)

-- Builders ----------------------------------------------------------------

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
mkStrLit s = noLocA $ HsLit noAnn (HsString NoSourceText (fsLit s))

mkIntList :: [Int] -> LHsExpr GhcPs
mkIntList xs = noLocA $ ExplicitList noAnn [mkIntLit (fromIntegral x) | x <- xs]

mkIntLit :: Integer -> LHsExpr GhcPs
mkIntLit n = noLocA $ HsOverLit noAnn (mkHsIntegral (mkIntegralLit n))

mkStrList :: [String] -> LHsExpr GhcPs
mkStrList xs = noLocA $ ExplicitList noAnn [mkStrLit s | s <- xs]

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app f x = noLocA $ HsApp noAnn f x

unLoc :: GenLocated l a -> a
unLoc (L _ a) = a

-- Helpers -----------------------------------------------------------------

isQueryFileRdr :: HsExpr GhcPs -> Bool
isQueryFileRdr (HsVar _ (L _ rdr)) =
  let s = occNameString (rdrNameOcc rdr)
   in s == "queryFile" || s == "queryFileAs"
isQueryFileRdr _ = False

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

-- | Add @import qualified Hsqlx.Statement (mkStatement)@ to the import list.
addImport :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
addImport imports = mkStatementImport : imports

mkStatementImport :: LImportDecl GhcPs
mkStatementImport =
  noLocA
    ImportDecl
      { ideclExt = noAnn
      , ideclSourceSrc = NoSourceText
      , ideclName = noLocA (mkModuleName "Hsqlx.Statement")
      , ideclPkgQual = NoRawPkgQual
      , ideclSource = NotBoot
      , ideclSafe = False
      , ideclQualified = QualifiedPre
      , ideclImplicit = False
      , ideclAs = Nothing
      , ideclHiding = Nothing
      }
