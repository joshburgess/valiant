-- | Walk the typechecked AST to find queryFile/queryFileAs/mkStatement call sites.
module Hsqlx.Plugin.Traverse
  ( QueryFileCall (..)
  , findQueryFileCalls
  ) where

import GHC.Hs
import GHC.Plugins
  ( Var, idType, varName, nameOccName, occNameString
  , GenLocated(..), SrcSpan
  )
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Core.Type (Type)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (unpackFS)

-- | A located queryFile call extracted from the AST.
data QueryFileCall = QueryFileCall
  { qfcSrcSpan :: SrcSpan
  , qfcFilePath :: String
  , qfcBindType :: Type
  , qfcBindName :: String
  }

-- | Find all queryFile/queryFileAs/mkStatement calls in the typechecked module.
-- After the parse-phase rewrite, queryFile calls become mkStatement calls.
-- We detect both forms to handle modules compiled with and without rewriting.
findQueryFileCalls :: TcGblEnv -> [QueryFileCall]
findQueryFileCalls env =
  concatMap findInBind (bagToList (tcg_binds env))

findInBind :: GenLocated l (HsBindLR GhcTc GhcTc) -> [QueryFileCall]
findInBind (L _ (FunBind {fun_id = L _ var, fun_matches = matches})) =
  let bindName = occNameString (nameOccName (varName var))
      bindType = idType var
      calls = findCallsInMG matches
   in [ QueryFileCall sp path bindType bindName | (sp, path) <- calls ]
findInBind (L _ (XHsBindsLR (AbsBinds {abs_binds = binds}))) =
  concatMap findInBind (bagToList binds)
findInBind (L _ (VarBind {var_id = var, var_rhs = rhs})) =
  let bindName = occNameString (nameOccName (varName var))
      bindType = idType var
      calls = findCallsInLExpr rhs
   in [ QueryFileCall sp path bindType bindName | (sp, path) <- calls ]
findInBind _ = []

findCallsInMG :: MatchGroup GhcTc (LHsExpr GhcTc) -> [(SrcSpan, String)]
findCallsInMG mg = case mg_alts mg of
  L _ alts -> concatMap (\(L _ m) -> findCallsInMatch m) alts

findCallsInMatch :: Match GhcTc (LHsExpr GhcTc) -> [(SrcSpan, String)]
findCallsInMatch Match {m_grhss = grhss} =
  concatMap (\(L _ g) -> findCallsInGRHS g) (grhssGRHSs grhss)

findCallsInGRHS :: GRHS GhcTc (LHsExpr GhcTc) -> [(SrcSpan, String)]
findCallsInGRHS (GRHS _ _ body) = findCallsInLExpr body

findCallsInLExpr :: LHsExpr GhcTc -> [(SrcSpan, String)]
findCallsInLExpr (L ann expr) = findCallsInExpr (getLocA (L ann expr)) expr

findCallsInExpr :: SrcSpan -> HsExpr GhcTc -> [(SrcSpan, String)]
findCallsInExpr sp expr =
  -- First, try to detect mkStatement (rewritten queryFile)
  case extractMkStatementCall expr of
    Just path -> [(sp, path)]
    Nothing -> case expr of
      -- Original queryFile "path.sql" (not rewritten)
      HsApp _ (L _ func) (L _ arg)
        | isQueryFileExpr func
        , Just path <- extractStringLit arg ->
            [(sp, path)]
      HsApp _ func arg ->
        findCallsInLExpr func ++ findCallsInLExpr arg
      OpApp _ l op r ->
        findCallsInLExpr l ++ findCallsInLExpr op ++ findCallsInLExpr r
      NegApp _ e _ -> findCallsInLExpr e
      HsPar _ e -> findCallsInLExpr e
      SectionL _ e1 e2 -> findCallsInLExpr e1 ++ findCallsInLExpr e2
      SectionR _ e1 e2 -> findCallsInLExpr e1 ++ findCallsInLExpr e2
      ExplicitTuple _ args _ ->
        concatMap (\case Present _ e -> findCallsInLExpr e; _ -> []) args
      HsLet _ _ e -> findCallsInLExpr e
      HsCase _ scrut mg ->
        findCallsInLExpr scrut ++ findCallsInMG mg
      HsIf _ c t f ->
        findCallsInLExpr c ++ findCallsInLExpr t ++ findCallsInLExpr f
      HsDo _ _ (L _ stmts) -> concatMap (\(L _ s) -> findCallsInStmt s) stmts
      ExplicitList _ exprs -> concatMap findCallsInLExpr exprs
      XExpr (WrapExpr (HsWrap _ inner)) -> findCallsInExpr sp inner
      XExpr (ExpandedThingTc _ inner) -> findCallsInExpr sp inner
      _ -> []

-- | Try to extract the file path from a mkStatement call.
-- After rewrite, the expression is:
--   mkStatement sqlStr oidsLit colsLit pathStr
-- Which in the AST is a chain of HsApp:
--   (((mkStatement `app` sql) `app` oids) `app` cols) `app` path
-- We collect all args from a left-nested application spine,
-- check the head is mkStatement, and take the 4th arg as the path.
extractMkStatementCall :: HsExpr GhcTc -> Maybe String
extractMkStatementCall expr = do
  let (f, args) = collectArgs expr
  if isMkStatementExpr f && length args == 4
    then extractStringLit (unLHsExpr (args !! 3))
    else Nothing

-- | Collect the function and arguments from a left-nested application spine.
-- (((f a1) a2) a3) → (f, [a1, a2, a3])
-- Uses reverse-accumulate internally to avoid O(n²) from repeated (++).
collectArgs :: HsExpr GhcTc -> (HsExpr GhcTc, [LHsExpr GhcTc])
collectArgs = go []
  where
    go !acc (HsApp _ (L _ f) arg) = go (arg : acc) f
    go !acc (XExpr (WrapExpr (HsWrap _ inner))) = go acc inner
    go !acc other = (other, acc) -- acc is already in correct order
    -- because we prepend as we peel from the outside in:
    -- (((f a1) a2) a3) → go [] expr → go [a3] (f a1 a2) → go [a2,a3] (f a1) → go [a1,a2,a3] f

unLHsExpr :: LHsExpr GhcTc -> HsExpr GhcTc
unLHsExpr (L _ e) = e

isMkStatementExpr :: HsExpr GhcTc -> Bool
isMkStatementExpr (HsVar _ (L _ var)) = isMkStatementName var
isMkStatementExpr (XExpr (WrapExpr (HsWrap _ inner))) = isMkStatementExpr inner
isMkStatementExpr _ = False

isMkStatementName :: Var -> Bool
isMkStatementName var =
  occNameString (nameOccName (varName var)) == "mkStatement"

findCallsInStmt :: StmtLR GhcTc GhcTc (LHsExpr GhcTc) -> [(SrcSpan, String)]
findCallsInStmt (BodyStmt _ body _ _) = findCallsInLExpr body
findCallsInStmt (BindStmt _ _ body) = findCallsInLExpr body
findCallsInStmt _ = []

isQueryFileExpr :: HsExpr GhcTc -> Bool
isQueryFileExpr (HsVar _ (L _ var)) = isQueryFileName var
isQueryFileExpr (XExpr (WrapExpr (HsWrap _ inner))) = isQueryFileExpr inner
isQueryFileExpr _ = False

isQueryFileName :: Var -> Bool
isQueryFileName var =
  let occ = occNameString (nameOccName (varName var))
   in occ == "queryFile" || occ == "queryFileAs"

extractStringLit :: HsExpr GhcTc -> Maybe String
extractStringLit (HsLit _ (HsString _ fs)) = Just (unpackFS fs)
extractStringLit (HsOverLit _ (OverLit _ (HsIsString _ fs))) = Just (unpackFS fs)
extractStringLit (XExpr (WrapExpr (HsWrap _ inner))) = extractStringLit inner
extractStringLit _ = Nothing
