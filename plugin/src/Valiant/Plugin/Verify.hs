-- | Verify that user type signatures match cached query metadata.
module Valiant.Plugin.Verify
  ( verifyQueryFile
  ) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Core.TyCon qualified as TyCon
import GHC.Core.Type (Type, splitTyConApp_maybe)
import GHC.Plugins (showSDocUnsafe, ppr, TyCon, tyConName, nameOccName, occNameString)
import GHC.Tc.Utils.Monad (TcM)
import GHC.Types.SrcLoc (SrcSpan)
import Valiant.Plugin.Cache (CacheColumn (..), CacheEntry (..), CacheParam (..))
import Valiant.Plugin.Errors
import Valiant.Plugin.Traverse (QueryFileCall (..))

-- | Verify a queryFile call against its cache entry.
-- Emits compile errors if the types don't match.
verifyQueryFile :: QueryFileCall -> CacheEntry -> TcM ()
verifyQueryFile call entry = do
  let bindTy = qfcBindType call
      srcSpan = qfcSrcSpan call
      path = ceFile entry

  -- Try to decompose the type as Statement p r
  case decomposeStatementType bindTy of
    Nothing ->
      -- The binding type is not `Statement p r`. This happens when:
      --   1. The user wrote a type hole `_` — GHC will infer the type from
      --      mkStatement's constraints, which is correct behavior.
      --   2. No type signature was given — GHC infers from the rewritten
      --      mkStatement call, which already encodes the correct types.
      --   3. The type is something other than Statement entirely — this will
      --      be caught by GHC's own typechecker when it tries to unify with
      --      mkStatement's return type, so we don't need to emit our own error.
      -- In all cases, skipping plugin validation is the right thing to do.
      pure ()
    Just (paramTy, resultTy) -> do
      -- Verify parameter types
      verifyParams srcSpan path paramTy (ceParams entry)
      -- Verify result types
      verifyResult srcSpan path resultTy (ceColumns entry)

-- | Decompose a type into (paramType, resultType) if it's Statement p r.
decomposeStatementType :: Type -> Maybe (Type, Type)
decomposeStatementType ty = do
  (tyCon, args) <- splitTyConApp_maybe ty
  let tcName = occNameString (nameOccName (tyConName tyCon))
  case args of
    [a, b] | tcName == "Statement" -> Just (a, b)
    _ -> Nothing

-- | Verify parameter types match.
verifyParams :: SrcSpan -> FilePath -> Type -> [CacheParam] -> TcM ()
verifyParams srcSpan path paramTy params = do
  let expectedArity = length params
      actualTypes = decomposeType paramTy
      actualArity = length actualTypes

  -- Check arity
  if expectedArity == 0 && isUnitType paramTy
    then pure () -- () matches 0 params
    else
      if actualArity /= expectedArity
        then errParamCountMismatch srcSpan path expectedArity actualArity params
        else do
          -- Check individual types
          let expected = map cpHaskellType params
              actual = map typeToText actualTypes
          mapM_ (checkParamType srcSpan path) (zip3 [1 ..] expected actual)

checkParamType :: SrcSpan -> FilePath -> (Int, Text, Text) -> TcM ()
checkParamType srcSpan path (idx, expected, actual)
  | matchesParamTypeText expected actual = pure ()
  | otherwise = errParamTypeMismatch srcSpan path idx expected actual

-- | Verify result types match.
verifyResult :: SrcSpan -> FilePath -> Type -> [CacheColumn] -> TcM ()
verifyResult srcSpan path resultTy cols = do
  let expectedArity = length cols
      actualTypes = decomposeType resultTy
      actualArity = length actualTypes

  if expectedArity == 0 && isUnitType resultTy
    then pure ()
    else
      if actualArity /= expectedArity
        then errColumnCountMismatch srcSpan path expectedArity actualArity cols
        else do
          let mismatches =
                [ (c, actual, matchesTypeText (ccHaskellType c) actual)
                | (c, ty) <- zip cols actualTypes
                , let actual = typeToText ty
                ]
          if all (\(_, _, ok) -> ok) mismatches
            then pure ()
            else errResultTypeMismatch srcSpan path mismatches

-- Helpers -----------------------------------------------------------------

-- | Decompose a type into its constituent parts.
-- Unit type () -> []
-- Single type T -> [T]
-- Tuple (A, B, C) -> [A, B, C]
decomposeType :: Type -> [Type]
decomposeType ty = case splitTyConApp_maybe ty of
  Just (tc, args)
    | isTupleTyCon tc -> args
    | isUnitTyCon tc -> []
  _ -> [ty] -- single type

-- | Convert a GHC Type to a text representation for comparison.
typeToText :: Type -> Text
typeToText ty = T.pack (showSDocUnsafe (ppr ty))

-- | Check if a type is the unit type ().
isUnitType :: Type -> Bool
isUnitType ty = case splitTyConApp_maybe ty of
  Just (tc, []) -> isUnitTyCon tc
  _ -> False

isUnitTyCon :: TyCon -> Bool
isUnitTyCon tc = occNameString (nameOccName (tyConName tc)) == "()"
  || TyCon.isUnboxedTupleTyCon tc && TyCon.tyConArity tc == 0

isTupleTyCon :: TyCon -> Bool
isTupleTyCon = TyCon.isTupleTyCon

-- | Compare expected type text from cache with actual type text from GHC.
-- This does a normalized comparison to handle differences in qualification.
matchesTypeText :: Text -> Text -> Bool
matchesTypeText expected actual =
  normalize expected == normalize actual

-- | Like 'matchesTypeText' but for parameters: allows @Maybe T@ to match @T@.
-- This is valid because Haskell's @Maybe@ encodes as SQL NULL, and Postgres
-- doesn't prevent NULL values in parameters (the column constraint enforces it).
matchesParamTypeText :: Text -> Text -> Bool
matchesParamTypeText expected actual =
  matchesTypeText expected actual
    || matchesTypeText expected (stripMaybe actual)
  where
    stripMaybe t = case T.words (normalize t) of
      ("Maybe" : rest) -> T.unwords rest
      _ -> t

normalize :: Text -> Text
normalize = T.strip . stripQualifiers

stripQualifiers :: Text -> Text
stripQualifiers t =
  -- "Data.Int.Int32" -> "Int32", "Maybe Data.Text.Text" -> "Maybe Text"
  T.unwords [lastPart w | w <- T.words t]
  where
    lastPart w = case NE.nonEmpty (T.splitOn "." w) of
      Nothing    -> w
      Just parts -> NE.last parts
