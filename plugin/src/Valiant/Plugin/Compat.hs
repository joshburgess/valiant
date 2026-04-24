{-# LANGUAGE CPP #-}

-- | GHC version compatibility layer for the valiant plugin.
--
-- Abstracts over GHC API changes between 9.6, 9.8, and 9.10.
module Valiant.Plugin.Compat
  ( emitPluginError
  , addFileDependency
  ) where

import GHC.Plugins (SDoc)
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Utils.Monad (TcM, addErrAt, addDependentFiles)
import GHC.Types.SrcLoc (SrcSpan)

#if MIN_VERSION_ghc(9, 8, 0)
import GHC.Types.Error (mkSimpleUnknownDiagnostic, noHints, mkPlainError)
#else
import GHC.Types.Error (mkPlainError, noHints)
#endif

-- | Emit a compile error from the plugin at a specific source location.
emitPluginError :: SrcSpan -> SDoc -> TcM ()
#if MIN_VERSION_ghc(9, 8, 0)
-- GHC 9.8+: requires mkSimpleUnknownDiagnostic wrapper
emitPluginError srcSpan msg =
  addErrAt srcSpan (TcRnUnknownMessage (mkSimpleUnknownDiagnostic (mkPlainError noHints msg)))
#else
-- GHC 9.6: TcRnUnknownMessage takes the diagnostic directly
emitPluginError srcSpan msg =
  addErrAt srcSpan (TcRnUnknownMessage (mkPlainError noHints msg))
#endif

-- | Register a file as a dependency for recompilation tracking.
addFileDependency :: FilePath -> TcM ()
addFileDependency path = addDependentFiles [path]
