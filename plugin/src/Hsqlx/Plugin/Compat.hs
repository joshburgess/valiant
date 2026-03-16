-- | GHC 9.4 compatibility layer for plugin diagnostics.
module Hsqlx.Plugin.Compat
  ( emitPluginError
  , addFileDependency
  ) where

import GHC.Plugins (SDoc)
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Utils.Monad (TcM, addErrAt, addDependentFiles)
import GHC.Types.Error (mkPlainError, noHints)
import GHC.Types.SrcLoc (SrcSpan)

-- | Emit a compile error from the plugin at a specific source location.
emitPluginError :: SrcSpan -> SDoc -> TcM ()
emitPluginError srcSpan msg =
  addErrAt srcSpan (TcRnUnknownMessage (mkPlainError noHints msg))

-- | Register a file as a dependency for recompilation tracking.
addFileDependency :: FilePath -> TcM ()
addFileDependency path = addDependentFiles [path]
