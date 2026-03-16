module Hsqlx.CLI.CustomTypes
  ( CustomTypeMap
  , loadCustomTypes
  , customTypesFile
  ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Database.PostgreSQL.LibPQ (Oid (..))
import Foreign.C.Types (CUInt)
import Hsqlx.CLI.TypeMap (HaskellType (..))
import System.Directory (doesFileExist)

-- | User-defined OID → Haskell type mappings.
type CustomTypeMap = Map Oid HaskellType

-- | Default file name for custom type mappings.
customTypesFile :: FilePath
customTypesFile = "hsqlx-types.json"

-- | Load custom type mappings from a JSON file if it exists.
-- Returns an empty map if the file is absent.
--
-- Expected format:
--
-- > [
-- >   { "oid": 600, "pg_type_name": "point", "haskell_type": "MyPoint", "haskell_module": "MyApp.Types" },
-- >   { "oid": 16389, "pg_type_name": "mood", "haskell_type": "Text", "haskell_module": "Data.Text" }
-- > ]
loadCustomTypes :: FilePath -> IO CustomTypeMap
loadCustomTypes path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      bytes <- LBS.readFile path
      case eitherDecode bytes of
        Left _ -> pure Map.empty
        Right entries -> pure (toMap entries)

toMap :: [CustomTypeEntry] -> CustomTypeMap
toMap = Map.fromList . map toPair
  where
    toPair CustomTypeEntry {..} =
      (Oid (fromIntegral cteOid :: CUInt), HaskellType cteHaskellType cteHaskellModule)

data CustomTypeEntry = CustomTypeEntry
  { cteOid :: Word32
  , ctePgTypeName :: Text
  , cteHaskellType :: Text
  , cteHaskellModule :: Text
  }

instance FromJSON CustomTypeEntry where
  parseJSON = withObject "CustomTypeEntry" $ \o ->
    CustomTypeEntry
      <$> o .: "oid"
      <*> o .: "pg_type_name"
      <*> o .: "haskell_type"
      <*> o .: "haskell_module"
