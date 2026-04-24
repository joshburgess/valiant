module Valiant.CLI.CustomTypesSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import PgWire.Protocol.Oid (Oid (..))
import Valiant.CLI.CustomTypes (loadCustomTypes)
import Valiant.CLI.TypeMap (HaskellType (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "loadCustomTypes" $ do
    it "returns empty map when file does not exist" $ do
      customs <- loadCustomTypes "/nonexistent/valiant-types.json"
      customs `shouldBe` Map.empty

    it "loads custom type mappings from JSON file" $ do
      withSystemTempDirectory "valiant-test" $ \dir -> do
        let path = dir <> "/valiant-types.json"
            content =
              encode
                [ object
                    [ "oid" .= (600 :: Int)
                    , "pg_type_name" .= ("point" :: String)
                    , "haskell_type" .= ("MyPoint" :: String)
                    , "haskell_module" .= ("MyApp.Types" :: String)
                    ]
                ]
        LBS.writeFile path content
        customs <- loadCustomTypes path
        Map.lookup (Oid 600) customs `shouldBe` Just (HaskellType "MyPoint" "MyApp.Types")

    it "returns empty map on invalid JSON" $ do
      withSystemTempDirectory "valiant-test" $ \dir -> do
        let path = dir <> "/valiant-types.json"
        writeFile path "not valid json"
        customs <- loadCustomTypes path
        customs `shouldBe` Map.empty

    it "loads multiple entries" $ do
      withSystemTempDirectory "valiant-test" $ \dir -> do
        let path = dir <> "/valiant-types.json"
            content =
              encode
                [ object
                    [ "oid" .= (600 :: Int)
                    , "pg_type_name" .= ("point" :: String)
                    , "haskell_type" .= ("MyPoint" :: String)
                    , "haskell_module" .= ("MyApp.Types" :: String)
                    ]
                , object
                    [ "oid" .= (601 :: Int)
                    , "pg_type_name" .= ("lseg" :: String)
                    , "haskell_type" .= ("MySegment" :: String)
                    , "haskell_module" .= ("MyApp.Types" :: String)
                    ]
                ]
        LBS.writeFile path content
        customs <- loadCustomTypes path
        Map.size customs `shouldBe` 2
