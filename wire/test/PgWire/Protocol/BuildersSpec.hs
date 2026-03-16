module PgWire.Protocol.BuildersSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32)
import Data.Vector qualified as V
import PgWire.Protocol.Builders (buildFrontendMsg, buildStartup)
import PgWire.Protocol.Frontend
import Test.Hspec

spec :: Spec
spec = do
  describe "zero-payload messages" $ do
    it "Sync is tag 'S' + length 4" $ do
      let bs = buildFrontendMsg Sync
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 83

    it "Terminate is tag 'X' + length 4" $ do
      let bs = buildFrontendMsg Terminate
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 88

    it "Flush is tag 'H' + length 4" $ do
      let bs = buildFrontendMsg Flush
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 72

    it "CopyDone is tag 'c' + length 4" $ do
      let bs = buildFrontendMsg CopyDone
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 99

  describe "Query" $ do
    it "has tag 'Q' and NUL-terminated SQL" $ do
      let bs = buildFrontendMsg (Query "SELECT 1")
      BS.index bs 0 `shouldBe` 81
      BS.last bs `shouldBe` 0
      "SELECT 1" `BS.isInfixOf` bs `shouldBe` True

    it "length includes itself + sql + NUL" $ do
      let bs = buildFrontendMsg (Query "SEL")
      -- tag(1) + len(4) + "SEL"(3) + NUL(1) = 9
      -- length field = 4 + 3 + 1 = 8
      BS.length bs `shouldBe` 9
      let lenBytes = BS.take 4 (BS.drop 1 bs)
      decodeInt32 lenBytes `shouldBe` 8

    it "handles empty query" $ do
      let bs = buildFrontendMsg (Query "")
      -- tag(1) + len(4) + NUL(1) = 6
      BS.length bs `shouldBe` 6

  describe "Parse" $ do
    it "has tag 'P'" $ do
      let bs = buildFrontendMsg (Parse "s1" "SELECT $1" (V.singleton 23))
      BS.index bs 0 `shouldBe` 80

    it "contains statement name and SQL" $ do
      let bs = buildFrontendMsg (Parse "mystmt" "SELECT * FROM t" V.empty)
      "mystmt" `BS.isInfixOf` bs `shouldBe` True
      "SELECT * FROM t" `BS.isInfixOf` bs `shouldBe` True

    it "encodes param count and OIDs" $ do
      let bs = buildFrontendMsg (Parse "" "SELECT $1, $2" (V.fromList [23, 25]))
      -- Should contain the Int16 param count (2) and two Int32 OIDs
      BS.length bs `shouldSatisfy` (> 15)

  describe "Bind" $ do
    it "has tag 'B'" $ do
      let bs = buildFrontendMsg (Bind "" "s1" V.empty V.empty V.empty)
      BS.index bs 0 `shouldBe` 66

    it "encodes portal and statement names" $ do
      let bs = buildFrontendMsg (Bind "portal" "stmt" V.empty V.empty V.empty)
      "portal" `BS.isInfixOf` bs `shouldBe` True
      "stmt" `BS.isInfixOf` bs `shouldBe` True

    it "encodes parameter values including NULLs" $ do
      let params = V.fromList [Just "hello", Nothing, Just "world"]
          bs = buildFrontendMsg (Bind "" "" V.empty params V.empty)
      "hello" `BS.isInfixOf` bs `shouldBe` True
      "world" `BS.isInfixOf` bs `shouldBe` True

  describe "Execute" $ do
    it "has tag 'E'" $ do
      let bs = buildFrontendMsg (Execute "" 0)
      BS.index bs 0 `shouldBe` 69

    it "encodes max rows" $ do
      let bs = buildFrontendMsg (Execute "" 100)
      BS.length bs `shouldSatisfy` (> 5)

  describe "Describe" $ do
    it "has tag 'D' for statement" $ do
      let bs = buildFrontendMsg (Describe DescribeStatement "s1")
      BS.index bs 0 `shouldBe` 68

    it "has tag 'D' for portal" $ do
      let bs = buildFrontendMsg (Describe DescribePortal "p1")
      BS.index bs 0 `shouldBe` 68

  describe "Close" $ do
    it "has tag 'C'" $ do
      let bs = buildFrontendMsg (Close DescribeStatement "s1")
      BS.index bs 0 `shouldBe` 67

  describe "PasswordMessage" $ do
    it "has tag 'p'" $ do
      let bs = buildFrontendMsg (PasswordMessage "secret")
      BS.index bs 0 `shouldBe` 112
      "secret" `BS.isInfixOf` bs `shouldBe` True

  describe "CopyData" $ do
    it "has tag 'd' and preserves payload" $ do
      let bs = buildFrontendMsg (CopyData "row data")
      BS.index bs 0 `shouldBe` 100
      "row data" `BS.isInfixOf` bs `shouldBe` True

  describe "CopyFail" $ do
    it "has tag 'f' and includes error message" $ do
      let bs = buildFrontendMsg (CopyFail "bad data")
      BS.index bs 0 `shouldBe` 102
      "bad data" `BS.isInfixOf` bs `shouldBe` True

  describe "SASLInitialResponse" $ do
    it "has tag 'p'" $ do
      let bs = buildFrontendMsg (SASLInitialResponse "SCRAM-SHA-256" "client-first")
      BS.index bs 0 `shouldBe` 112
      "SCRAM-SHA-256" `BS.isInfixOf` bs `shouldBe` True

  describe "SASLResponse" $ do
    it "has tag 'p'" $ do
      let bs = buildFrontendMsg (SASLResponse "client-final")
      BS.index bs 0 `shouldBe` 112

  describe "buildStartup" $ do
    it "starts with length and protocol 3.0" $ do
      let bs = buildStartup (StartupParams "u" "d" "" [])
      BS.length bs `shouldSatisfy` (> 8)
      let version = decodeInt32 (BS.take 4 (BS.drop 4 bs))
      version `shouldBe` 196608

    it "includes user and database" $ do
      let bs = buildStartup (StartupParams "alice" "mydb" "" [])
      "user" `BS.isInfixOf` bs `shouldBe` True
      "alice" `BS.isInfixOf` bs `shouldBe` True
      "database" `BS.isInfixOf` bs `shouldBe` True
      "mydb" `BS.isInfixOf` bs `shouldBe` True

    it "includes application_name when non-empty" $ do
      let bs = buildStartup (StartupParams "u" "d" "myapp" [])
      "application_name" `BS.isInfixOf` bs `shouldBe` True
      "myapp" `BS.isInfixOf` bs `shouldBe` True

    it "omits application_name when empty" $ do
      let bs = buildStartup (StartupParams "u" "d" "" [])
      "application_name" `BS.isInfixOf` bs `shouldBe` False

    it "includes extra params" $ do
      let bs = buildStartup (StartupParams "u" "d" "" [("extra_float_digits", "2")])
      "extra_float_digits" `BS.isInfixOf` bs `shouldBe` True
      "2" `BS.isInfixOf` bs `shouldBe` True

    it "ends with NUL terminator" $ do
      let bs = buildStartup (StartupParams "u" "d" "" [])
      BS.last bs `shouldBe` 0

    it "length field matches actual length" $ do
      let bs = buildStartup (StartupParams "testuser" "testdb" "" [])
          declaredLen = decodeInt32 (BS.take 4 bs)
      declaredLen `shouldBe` fromIntegral (BS.length bs)

-- Helpers -------------------------------------------------------------------

decodeInt32 :: BS.ByteString -> Int32
decodeInt32 bs =
  let b0 = fromIntegral (BS.index bs 0) :: Int32
      b1 = fromIntegral (BS.index bs 1) :: Int32
      b2 = fromIntegral (BS.index bs 2) :: Int32
      b3 = fromIntegral (BS.index bs 3) :: Int32
   in b0 * 256 * 256 * 256 + b1 * 256 * 256 + b2 * 256 + b3
