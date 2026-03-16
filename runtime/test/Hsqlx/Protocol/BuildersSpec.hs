module Hsqlx.Protocol.BuildersSpec (spec) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Hsqlx.Protocol.Builders
import Hsqlx.Protocol.Frontend
import Test.Hspec

spec :: Spec
spec = do
  describe "buildStartup" $ do
    it "starts with the correct protocol version (3.0)" $ do
      let bs = buildStartup (StartupParams "user" "db" "" [])
          -- Skip 4-byte length, then protocol version is bytes 4-7
          proto = BS.take 4 (BS.drop 4 bs)
      -- Protocol 3.0 = 0x00030000
      proto `shouldBe` BS.pack [0, 3, 0, 0]

    it "includes user and database parameters" $ do
      let bs = buildStartup (StartupParams "alice" "mydb" "" [])
      -- Should contain "user\0alice\0" and "database\0mydb\0"
      BS.isInfixOf "user\0alice\0" bs `shouldBe` True
      BS.isInfixOf "database\0mydb\0" bs `shouldBe` True

    it "includes application_name when non-empty" $ do
      let bs = buildStartup (StartupParams "alice" "mydb" "hsqlx-test" [])
      BS.isInfixOf "application_name\0hsqlx-test\0" bs `shouldBe` True

    it "omits application_name when empty" $ do
      let bs = buildStartup (StartupParams "alice" "mydb" "" [])
      BS.isInfixOf "application_name" bs `shouldBe` False

    it "includes extra parameters" $ do
      let bs = buildStartup (StartupParams "alice" "mydb" "" [("TimeZone", "UTC")])
      BS.isInfixOf "TimeZone\0UTC\0" bs `shouldBe` True

  describe "buildFrontendMsg" $ do
    it "encodes Sync as tag 'S' with length 4" $ do
      let bs = buildFrontendMsg Sync
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'S')
      -- Length field is 4 (just the length itself)
      BS.take 4 (BS.drop 1 bs) `shouldBe` BS.pack [0, 0, 0, 4]

    it "encodes Terminate as tag 'X' with length 4" $ do
      let bs = buildFrontendMsg Terminate
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'X')
      BS.length bs `shouldBe` 5

    it "encodes Flush as tag 'H' with length 4" $ do
      let bs = buildFrontendMsg Flush
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'H')
      BS.length bs `shouldBe` 5

    it "encodes Query with NUL-terminated SQL" $ do
      let bs = buildFrontendMsg (Query "SELECT 1")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'Q')
      -- Payload should contain "SELECT 1\0"
      BS.isInfixOf "SELECT 1\0" bs `shouldBe` True

    it "encodes Parse with statement name, SQL, and OIDs" $ do
      let bs = buildFrontendMsg (Parse "s1" "SELECT $1" (V.singleton 23))
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'P')
      BS.isInfixOf "s1\0" bs `shouldBe` True
      BS.isInfixOf "SELECT $1\0" bs `shouldBe` True

    it "encodes Parse with empty statement name" $ do
      let bs = buildFrontendMsg (Parse "" "SELECT 1" V.empty)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'P')
      -- Should have NUL for empty name followed by SQL
      BS.isInfixOf "\0SELECT 1\0" bs `shouldBe` True

    it "encodes PasswordMessage" $ do
      let bs = buildFrontendMsg (PasswordMessage "secret")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'p')
      BS.isInfixOf "secret\0" bs `shouldBe` True

  describe "Bind message encoding" $ do
    it "encodes Bind with no params and no format codes" $ do
      let bs = buildFrontendMsg (Bind "" "" V.empty V.empty V.empty)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'B')
      -- Should contain two NUL-terminated empty strings (portal + stmt name)
      -- Then 0 param format codes, 0 params, 0 result format codes
      BS.isInfixOf "\0\0" bs `shouldBe` True

    it "encodes Bind with parameter values" $ do
      let vals = V.fromList [Just "hello", Just "world"]
          pfmts = V.fromList [BinaryFormat, BinaryFormat]
          rfmts = V.fromList [BinaryFormat]
          bs = buildFrontendMsg (Bind "" "s1" pfmts vals rfmts)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'B')
      -- Values should appear in the output
      BS.isInfixOf "hello" bs `shouldBe` True
      BS.isInfixOf "world" bs `shouldBe` True

    it "encodes Bind with NULL parameter" $ do
      let vals = V.fromList [Just "abc", Nothing, Just "def"]
          pfmts = V.fromList [BinaryFormat, BinaryFormat, BinaryFormat]
          rfmts = V.singleton BinaryFormat
          bs = buildFrontendMsg (Bind "" "" pfmts vals rfmts)
      -- NULL is encoded as -1 in Int32 = 0xFF 0xFF 0xFF 0xFF
      BS.isInfixOf (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]) bs `shouldBe` True

    it "encodes Bind with text format codes" $ do
      let pfmts = V.fromList [TextFormat, BinaryFormat]
          vals = V.fromList [Just "42", Just (BS.pack [0, 0, 0, 42])]
          rfmts = V.singleton TextFormat
          bs = buildFrontendMsg (Bind "" "" pfmts vals rfmts)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'B')
      -- Text format = 0, Binary format = 1
      -- The param format codes section should contain [0, 0, 0, 0, 0, 1]
      -- (two Int16 values: 0 and 1)
      BS.isInfixOf (BS.pack [0, 0, 0, 1]) bs `shouldBe` True

  describe "Describe message encoding" $ do
    it "encodes Describe for a statement" $ do
      let bs = buildFrontendMsg (Describe DescribeStatement "s1")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'D')
      -- Should contain 'S' (for statement) followed by "s1\0"
      BS.isInfixOf (BS.pack [fromIntegral (fromEnum 'S')] <> "s1\0") bs `shouldBe` True

    it "encodes Describe for a portal" $ do
      let bs = buildFrontendMsg (Describe DescribePortal "p1")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'D')
      BS.isInfixOf (BS.pack [fromIntegral (fromEnum 'P')] <> "p1\0") bs `shouldBe` True

    it "encodes Describe with empty name (unnamed)" $ do
      let bs = buildFrontendMsg (Describe DescribeStatement "")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'D')
      -- 'S' followed by NUL
      BS.isInfixOf (BS.pack [fromIntegral (fromEnum 'S'), 0]) bs `shouldBe` True

  describe "Execute message encoding" $ do
    it "encodes Execute with unlimited rows" $ do
      let bs = buildFrontendMsg (Execute "" 0)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'E')
      -- Portal name NUL + max_rows (0 = unlimited) as 4 bytes
      BS.isInfixOf (BS.pack [0, 0, 0, 0, 0]) bs `shouldBe` True

    it "encodes Execute with row limit" $ do
      let bs = buildFrontendMsg (Execute "" 100)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'E')
      -- max_rows = 100 = 0x00000064
      BS.isInfixOf (BS.pack [0, 0, 0, 100]) bs `shouldBe` True

    it "encodes Execute with named portal" $ do
      let bs = buildFrontendMsg (Execute "portal1" 0)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'E')
      BS.isInfixOf "portal1\0" bs `shouldBe` True

  describe "SASLInitialResponse encoding" $ do
    it "encodes SCRAM-SHA-256 initial response" $ do
      let mech = "SCRAM-SHA-256"
          clientFirst = "n,,n=user,r=nonce"
          bs = buildFrontendMsg (SASLInitialResponse mech clientFirst)
      -- Tag is 'p' (same as PasswordMessage)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'p')
      -- Mechanism name should be NUL-terminated
      BS.isInfixOf "SCRAM-SHA-256\0" bs `shouldBe` True
      -- Client first message should appear after length
      BS.isInfixOf "n,,n=user,r=nonce" bs `shouldBe` True

    it "encodes SASLResponse" $ do
      let msg = "c=biws,r=nonce,p=proof"
          bs = buildFrontendMsg (SASLResponse msg)
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'p')
      BS.isInfixOf "c=biws,r=nonce,p=proof" bs `shouldBe` True

  describe "Close message encoding" $ do
    it "encodes Close for a statement" $ do
      let bs = buildFrontendMsg (Close DescribeStatement "s1")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'C')
      BS.isInfixOf (BS.pack [fromIntegral (fromEnum 'S')] <> "s1\0") bs `shouldBe` True

    it "encodes Close for a portal" $ do
      let bs = buildFrontendMsg (Close DescribePortal "p1")
      BS.index bs 0 `shouldBe` fromIntegral (fromEnum 'C')
      BS.isInfixOf (BS.pack [fromIntegral (fromEnum 'P')] <> "p1\0") bs `shouldBe` True
