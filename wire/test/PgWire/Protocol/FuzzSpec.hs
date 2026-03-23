{-# OPTIONS_GHC -Wno-orphans #-}

module PgWire.Protocol.FuzzSpec (spec) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (evaluate, try, SomeException)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word32)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgWire.Protocol.Backend (CommandTag (..))
import PgWire.Protocol.Parsers (parseBackendMsg, parseCommandTag)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

-- | NFData instance for CommandTag so we can deepseq it.
instance NFData CommandTag where
  rnf (SelectTag n) = n `seq` ()
  rnf (InsertTag n) = n `seq` ()
  rnf (UpdateTag n) = n `seq` ()
  rnf (DeleteTag n) = n `seq` ()
  rnf (OtherTag b) = rnf b

-- | Tags whose parsers are purely bounds-checked (no unbounded recursion).
-- These are safe to fuzz with completely random payloads even on a small
-- stack (the test suite runs with -K8K).
safeTags :: [Word8]
safeTags =
  [ 82  -- R: Authentication
  , 75  -- K: BackendKeyData
  , 83  -- S: ParameterStatus
  , 90  -- Z: ReadyForQuery
  , 49  -- 1: ParseComplete
  , 50  -- 2: BindComplete
  , 51  -- 3: CloseComplete
  , 73  -- I: EmptyQueryResponse
  , 99  -- c: CopyDone
  , 100 -- d: CopyData
  , 110 -- n: NoData
  , 65  -- A: NotificationResponse
  ]

-- | Tags whose parsers may recurse on their payload (ErrorResponse,
-- NoticeResponse) or iterate based on an encoded count (DataRow,
-- RowDescription, ParameterDescription, CopyIn/OutResponse,
-- CommandComplete). We fuzz these with payloads that contain NUL
-- terminators and bounded counts so they do not overflow the tiny
-- test-suite stack.
recursiveTags :: [Word8]
recursiveTags =
  [ 67  -- C: CommandComplete
  , 68  -- D: DataRow
  , 69  -- E: ErrorResponse
  , 71  -- G: CopyInResponse
  , 72  -- H: CopyOutResponse
  , 78  -- N: NoticeResponse
  , 84  -- T: RowDescription
  , 116 -- t: ParameterDescription
  ]

-- | Evaluate a parse result to WHNF (forcing the Either constructor).
-- The module uses StrictData and bang patterns internally, so forcing
-- the outer Either exercises the parser.  Returns True when the parser
-- produces Left or Right without throwing.
doesNotCrash :: Word8 -> ByteString -> IO Bool
doesNotCrash tag payload = do
  result <- try $ evaluate $ case parseBackendMsg tag payload of
    Left _ -> ()
    Right _ -> ()
  case result of
    Left (_ :: SomeException) -> pure False
    Right () -> pure True

spec :: Spec
spec = do
  -- ── 1. No crashes on arbitrary input ──────────────────────────────
  describe "No crashes on arbitrary input" $ do
    it "safe tags with completely random payloads" $ hedgehog $ do
      tag <- forAll (Gen.element safeTags)
      payload <- forAll (Gen.bytes (Range.linear 0 128))
      ok <- evalIO (doesNotCrash tag payload)
      assert ok

    it "unknown tags with random payloads" $ hedgehog $ do
      -- Tags outside the known set always return Left immediately
      tag <- forAll $ Gen.filter (\t -> t `notElem` safeTags && t `notElem` recursiveTags)
                                 (Gen.word8 Range.linearBounded)
      payload <- forAll (Gen.bytes (Range.linear 0 128))
      ok <- evalIO (doesNotCrash tag payload)
      assert ok

    it "ErrorResponse with NUL-bearing payloads" $ hedgehog $ do
      -- Generate payloads that contain NUL bytes so the field parser terminates
      payload <- forAll genNulBearingPayload
      ok <- evalIO (doesNotCrash 69 payload)
      assert ok

    it "NoticeResponse with NUL-bearing payloads" $ hedgehog $ do
      payload <- forAll genNulBearingPayload
      ok <- evalIO (doesNotCrash 78 payload)
      assert ok

    it "CommandComplete with arbitrary tag strings" $ hedgehog $ do
      tagStr <- forAll (Gen.bytes (Range.linear 0 64))
      -- CommandComplete expects a NUL-terminated tag
      let payload = tagStr <> BS.singleton 0
      ok <- evalIO (doesNotCrash 67 payload)
      assert ok

    it "RowDescription with bounded field count" $ hedgehog $ do
      payload <- forAll genRowDescPayload
      ok <- evalIO (doesNotCrash 84 payload)
      assert ok

    it "ParameterDescription with bounded param count" $ hedgehog $ do
      nParams <- forAll (Gen.int16 (Range.linear 0 20))
      trailing <- forAll (Gen.bytes (Range.linear 0 128))
      let payload = int16 nParams <> trailing
      ok <- evalIO (doesNotCrash 116 payload)
      assert ok

    it "CopyInResponse with bounded column count" $ hedgehog $ do
      payload <- forAll genCopyResponsePayload
      ok <- evalIO (doesNotCrash 71 payload)
      assert ok

    it "CopyOutResponse with bounded column count" $ hedgehog $ do
      payload <- forAll genCopyResponsePayload
      ok <- evalIO (doesNotCrash 72 payload)
      assert ok

    it "medium payloads do not hang (safe tags)" $ hedgehog $ do
      tag <- forAll (Gen.element safeTags)
      payload <- forAll (Gen.bytes (Range.linear 64 256))
      ok <- evalIO (doesNotCrash tag payload)
      assert ok

  -- ── 2. No crashes on truncated messages ───────────────────────────
  describe "No crashes on truncated messages" $ do
    it "truncated AuthOk" $ hedgehog $ do
      let fullPayload = int32 0
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 82 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated AuthMD5" $ hedgehog $ do
      let fullPayload = int32 5 <> BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 82 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated BackendKeyData" $ hedgehog $ do
      let fullPayload = int32 12345 <> int32 67890
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 75 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated ReadyForQuery" $ hedgehog $ do
      ok <- evalIO (doesNotCrash 90 BS.empty)
      assert ok

    it "truncated DataRow with columns" $ hedgehog $ do
      nCols <- forAll (Gen.int (Range.linear 1 10))
      let colPayload = mconcat [int32 3 <> "abc" | _ <- [1 :: Int .. nCols]]
          fullPayload = int16 (fromIntegral nCols) <> colPayload
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 68 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated RowDescription" $ hedgehog $ do
      let field = "col\0" <> word32 0 <> int16 1 <> word32 23 <> int16 4 <> int32 (-1) <> int16 0
          fullPayload = int16 1 <> field
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 84 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated ParameterDescription" $ hedgehog $ do
      nParams <- forAll (Gen.int (Range.linear 1 5))
      let fullPayload = int16 (fromIntegral nParams)
            <> mconcat [word32 (fromIntegral i) | i <- [1 :: Int .. nParams]]
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 116 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated CopyInResponse" $ hedgehog $ do
      let fullPayload = BS.singleton 0 <> int16 2 <> int16 0 <> int16 1
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 71 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated ErrorResponse" $ hedgehog $ do
      let fullPayload = BS.concat
            [ BS.singleton 83, "ERROR\0"
            , BS.singleton 67, "42P01\0"
            , BS.singleton 77, "relation not found\0"
            , BS.singleton 0
            ]
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 69 (BS.take truncateAt fullPayload))
      assert ok

    it "truncated NotificationResponse" $ hedgehog $ do
      let fullPayload = int32 42 <> "my_channel\0" <> "hello\0"
      truncateAt <- forAll (Gen.int (Range.linear 0 (BS.length fullPayload - 1)))
      ok <- evalIO (doesNotCrash 65 (BS.take truncateAt fullPayload))
      assert ok

  -- ── 3. CommandTag fuzz ────────────────────────────────────────────
  describe "CommandTag fuzz" $ do
    it "parseCommandTag handles arbitrary bytestrings" $ hedgehog $ do
      bs <- forAll (Gen.bytes (Range.linear 0 256))
      ok <- evalIO (commandTagDoesNotCrash bs)
      assert ok

    it "parseCommandTag handles SELECT prefix with garbage" $ hedgehog $ do
      suffix <- forAll (Gen.bytes (Range.linear 0 64))
      ok <- evalIO (commandTagDoesNotCrash ("SELECT " <> suffix))
      assert ok

    it "parseCommandTag handles INSERT prefix with garbage" $ hedgehog $ do
      suffix <- forAll (Gen.bytes (Range.linear 0 64))
      ok <- evalIO (commandTagDoesNotCrash ("INSERT " <> suffix))
      assert ok

    it "parseCommandTag handles UPDATE prefix with garbage" $ hedgehog $ do
      suffix <- forAll (Gen.bytes (Range.linear 0 64))
      ok <- evalIO (commandTagDoesNotCrash ("UPDATE " <> suffix))
      assert ok

    it "parseCommandTag handles DELETE prefix with garbage" $ hedgehog $ do
      suffix <- forAll (Gen.bytes (Range.linear 0 64))
      ok <- evalIO (commandTagDoesNotCrash ("DELETE " <> suffix))
      assert ok

  -- ── 4. DataRow bounds safety ──────────────────────────────────────
  describe "DataRow bounds safety" $ do
    it "well-formed random columns" $ hedgehog $ do
      nCols <- forAll (Gen.int (Range.linear 0 50))
      cols <- forAll (Gen.list (Range.singleton nCols) genColumnData)
      let payload = int16 (fromIntegral nCols) <> mconcat cols
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "declared column count exceeding actual data" $ hedgehog $ do
      declaredCols <- forAll (Gen.int (Range.linear 1 100))
      actualCols <- forAll (Gen.int (Range.linear 0 (declaredCols - 1)))
      cols <- forAll (Gen.list (Range.singleton actualCols) genColumnData)
      let payload = int16 (fromIntegral declaredCols) <> mconcat cols
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "zero-length columns" $ hedgehog $ do
      nCols <- forAll (Gen.int (Range.linear 1 20))
      let payload = int16 (fromIntegral nCols)
            <> mconcat (replicate nCols (int32 0))
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "all NULL columns" $ hedgehog $ do
      nCols <- forAll (Gen.int (Range.linear 1 50))
      let payload = int16 (fromIntegral nCols)
            <> mconcat (replicate nCols (int32 (-1)))
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "very large declared column length" $ hedgehog $ do
      bigLen <- forAll (Gen.int32 (Range.linear 1000 2147483647))
      let payload = int16 1 <> int32 bigLen <> "tiny"
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "mixed valid and truncated columns" $ hedgehog $ do
      nValid <- forAll (Gen.int (Range.linear 1 5))
      validCols <- forAll (Gen.list (Range.singleton nValid) genColumnData)
      -- Append a partial column (length header but no data)
      partialLen <- forAll (Gen.int32 (Range.linear 1 100))
      let payload = int16 (fromIntegral (nValid + 1))
            <> mconcat validCols
            <> int32 partialLen
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

    it "DataRow with random trailing garbage" $ hedgehog $ do
      nCols <- forAll (Gen.int (Range.linear 1 5))
      cols <- forAll (Gen.list (Range.singleton nCols) genColumnData)
      garbage <- forAll (Gen.bytes (Range.linear 0 64))
      let payload = int16 (fromIntegral nCols) <> mconcat cols <> garbage
      ok <- evalIO (doesNotCrash 68 payload)
      assert ok

-- | Evaluate parseCommandTag, fully forcing the result via deepseq.
commandTagDoesNotCrash :: ByteString -> IO Bool
commandTagDoesNotCrash bs = do
  result <- try $ evaluate (parseCommandTag bs `deepseq` ())
  case result of
    Left (_ :: SomeException) -> pure False
    Right () -> pure True

-- Generators ------------------------------------------------------------------

-- | Generate a payload with embedded NUL bytes so recursive parsers
-- (ErrorResponse / NoticeResponse) terminate promptly.
genNulBearingPayload :: Gen ByteString
genNulBearingPayload = do
  -- Build a sequence of (fieldType, value) pairs terminated by NUL
  nFields <- Gen.int (Range.linear 0 10)
  fields <- Gen.list (Range.singleton nFields) $ do
    fieldType <- Gen.word8 Range.linearBounded
    value <- Gen.bytes (Range.linear 0 32)
    -- Ensure value doesn't contain NUL (so NUL terminators are clear)
    let safeValue = BS.map (\b -> if b == 0 then 1 else b) value
    pure $ BS.singleton fieldType <> safeValue <> BS.singleton 0
  pure $ mconcat fields <> BS.singleton 0

-- | Generate a single DataRow column encoding: either NULL (-1) or
-- a length-prefixed bytestring.
genColumnData :: Gen ByteString
genColumnData = Gen.choice
  [ pure (int32 (-1))  -- NULL
  , do
      colBytes <- Gen.bytes (Range.linear 0 128)
      pure (int32 (fromIntegral (BS.length colBytes)) <> colBytes)
  ]

-- | Generate a RowDescription payload with a bounded field count.
genRowDescPayload :: Gen ByteString
genRowDescPayload = do
  nFields <- Gen.int16 (Range.linear 0 5)
  fields <- Gen.list (Range.singleton (fromIntegral nFields)) $ do
    -- name (NUL-terminated) + 18 bytes of field metadata
    name <- Gen.bytes (Range.linear 1 16)
    let safeName = BS.map (\b -> if b == 0 then 65 else b) name
    metadata <- Gen.bytes (Range.singleton 18)
    pure $ safeName <> BS.singleton 0 <> metadata
  pure $ int16 nFields <> mconcat fields

-- | Generate a CopyInResponse / CopyOutResponse payload.
genCopyResponsePayload :: Gen ByteString
genCopyResponsePayload = do
  fmt <- Gen.word8 (Range.linear 0 1)
  nCols <- Gen.int16 (Range.linear 0 10)
  colFmts <- Gen.list (Range.singleton (fromIntegral nCols)) $
    Gen.bytes (Range.singleton 2)
  pure $ BS.singleton fmt <> int16 nCols <> mconcat colFmts

-- Helpers ---------------------------------------------------------------------

int16 :: Int16 -> ByteString
int16 n = LBS.toStrict . B.toLazyByteString $ B.int16BE n

int32 :: Int32 -> ByteString
int32 n = LBS.toStrict . B.toLazyByteString $ B.int32BE n

word32 :: Word32 -> ByteString
word32 n = LBS.toStrict . B.toLazyByteString $ B.word32BE n
