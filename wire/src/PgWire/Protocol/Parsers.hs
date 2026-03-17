module PgWire.Protocol.Parsers
  ( parseBackendMsg
  , parseCommandTag
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.Int (Int16, Int32, Int64)
import Data.Vector qualified as V
import Data.Word (Word8, Word32)
import PgWire.Protocol.Backend

-- | Parse a complete backend message from tag + payload bytes.
-- The caller must have already read the tag byte and the 4-byte length,
-- and pass the tag and the remaining payload (length - 4 bytes).
parseBackendMsg :: Word8 -> ByteString -> Either String BackendMsg
parseBackendMsg tag payload = case tag of
  82 {- R -} -> parseAuth payload
  75 {- K -} -> parseBackendKeyData payload
  83 {- S -} -> parseParameterStatus payload
  90 {- Z -} -> parseReadyForQuery payload
  49 {- 1 -} -> Right ParseComplete
  50 {- 2 -} -> Right BindComplete
  51 {- 3 -} -> Right CloseComplete
  67 {- C -} -> parseCommandComplete payload
  68 {- D -} -> parseDataRow payload
  69 {- E -} -> ErrorResponse <$> parseErrorFields payload
  71 {- G -} -> parseCopyInResponse payload
  72 {- H -} -> parseCopyOutResponse payload
  73 {- I -} -> Right EmptyQueryResponse
  99 {- c -} -> Right CopyDoneMsg
  100 {- d -} -> Right (CopyDataMsg payload)
  78 {- N -} -> NoticeResponse . PgNotice <$> parseErrorFields payload
  84 {- T -} -> parseRowDescription payload
  110 {- n -} -> Right NoData
  116 {- t -} -> parseParameterDescription payload
  65 {- A -} -> parseNotification payload
  _ -> Left $ "Unknown backend message tag: " <> show tag

-- Authentication ----------------------------------------------------------

parseAuth :: ByteString -> Either String BackendMsg
parseAuth bs = do
  authType <- getInt32 bs 0
  case authType of
    0 -> Right (Authentication AuthOk)
    3 -> Right (Authentication AuthCleartextPassword)
    5 -> do
      salt <- getBytes bs 4 4
      Right (Authentication (AuthMD5Password salt))
    10 -> do
      -- SASL: list of mechanism names, each NUL-terminated, final NUL
      let mechData = BS.drop 4 bs
          mechs = parseCStringList mechData
      Right (Authentication (AuthSASL mechs))
    11 -> Right (Authentication (AuthSASLContinue (BS.drop 4 bs)))
    12 -> Right (Authentication (AuthSASLFinal (BS.drop 4 bs)))
    _ -> Left $ "Unknown auth type: " <> show authType

-- ParameterStatus ---------------------------------------------------------

parseParameterStatus :: ByteString -> Either String BackendMsg
parseParameterStatus bs =
  let (name, rest) = BS.break (== 0) bs
      value = BS.takeWhile (/= 0) (BS.drop 1 rest)
   in Right (ParameterStatus name value)

-- BackendKeyData ----------------------------------------------------------

parseBackendKeyData :: ByteString -> Either String BackendMsg
parseBackendKeyData bs = do
  pid <- getInt32 bs 0
  key <- getInt32 bs 4
  Right (BackendKeyData pid key)

-- ReadyForQuery -----------------------------------------------------------

parseReadyForQuery :: ByteString -> Either String BackendMsg
parseReadyForQuery bs
  | BS.length bs < 1 = Left "ReadyForQuery: empty payload"
  | otherwise = case BS.index bs 0 of
      73 {- I -} -> Right (ReadyForQuery TxIdle)
      84 {- T -} -> Right (ReadyForQuery TxInTransaction)
      69 {- E -} -> Right (ReadyForQuery TxFailed)
      c -> Left $ "ReadyForQuery: unknown status " <> show c

-- CommandComplete ---------------------------------------------------------

parseCommandComplete :: ByteString -> Either String BackendMsg
parseCommandComplete bs =
  let tag = BS.takeWhile (/= 0) bs
   in Right (CommandComplete (parseCommandTag tag))

parseCommandTag :: ByteString -> CommandTag
parseCommandTag bs
  | "SELECT " `BS.isPrefixOf` bs = SelectTag (readInt64 (BS.drop 7 bs))
  | "INSERT " `BS.isPrefixOf` bs =
      -- INSERT oid count — we want the count (after the second space)
      let afterInsert = BS.drop 7 bs
          afterOid = BS.drop 1 (BS.dropWhile (/= 32) afterInsert)
       in InsertTag (readInt64 afterOid)
  | "UPDATE " `BS.isPrefixOf` bs = UpdateTag (readInt64 (BS.drop 7 bs))
  | "DELETE " `BS.isPrefixOf` bs = DeleteTag (readInt64 (BS.drop 7 bs))
  | otherwise = OtherTag bs

readInt64 :: ByteString -> Int64
readInt64 bs =
  BS.foldl' (\acc w -> acc * 10 + fromIntegral (w - 48)) 0 (BS.takeWhile (\w -> w >= 48 && w <= 57) bs)

-- RowDescription ----------------------------------------------------------

parseRowDescription :: ByteString -> Either String BackendMsg
parseRowDescription bs = do
  nFields <- getInt16 bs 0
  (fields, _) <- parseFields (fromIntegral nFields) (BS.drop 2 bs)
  Right (RowDescription (V.fromList fields))

parseFields :: Int -> ByteString -> Either String ([FieldInfo], ByteString)
parseFields 0 rest = Right ([], rest)
parseFields n bs = do
  let (name, afterName) = BS.break (== 0) bs
      rest = BS.drop 1 afterName -- skip NUL
  tableOid <- getWord32 rest 0
  colNum <- getInt16 rest 4
  typeOid <- getWord32 rest 6
  typeSize <- getInt16 rest 10
  typeMod <- getInt32 rest 12
  fmtCode <- getInt16 rest 16
  let field =
        FieldInfo
          { fiName = name
          , fiTableOid = tableOid
          , fiColumnNum = colNum
          , fiTypeOid = typeOid
          , fiTypeSize = typeSize
          , fiTypeMod = typeMod
          , fiFormatCode = fmtCode
          }
  (fields, rest') <- parseFields (n - 1) (BS.drop 18 rest)
  Right (field : fields, rest')

-- DataRow -----------------------------------------------------------------

-- | Parse a DataRow message. Uses V.fromListN with the known column count
-- to pre-allocate the Vector at the correct size.
parseDataRow :: ByteString -> Either String BackendMsg
parseDataRow bs = do
  nCols <- getInt16 bs 0
  let !n = fromIntegral nCols
  (vals, _) <- parseColValues n (BS.drop 2 bs)
  Right (DataRow (V.fromListN n vals))

parseColValues :: Int -> ByteString -> Either String ([Maybe ByteString], ByteString)
parseColValues 0 rest = Right ([], rest)
parseColValues !n bs = do
  len <- getInt32 bs 0
  if len == -1
    then do
      (vals, rest) <- parseColValues (n - 1) (BS.drop 4 bs)
      Right (Nothing : vals, rest)
    else do
      let !dataLen = fromIntegral len
      val <- getBytes bs 4 dataLen
      (vals, rest) <- parseColValues (n - 1) (BS.drop (4 + dataLen) bs)
      Right (Just val : vals, rest)

-- ParameterDescription ----------------------------------------------------

parseParameterDescription :: ByteString -> Either String BackendMsg
parseParameterDescription bs = do
  nParams <- getInt16 bs 0
  oids <- parseOids (fromIntegral nParams) (BS.drop 2 bs)
  Right (ParameterDescription (V.fromList oids))

parseOids :: Int -> ByteString -> Either String [Word32]
parseOids 0 _ = Right []
parseOids n bs = do
  oid <- getWord32 bs 0
  oids <- parseOids (n - 1) (BS.drop 4 bs)
  Right (oid : oids)

-- ErrorResponse / NoticeResponse ------------------------------------------

parseErrorFields :: ByteString -> Either String PgError
parseErrorFields = go emptyPgError
  where
    go !err bs
      | BS.null bs = Right err
      | BS.index bs 0 == 0 = Right err
      | otherwise =
          let fieldType = BS.index bs 0
              (value, rest) = BS.break (== 0) (BS.drop 1 bs)
              err' = case fieldType of
                83 {- S -} -> err {pgSeverity = value}
                67 {- C -} -> err {pgCode = value}
                77 {- M -} -> err {pgMessage = value}
                68 {- D -} -> err {pgDetail = Just value}
                72 {- H -} -> err {pgHint = Just value}
                80 {- P -} -> err {pgPosition = Just (fromIntegral (readInt64 value))}
                112 {- p -} -> err {pgInternalPosition = Just (fromIntegral (readInt64 value))}
                113 {- q -} -> err {pgInternalQuery = Just value}
                87 {- W -} -> err {pgWhere = Just value}
                115 {- s -} -> err {pgSchema = Just value}
                116 {- t -} -> err {pgTable = Just value}
                99 {- c -} -> err {pgColumn = Just value}
                100 {- d -} -> err {pgDataType = Just value}
                110 {- n -} -> err {pgConstraint = Just value}
                70 {- F -} -> err {pgFile = Just value}
                76 {- L -} -> err {pgLine = Just (fromIntegral (readInt64 value))}
                82 {- R -} -> err {pgRoutine = Just value}
                _ -> err
           in go err' (BS.drop 1 rest) -- skip NUL after value

    emptyPgError =
      PgError
        { pgSeverity = ""
        , pgCode = ""
        , pgMessage = ""
        , pgDetail = Nothing
        , pgHint = Nothing
        , pgPosition = Nothing
        , pgInternalPosition = Nothing
        , pgInternalQuery = Nothing
        , pgWhere = Nothing
        , pgSchema = Nothing
        , pgTable = Nothing
        , pgColumn = Nothing
        , pgDataType = Nothing
        , pgConstraint = Nothing
        , pgFile = Nothing
        , pgLine = Nothing
        , pgRoutine = Nothing
        }

-- CopyInResponse / CopyOutResponse ----------------------------------------

parseCopyInResponse :: ByteString -> Either String BackendMsg
parseCopyInResponse bs = do
  fmt <- getInt8 bs 0
  nCols <- getInt16 bs 1
  fmts <- mapM (\i -> getInt16 bs (3 + i * 2)) [0 .. fromIntegral nCols - 1]
  Right (CopyInResponse (fromIntegral fmt) (V.fromList fmts))

parseCopyOutResponse :: ByteString -> Either String BackendMsg
parseCopyOutResponse bs = do
  fmt <- getInt8 bs 0
  nCols <- getInt16 bs 1
  fmts <- mapM (\i -> getInt16 bs (3 + i * 2)) [0 .. fromIntegral nCols - 1]
  Right (CopyOutResponse (fromIntegral fmt) (V.fromList fmts))

getInt8 :: ByteString -> Int -> Either String Word8
getInt8 bs off
  | BS.length bs < off + 1 = Left "getInt8: buffer too short"
  | otherwise = Right (BS.index bs off)

-- Notification ------------------------------------------------------------

parseNotification :: ByteString -> Either String BackendMsg
parseNotification bs = do
  pid <- getInt32 bs 0
  let (channel, rest1) = BS.break (== 0) (BS.drop 4 bs)
      payload = BS.takeWhile (/= 0) (BS.drop 1 rest1)
  Right (NotificationResponse pid channel payload)

-- Helpers -----------------------------------------------------------------

parseCStringList :: ByteString -> [ByteString]
parseCStringList bs
  | BS.null bs = []
  | BS.index bs 0 == 0 = []
  | otherwise =
      let (s, rest) = BS.break (== 0) bs
       in s : parseCStringList (BS.drop 1 rest)

getInt16 :: ByteString -> Int -> Either String Int16
getInt16 bs off
  | BS.length bs < off + 2 = Left "getInt16: buffer too short"
  | otherwise =
      -- After bounds check, use unsafeIndex for the hot path
      let hi = fromIntegral (BU.unsafeIndex bs off) :: Int16
          lo = fromIntegral (BU.unsafeIndex bs (off + 1)) :: Int16
       in Right (hi `shiftL` 8 .|. lo)
{-# INLINE getInt16 #-}

getInt32 :: ByteString -> Int -> Either String Int32
getInt32 bs off
  | BS.length bs < off + 4 = Left "getInt32: buffer too short"
  | otherwise =
      let b0 = fromIntegral (BU.unsafeIndex bs off) :: Int32
          b1 = fromIntegral (BU.unsafeIndex bs (off + 1)) :: Int32
          b2 = fromIntegral (BU.unsafeIndex bs (off + 2)) :: Int32
          b3 = fromIntegral (BU.unsafeIndex bs (off + 3)) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
{-# INLINE getInt32 #-}

getWord32 :: ByteString -> Int -> Either String Word32
getWord32 bs off
  | BS.length bs < off + 4 = Left "getWord32: buffer too short"
  | otherwise =
      let b0 = fromIntegral (BU.unsafeIndex bs off) :: Word32
          b1 = fromIntegral (BU.unsafeIndex bs (off + 1)) :: Word32
          b2 = fromIntegral (BU.unsafeIndex bs (off + 2)) :: Word32
          b3 = fromIntegral (BU.unsafeIndex bs (off + 3)) :: Word32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
{-# INLINE getWord32 #-}

getBytes :: ByteString -> Int -> Int -> Either String ByteString
getBytes bs off len
  | BS.length bs < off + len = Left "getBytes: buffer too short"
  | otherwise = Right (BS.take len (BS.drop off bs))
{-# INLINE getBytes #-}
