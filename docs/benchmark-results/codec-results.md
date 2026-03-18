# Codec Benchmark Results

Machine: Apple Silicon (macOS Darwin 24.4.0)
GHC: 9.10.3, -O2
Date: 2026-03-17
Tool: criterion 1.6, time-limit 2s per benchmark

## Encode (Haskell value → PG binary)

| Type | Time | Notes |
|------|------|-------|
| Bool | 21 ns | 1 byte, direct |
| Int16 | 31 ns | 2 bytes, unsafeCreate + poke |
| Int32 | 38 ns | 4 bytes, unsafeCreate + poke |
| Int64 | 31 ns | 8 bytes, unsafeCreate + poke |
| Float | 33 ns | castFloatToWord32 + 4 bytes |
| Double | 36 ns | castDoubleToWord64 + 8 bytes |
| Text (5 chars) | 41 ns | UTF-8 encodeUtf8 |
| Text (100 chars) | 41 ns | Same — encodeUtf8 is O(1) for ASCII |
| Text (10K chars) | 378 ns | Non-ASCII or large copy |
| ByteString (100B) | 18 ns | Identity (zero-copy) |
| ByteString (10KB) | 18 ns | Identity (zero-copy) |
| Day | 56 ns | Julian day arithmetic |
| TimeOfDay | 143 ns | picosecond conversion |
| UTCTime | 296 ns | POSIX seconds → PG epoch micros |
| LocalTime | 159 ns | Day + TimeOfDay combined |
| ZonedTime | 462 ns | zonedTimeToUTC + UTCTime encode |
| (TimeOfDay, TimeZone) | 132 ns | 8 + 4 bytes, unsafeCreate |
| Scientific | 518 ns | Base-10000 digit conversion |
| PgInterval | 174 ns | 3 fields × int encode |
| PgInet (IPv4) | 193 ns | Builder: 4-byte header + 4-byte addr |
| PgInet (IPv6) | 197 ns | Builder: 4-byte header + 16-byte addr |
| PgMacAddr | 22 ns | Raw 6 bytes (identity) |
| PgPoint | 174 ns | Builder: 2 × float8 |
| PgHStore (5 pairs) | 930 ns | Builder: count + 5 × (key + value) |
| PgHStore (50 pairs) | 7.2 μs | ~144 ns/pair |
| Unbounded UTCTime (finite) | 284 ns | Delegates to UTCTime encode |
| Unbounded UTCTime (infinity) | 20 ns | Sentinel constant, int64BE |

## Decode (PG binary → Haskell value)

| Type | Time | Notes |
|------|------|-------|
| Bool | 25 ns | 1 byte check |
| Int16 | 23 ns | 2-byte big-endian |
| Int32 | 25 ns | 4-byte big-endian |
| Int64 | 19 ns | 8-byte big-endian, unrolled |
| Float | 36 ns | Word32 → castWord32ToFloat |
| Double | 19 ns | Word64 → castWord64ToDouble |
| Text (5 chars) | 56 ns | decodeUtf8' validation |
| Text (100 chars) | 57 ns | Same class — UTF-8 validation |
| Day | 29 ns | int32 + addDays |
| UTCTime | 57 ns | int64 → epoch arithmetic |
| Scientific | 119 ns | Base-10000 digit parsing |
| PgInterval | 59 ns | 3 × int decode |
| PgInet (IPv4) | 25 ns | 4-byte header + BS.drop |
| PgMacAddr | 23 ns | Length check + wrap |
| PgPoint | 40 ns | 2 × Word64 → Double |
| PgHStore (5 pairs) | 614 ns | ~123 ns/pair |
| PgHStore (50 pairs) | 6.7 μs | ~134 ns/pair |
| ZonedTime | 1.26 μs | UTCTime decode + utcToZonedTime |
| (TimeOfDay, TimeZone) | 1.06 μs | int64 + int32 + minutesToTimeZone |
| Unbounded UTCTime | 147 ns | Sentinel check + UTCTime decode |

## Array Encode

| Type | Size | Time | Per-element |
|------|------|------|-------------|
| Int32 | 10 | 761 ns | 76 ns |
| Int32 | 100 | 4.8 μs | 48 ns |
| Int32 | 1000 | 53.6 μs | 54 ns |
| Text | 100 | 6.0 μs | 60 ns |

## Array Decode

| Type | Size | Time | Per-element |
|------|------|------|-------------|
| Int32 | 10 | 522 ns | 52 ns |
| Int32 | 100 | 3.8 μs | 38 ns |
| Int32 | 1000 | 41.7 μs | 42 ns |
| Text | 100 | 7.0 μs | 70 ns |

## Key Observations

1. **Fixed-size types (Int, Bool, Float, Double)**: 19-38 ns. Dominated by
   `unsafeCreate` allocation overhead, not computation.

2. **ByteString encode is zero-copy**: 18 ns regardless of size — `pgEncode = id`.

3. **Text encode is O(1) for ASCII**: UTF-8 encoding of pure ASCII text is
   essentially a memcpy, so 100 chars takes the same time as 5.

4. **Infinity sentinel is 20 ns**: Just writes a constant int64, no conversion.

5. **HStore scales linearly**: ~130-144 ns per key-value pair for both
   encode and decode.

6. **ZonedTime decode is slow** (1.26 μs): `utcToZonedTime` does calendar
   computation. Use `UTCTime` when timezone display is not needed.

7. **Scientific is the slowest scalar** (518 ns encode, 119 ns decode):
   base-10000 digit representation requires division loops.

8. **Array per-element cost**: 38-76 ns per element, amortized. The overhead
   is primarily from the element codec, not the array framing.
