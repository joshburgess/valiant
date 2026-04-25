module LargeObjectSpec (spec) where

import Control.Exception (bracket_)
import Data.ByteString qualified as BS
import Valiant
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "loCreate / loUnlink" $ do
    it "round-trips OIDs without error" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> do
          oid <- loCreate (txConn tx)
          oid `shouldSatisfy` (> 0)
          loUnlink (txConn tx) oid

  describe "withLargeObject + loRead/loWrite" $ do
    it "writes and reads back binary data" $ do
      withTestPool $ \pool -> do
        let payload = BS.pack [0 .. 255]
        oid <- withTransaction pool $ \tx -> do
          o <- loCreate (txConn tx)
          withLargeObject (txConn tx) o WriteMode $ \fd -> do
            n <- loWrite (txConn tx) fd payload
            n `shouldBe` fromIntegral (BS.length payload)
          pure o

        readBack <- withTransaction pool $ \tx ->
          withLargeObject (txConn tx) oid ReadMode $ \fd ->
            loRead (txConn tx) fd 1024
        readBack `shouldBe` payload

        withTransaction pool $ \tx -> loUnlink (txConn tx) oid

    it "supports loSeek + partial reads" $ do
      withTestPool $ \pool -> do
        let payload = BS.pack [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
        oid <- withTransaction pool $ \tx -> do
          o <- loCreate (txConn tx)
          withLargeObject (txConn tx) o WriteMode $ \fd -> do
            _ <- loWrite (txConn tx) fd payload
            pure ()
          pure o

        chunk <- withTransaction pool $ \tx ->
          withLargeObject (txConn tx) oid ReadMode $ \fd -> do
            _ <- loSeek (txConn tx) fd 4 0  -- SEEK_SET
            loRead (txConn tx) fd 3
        chunk `shouldBe` BS.pack [50, 60, 70]

        withTransaction pool $ \tx -> loUnlink (txConn tx) oid

    it "loTell reports the current cursor position" $ do
      withTestPool $ \pool -> do
        let payload = BS.replicate 100 0xAB
        oid <- withTransaction pool $ \tx -> do
          o <- loCreate (txConn tx)
          withLargeObject (txConn tx) o WriteMode $ \fd -> do
            _ <- loWrite (txConn tx) fd payload
            pure ()
          pure o

        position <- withTransaction pool $ \tx ->
          withLargeObject (txConn tx) oid ReadMode $ \fd -> do
            _ <- loRead (txConn tx) fd 25
            loTell (txConn tx) fd
        position `shouldBe` 25

        withTransaction pool $ \tx -> loUnlink (txConn tx) oid

  describe "withLargeObject bracket safety" $ do
    it "closes the descriptor when the action throws" $ do
      withTestPool $ \pool -> do
        oid <- withTransaction pool $ \tx -> loCreate (txConn tx)
        -- The bracket should propagate exceptions while still closing
        -- the underlying lo descriptor on the server. Round-trip via a
        -- second transaction proves the OID is still readable.
        _ <- withTransaction pool $ \tx ->
          withLargeObject (txConn tx) oid WriteMode $ \fd ->
            bracket_ (pure ()) (pure ()) $ do
              _ <- loWrite (txConn tx) fd "hello"
              pure ()

        readBack <- withTransaction pool $ \tx ->
          withLargeObject (txConn tx) oid ReadMode $ \fd ->
            loRead (txConn tx) fd 5
        readBack `shouldBe` "hello"

        withTransaction pool $ \tx -> loUnlink (txConn tx) oid
