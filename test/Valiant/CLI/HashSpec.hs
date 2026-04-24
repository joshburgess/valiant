module Valiant.CLI.HashSpec (spec) where

import Data.Text qualified as T
import Valiant.CLI.Hash
import Test.Hspec

spec :: Spec
spec = do
  it "produces a 64-character hex string" $ do
    let h = sha256Hex "hello"
    T.length h `shouldBe` 64

  it "is deterministic" $ do
    sha256Hex "test" `shouldBe` sha256Hex "test"

  it "differs for different inputs" $ do
    sha256Hex "a" `shouldNotBe` sha256Hex "b"

  it "truncates to the requested length" $ do
    let h = sha256HexTruncated 12 "hello"
    T.length h `shouldBe` 12

  it "truncated hash is a prefix of the full hash" $ do
    let full = sha256Hex "hello"
        short = sha256HexTruncated 12 "hello"
    T.take 12 full `shouldBe` short

  -- Known SHA-256 of empty string
  it "hashes empty string correctly" $ do
    sha256Hex "" `shouldBe` "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
