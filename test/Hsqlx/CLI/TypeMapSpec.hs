{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Hsqlx.CLI.TypeMapSpec (spec) where

import Data.Map.Strict qualified as Map
import Database.PostgreSQL.LibPQ (Oid (..))
import Hsqlx.CLI.TypeMap
import Test.Hspec

spec :: Spec
spec = do
  describe "oidToHaskellType" $ do
    it "maps int4 (OID 23) to Int32" $ do
      let Just ht = oidToHaskellType (Oid 23)
      htType ht `shouldBe` "Int32"
      htModule ht `shouldBe` "Data.Int"

    it "maps text (OID 25) to Text" $ do
      let Just ht = oidToHaskellType (Oid 25)
      htType ht `shouldBe` "Text"

    it "maps bool (OID 16) to Bool" $ do
      let Just ht = oidToHaskellType (Oid 16)
      htType ht `shouldBe` "Bool"

    it "maps timestamptz (OID 1184) to UTCTime" $ do
      let Just ht = oidToHaskellType (Oid 1184)
      htType ht `shouldBe` "UTCTime"

    it "returns Nothing for unknown OIDs" $ do
      oidToHaskellType (Oid 99999) `shouldBe` Nothing

  describe "oidToTypeName" $ do
    it "maps OID 23 to int4" $ do
      oidToTypeName (Oid 23) `shouldBe` Just "int4"

    it "maps OID 25 to text" $ do
      oidToTypeName (Oid 25) `shouldBe` Just "text"

  describe "resolveType" $ do
    it "wraps in Maybe when nullable" $ do
      let ht = HaskellType "Text" "Data.Text"
      htType (resolveType True ht) `shouldBe` "Maybe Text"

    it "leaves unchanged when not nullable" $ do
      let ht = HaskellType "Int32" "Data.Int"
      htType (resolveType False ht) `shouldBe` "Int32"

  describe "isArrayOid" $ do
    it "recognises int4[] (OID 1007)" $ do
      isArrayOid (Oid 1007) `shouldBe` True

    it "recognises text[] (OID 1009)" $ do
      isArrayOid (Oid 1009) `shouldBe` True

    it "rejects non-array OID 23" $ do
      isArrayOid (Oid 23) `shouldBe` False

    it "rejects unknown OID" $ do
      isArrayOid (Oid 99999) `shouldBe` False

  describe "arrayElementOid" $ do
    it "maps int4[] to int4" $ do
      arrayElementOid (Oid 1007) `shouldBe` Just (Oid 23)

    it "maps text[] to text" $ do
      arrayElementOid (Oid 1009) `shouldBe` Just (Oid 25)

    it "maps timestamptz[] to timestamptz" $ do
      arrayElementOid (Oid 1185) `shouldBe` Just (Oid 1184)

    it "returns Nothing for non-array OID" $ do
      arrayElementOid (Oid 23) `shouldBe` Nothing

  describe "oidToHaskellTypeWith (array support)" $ do
    it "maps int4[] to Vector Int32" $ do
      let Just ht = oidToHaskellTypeWith Map.empty (Oid 1007)
      htType ht `shouldBe` "Vector Int32"
      htModule ht `shouldBe` "Data.Vector"

    it "maps text[] to Vector Text" $ do
      let Just ht = oidToHaskellTypeWith Map.empty (Oid 1009)
      htType ht `shouldBe` "Vector Text"

    it "maps bool[] to Vector Bool" $ do
      let Just ht = oidToHaskellTypeWith Map.empty (Oid 1000)
      htType ht `shouldBe` "Vector Bool"

    it "uses custom types for unknown OIDs" $ do
      let customs = Map.singleton (Oid 600) (HaskellType "MyPoint" "MyApp.Types")
          Just ht = oidToHaskellTypeWith customs (Oid 600)
      htType ht `shouldBe` "MyPoint"
      htModule ht `shouldBe` "MyApp.Types"

    it "custom types override built-in types" $ do
      let customs = Map.singleton (Oid 23) (HaskellType "CustomInt" "Custom")
          Just ht = oidToHaskellTypeWith customs (Oid 23)
      htType ht `shouldBe` "CustomInt"

  describe "oidToTypeName (array names)" $ do
    it "maps int4[] OID to _int4" $ do
      oidToTypeName (Oid 1007) `shouldBe` Just "_int4"

    it "maps text[] OID to _text" $ do
      oidToTypeName (Oid 1009) `shouldBe` Just "_text"
