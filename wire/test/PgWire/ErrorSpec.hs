module PgWire.ErrorSpec (spec) where

import Control.Exception (catch)
import PgWire.Error
import PgWire.Protocol.Backend (PgError (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "PgWireError constructors" $ do
    it "ConnectionError carries message" $ do
      let err = ConnectionError "host unreachable"
      show err `shouldContain` "host unreachable"

    it "AuthError carries message" $ do
      let err = AuthError "bad password"
      show err `shouldContain` "bad password"

    it "ProtocolError carries message" $ do
      let err = ProtocolError "unexpected tag"
      show err `shouldContain` "unexpected tag"

    it "DecodeError carries message" $ do
      let err = DecodeError "int32: wrong size"
      show err `shouldContain` "int32: wrong size"

    it "PoolTimeout has no payload" $
      show PoolTimeout `shouldContain` "PoolTimeout"

    it "PoolClosed has no payload" $
      show PoolClosed `shouldContain` "PoolClosed"

    it "QueryError wraps PgError" $ do
      let pgErr = emptyPgError {pgMessage = "table not found", pgCode = "42P01"}
          err = QueryError pgErr
      show err `shouldContain` "table not found"
      show err `shouldContain` "42P01"

  describe "throwPgWire" $ do
    it "throws as an Exception" $ do
      let action = throwPgWire (ConnectionError "test") :: IO ()
      action `shouldThrow` (== ConnectionError "test")

    it "can be caught with specific pattern" $ do
      result <- (throwPgWire PoolTimeout >> pure "no") `catch` \(e :: PgWireError) ->
        case e of
          PoolTimeout -> pure "caught"
          _ -> pure "wrong"
      result `shouldBe` ("caught" :: String)

  describe "Eq instance" $ do
    it "equal errors are equal" $
      ConnectionError "x" `shouldBe` ConnectionError "x"

    it "different errors are not equal" $
      ConnectionError "x" `shouldNotBe` ConnectionError "y"

    it "different constructors are not equal" $
      ConnectionError "x" `shouldNotBe` AuthError "x"

emptyPgError :: PgError
emptyPgError = PgError "" "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
