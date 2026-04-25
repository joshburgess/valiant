module NotifySpec (spec) where

import Control.Concurrent (threadDelay)
import Valiant
import TestSupport
import Test.Hspec

-- NOTE: waitForNotification is edge-triggered by the next query; see
-- Valiant.Notify haddock. These tests send the NOTIFY first, wait a
-- beat for the server to push it onto the listener's socket, then call
-- waitForNotificationTimeout so a hung delivery fails fast instead of
-- stalling CI.
spec :: Spec
spec = do
  describe "listen / waitForNotificationTimeout" $ do
    it "delivers a notification queued before the wait" $ do
      withTestConnection $ \listenerConn -> withTestConnection $ \senderConn -> do
        listen listenerConn "valiant_test_chan"
        _ <- simpleQuery senderConn "NOTIFY valiant_test_chan, 'hello'"
        threadDelay 50000
        result <- waitForNotificationTimeout listenerConn 1.0
        case result of
          Just n -> do
            notifChannel n `shouldBe` "valiant_test_chan"
            notifPayload n `shouldBe` "hello"
          Nothing -> expectationFailure "expected notification, got timeout"
        unlisten listenerConn "valiant_test_chan"

    it "returns Nothing when no notification is sent" $ do
      withTestConnection $ \conn -> do
        listen conn "valiant_test_silent"
        result <- waitForNotificationTimeout conn 0.2
        result `shouldBe` Nothing
        unlisten conn "valiant_test_silent"

    it "ignores notifications on other channels" $ do
      withTestConnection $ \listenerConn -> withTestConnection $ \senderConn -> do
        listen listenerConn "valiant_test_a"
        _ <- simpleQuery senderConn "NOTIFY valiant_test_b, 'wrong-channel'"
        threadDelay 50000
        result <- waitForNotificationTimeout listenerConn 0.3
        result `shouldBe` Nothing
        unlisten listenerConn "valiant_test_a"

  describe "channel name escaping" $ do
    it "accepts channel names that need quoting" $ do
      withTestConnection $ \conn -> do
        -- Mixed-case names require quoting; the LISTEN/UNLISTEN helpers
        -- quote identifiers automatically.
        listen conn "MixedCase"
        unlisten conn "MixedCase"
