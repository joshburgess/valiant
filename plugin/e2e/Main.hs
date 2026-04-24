-- | If this compiles, the plugin is working: queryFile calls were
-- rewritten to mkStatement, and type signatures were validated
-- against the .valiant/ cache.
module Main where

import PluginE2E

main :: IO ()
main = do
  -- Just verify the statements exist and have the right SQL embedded.
  -- The real validation happened at compile time.
  let _ = findUserById
  let _ = listUsers
  let _ = insertUser
  let _ = findByNameAndStatus
  putStrLn "Plugin e2e: all queryFile calls compiled successfully."
