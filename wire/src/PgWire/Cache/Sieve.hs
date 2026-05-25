-- | SIEVE cache eviction algorithm.
--
-- A simple, efficient eviction policy that outperforms FIFO and
-- approaches LRU quality with lower overhead. Based on the NSDI'24
-- paper "SIEVE is Simpler than LRU."
--
-- On cache hit: set a visited bit (no list mutation).
-- On eviction: sweep from hand pointer, skip visited entries (reset
-- their bit), evict the first unvisited entry.
--
-- This implementation uses IORef-based mutable linked list nodes,
-- suitable for single-threaded per-connection use.
module PgWire.Cache.Sieve
  ( SieveCache
  , newSieveCache
  , lookup
  , insert
  , delete
  , size
  , clear
  , capacity
  , foldEntries
  ) where

import Prelude hiding (lookup)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | A SIEVE cache with keys @k@ and values @v@.
data SieveCache k v = SieveCache
  { scCapacity :: !Int
  , scSize :: !(IORef Int)
  , scMap :: !(IORef (Map k (Node k v)))
  , scHead :: !(IORef (Maybe (Node k v)))
  -- ^ Newest entry (insertion point).
  , scTail :: !(IORef (Maybe (Node k v)))
  -- ^ Oldest entry.
  , scHand :: !(IORef (Maybe (Node k v)))
  -- ^ Eviction sweep pointer. Starts at tail, moves toward head.
  }

-- | A node in the doubly-linked list.
data Node k v = Node
  { nodeKey :: !k
  , nodeVal :: !v
  , nodeVisited :: !(IORef Bool)
  , nodePrev :: !(IORef (Maybe (Node k v)))
  -- ^ Toward head (newer).
  , nodeNext :: !(IORef (Maybe (Node k v)))
  -- ^ Toward tail (older).
  }

-- | Create an empty SIEVE cache with the given capacity.
newSieveCache :: Int -> IO (SieveCache k v)
newSieveCache cap = do
  sz <- newIORef 0
  m <- newIORef Map.empty
  hd <- newIORef Nothing
  tl <- newIORef Nothing
  hand <- newIORef Nothing
  pure SieveCache
    { scCapacity = cap
    , scSize = sz
    , scMap = m
    , scHead = hd
    , scTail = tl
    , scHand = hand
    }

-- | Look up a key. On hit, marks the entry as visited.
lookup :: (Ord k) => SieveCache k v -> k -> IO (Maybe v)
lookup sc k = do
  m <- readIORef (scMap sc)
  case Map.lookup k m of
    Nothing -> pure Nothing
    Just node -> do
      writeIORef (nodeVisited node) True
      pure (Just (nodeVal node))

-- | Insert a key-value pair. If the cache is full, evicts an entry first.
-- If the key already exists, updates the value and marks as visited.
insert :: (Ord k) => SieveCache k v -> k -> v -> IO (Maybe (k, v))
  -- ^ Returns the evicted key and value, if any.
insert sc k v = do
  m <- readIORef (scMap sc)
  case Map.lookup k m of
    Just node -> do
      -- Key exists — update value and mark visited.
      deleteNode sc node
      insertFresh sc k v
    Nothing -> do
      sz <- readIORef (scSize sc)
      if sz >= scCapacity sc
        then do
          evicted <- evict sc
          insertNew sc k v
          pure evicted
        else do
          insertNew sc k v
          pure Nothing

-- | Delete a key from the cache.
delete :: (Ord k) => SieveCache k v -> k -> IO ()
delete sc k = do
  m <- readIORef (scMap sc)
  case Map.lookup k m of
    Nothing -> pure ()
    Just node -> deleteNode sc node

-- | Current number of entries.
size :: SieveCache k v -> IO Int
size sc = readIORef (scSize sc)

-- | The configured capacity of this cache.
capacity :: SieveCache k v -> Int
capacity = scCapacity

-- | Drop every entry. After 'clear', 'size' is 0 and the cache behaves
-- as if it had just been allocated.
clear :: SieveCache k v -> IO ()
clear sc = do
  writeIORef (scSize sc) 0
  writeIORef (scMap sc) Map.empty
  writeIORef (scHead sc) Nothing
  writeIORef (scTail sc) Nothing
  writeIORef (scHand sc) Nothing

-- | Walk every (key, value) pair currently in the cache. Order is
-- newest-first (insertion order at the head). Used by callers that
-- need to release server-side resources for cached entries on shutdown.
foldEntries :: SieveCache k v -> b -> (b -> k -> v -> IO b) -> IO b
foldEntries sc z f = do
  mHead <- readIORef (scHead sc)
  go z mHead
  where
    go acc Nothing = pure acc
    go acc (Just node) = do
      acc' <- f acc (nodeKey node) (nodeVal node)
      mNext <- readIORef (nodeNext node)
      go acc' mNext

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

-- | Insert a new entry at the head. Does NOT check capacity.
insertNew :: (Ord k) => SieveCache k v -> k -> v -> IO ()
insertNew sc k v = do
  visited <- newIORef False
  prev <- newIORef Nothing
  next <- newIORef Nothing
  let node = Node k v visited prev next

  mHead <- readIORef (scHead sc)
  case mHead of
    Nothing -> do
      -- Empty cache: node is both head and tail.
      writeIORef (scHead sc) (Just node)
      writeIORef (scTail sc) (Just node)
      writeIORef (scHand sc) (Just node)
    Just oldHead -> do
      -- Insert before old head.
      writeIORef (nodeNext node) (Just oldHead)
      writeIORef (nodePrev oldHead) (Just node)
      writeIORef (scHead sc) (Just node)

  modifyIORef' (scMap sc) (Map.insert k node)
  modifyIORef' (scSize sc) (+ 1)

-- | Insert fresh after a delete (for update). Returns Nothing.
insertFresh :: (Ord k) => SieveCache k v -> k -> v -> IO (Maybe (k, v))
insertFresh sc k v = do
  insertNew sc k v
  -- Mark as visited since it was just accessed.
  m <- readIORef (scMap sc)
  case Map.lookup k m of
    Just node -> writeIORef (nodeVisited node) True
    Nothing -> pure ()
  pure Nothing

-- | Remove a node from the linked list and map.
deleteNode :: (Ord k) => SieveCache k v -> Node k v -> IO ()
deleteNode sc node = do
  -- Fix hand pointer if it points to this node.
  mHand <- readIORef (scHand sc)
  case mHand of
    Just handNode | sameNode handNode node -> advanceHand sc node
    _ -> pure ()

  -- Unlink from doubly-linked list.
  mPrev <- readIORef (nodePrev node)
  mNext <- readIORef (nodeNext node)

  case mPrev of
    Just p -> writeIORef (nodeNext p) mNext
    Nothing -> writeIORef (scHead sc) mNext  -- node was head

  case mNext of
    Just n -> writeIORef (nodePrev n) mPrev
    Nothing -> writeIORef (scTail sc) mPrev  -- node was tail

  -- Remove from map and decrement size.
  modifyIORef' (scMap sc) (Map.delete (nodeKey node))
  modifyIORef' (scSize sc) (subtract 1)

-- | Evict one entry using the SIEVE sweep. Returns the evicted key and value.
evict :: (Ord k) => SieveCache k v -> IO (Maybe (k, v))
evict sc = do
  mHand <- readIORef (scHand sc)
  case mHand of
    Nothing -> pure Nothing
    Just startNode -> sweep sc startNode

-- | Sweep from the hand, skipping visited nodes (resetting their bit),
-- until finding an unvisited node to evict.
sweep :: (Ord k) => SieveCache k v -> Node k v -> IO (Maybe (k, v))
sweep sc node = do
  visited <- readIORef (nodeVisited node)
  if visited
    then do
      -- Give a second chance: reset visited bit, advance hand.
      writeIORef (nodeVisited node) False
      advanceHand sc node
      mNext <- readIORef (scHand sc)
      case mNext of
        Nothing -> pure Nothing  -- should not happen
        Just nextNode -> sweep sc nextNode
    else do
      -- Evict this node.
      let k = nodeKey node
          v = nodeVal node
      advanceHand sc node
      deleteNode sc node
      pure (Just (k, v))

-- | Advance the hand pointer. Moves toward the tail (next pointer).
-- If at the tail, wraps to the head.
advanceHand :: SieveCache k v -> Node k v -> IO ()
advanceHand sc node = do
  mNext <- readIORef (nodeNext node)
  case mNext of
    Just n -> writeIORef (scHand sc) (Just n)
    Nothing -> do
      -- At tail, wrap to head.
      mHead <- readIORef (scHead sc)
      writeIORef (scHand sc) mHead

-- | Check if two nodes are the same (by IORef identity of visited).
sameNode :: Node k v -> Node k v -> Bool
sameNode a b = nodeVisited a == nodeVisited b
  -- IORef equality is pointer equality.
