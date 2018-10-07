{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Map.Lazy
import Data.Time.Clock.POSIX

import GHC.Conc.Sync

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256     as SHA256

-- | Globals
difficulty = 5

numThreads = 4

referenceHash = fst . B16.decode . C8.pack $ zeros <> effs
  where zeros = take difficulty $ repeat '0'
        effs  = take (64 - difficulty) $ repeat 'f'

-- | Hashcash Primitives
data Hashcash = Hashcash
  { prevHash   :: BS.ByteString  -- Previous Hash
  , hashcashNo :: Integer        -- Number X in chain
  , hcEpoch    :: Integer        -- Timestamp when this was solved (not hashed)
  , nonce      :: Integer        -- Nonce to satisify constraint
  } deriving (Show, Eq)

-- | Data Structure to store our successive chains
-- | of valid hashcashes as well as our current problem
data HashcashState = HashcashState
  { hcChain    :: Map BS.ByteString Hashcash
  , hcCurrent  :: Hashcash
  } deriving (Show, Eq)

-- | HashCashState (HCS) uses a monad transformer
-- | to store our successive chains of 
-- | valid hashcashes and to do I/O
type HCS a = StateT HashcashState IO a

-- | HashcashState for STM is different from
-- a sequential state and requires different data structure
data HCThreadState = HCThreadState
  { threadI :: Integer
  , threadHC :: Hashcash -- Thread current hashcash
  }

-- | Hashcash State is global, and is encapsulated
-- | in a TVar
type HashcashSTM = TVar HashcashState

-- | Local State is encapsulated in a monad
-- | transformer
type STMHCS a = StateT HCThreadState IO a

-- | Get hash of hashcash primitive
-- | New Method (without base 16)
hcHash :: Hashcash -> BS.ByteString
hcHash h = digest
  where
    i2s = (C8.pack . show)    -- Integer to String
    ctx0 = SHA256.init
    ctx1 = SHA256.update ctx0 (prevHash h)
    ctx2 = SHA256.update ctx1 (i2s $ hashcashNo h)
    ctx3 = SHA256.update ctx2 (i2s $ nonce h)
    digest = SHA256.finalize ctx3

-- | Check hash's validity
--
validHash :: BS.ByteString -> Bool
validHash = (>=) referenceHash


-- | Helper function to increase nonce by <amount>
increaseNonce :: Integer -> Hashcash -> Hashcash
increaseNonce i h = h { nonce = i + n }
  where n = nonce h

-- | Sequentially solve for successive hashcashes
-- | Logic to find valid hashcash is also embedded here
hashcashSolver :: HCS ()
hashcashSolver = do
  -- Gets HashcashState
  state <- get    
  -- Increments nonce, gets hash, check if its valid
  let current     = increaseNonce 1 (hcCurrent state)
      currentHash = hcHash current
  case validHash currentHash of
    -- If valid hash, the update solved timestamp
    -- Output found hash
    True  -> do
      -- Get current unix time and insert it into solve block
      newEpoch <- liftIO $ round <$> getPOSIXTime
      -- Construct solved hashcash and new current hashcash
      -- and new state
      let solvedHashcash   = current { hcEpoch = newEpoch }
          newHcCurrent     = Hashcash currentHash (hashcashNo current + 1) newEpoch 0
          newHcChain       = insert currentHash solvedHashcash (hcChain state)
          newState         = HashcashState newHcChain newHcCurrent
          epochDifference  = newEpoch - (hcEpoch current)
      -- Analytics
      liftIO . putStrLn $ "Found solution for hashcashNo [" <> (show . hashcashNo) solvedHashcash <> "]"
      liftIO . putStrLn $ "    took (s): " <> (show $ epochDifference)
      liftIO . putStrLn $ "    nonce:    " <> (show $ nonce solvedHashcash)
      liftIO . putStrLn $ "    hash:     " <> (show $ B16.encode currentHash)
      if hashcashNo current < 10
         then do
            put newState
            hashcashSolver
         else return ()
    
    -- Else continue hashing (with updated current)
    False -> do
      -- Update current
      put $ state { hcCurrent = current }
      -- Continue on our merry way to find a valid hash
      hashcashSolver

-- | Hashcash solver with STM
hashcashSolverSTM :: HashcashSTM -> STMHCS ()
hashcashSolverSTM stmState = do
  -- Get global and local state
  globalState <- liftIO . atomically $ readTVar stmState
  threadState <- get
  -- Extract out variables
  let globalCurrent = hcCurrent globalState
      globalChain   = hcChain globalState
      threadCurrent = threadHC threadState
  -- If a new valid hash has been found
  -- Update thread current with global current
  -- then start finding the valid hash
  when (prevHash threadCurrent /= prevHash globalCurrent)
    (do 
      put (threadState { threadHC = (globalCurrent { nonce = threadI threadState } ) } )
      hashcashSolverSTM stmState
    )
  -- Increment nonce, get hash
  let newCurrent     = increaseNonce numThreads threadCurrent
      newCurrentHash = hcHash newCurrent
  -- Check if new hash is valid
  case validHash newCurrentHash of
    True -> do
      -- Get current epoch and insert it into solution
      newEpoch <- liftIO $ round <$> getPOSIXTime
      -- Construct new hashcash and new current hashcash
      -- and insert into global chain
      let solvedHashcash   = newCurrent { hcEpoch = newEpoch }
          newGlobalCurrent = Hashcash newCurrentHash (hashcashNo newCurrent + 1) newEpoch 0
          newGlobalChain   = insert newCurrentHash solvedHashcash globalChain
          newGlobalState   = HashcashState newGlobalChain newGlobalCurrent
          newThreadState   = threadState { threadHC = newGlobalCurrent }
          epochDifference  = newEpoch - (hcEpoch globalCurrent)
      -- Update thread state
      put newThreadState
      -- Update global state
      liftIO . atomically $ writeTVar stmState newGlobalState
      -- Analytics
      liftIO . putStrLn $ "Found solution for hashcashNo [" <> (show . hashcashNo) solvedHashcash <> "]"
      liftIO . putStrLn $ "    took (s): " <> (show $ epochDifference)
      liftIO . putStrLn $ "    nonce:    " <> (show $ nonce solvedHashcash)
      liftIO . putStrLn $ "    hash:     " <> (show $ B16.encode newCurrentHash)
      if hashcashNo solvedHashcash < 1
         then hashcashSolverSTM stmState
         else do
           liftIO $ putStrLn "quit!"
           return ()
    -- If invalid hash, continue hashing with
    -- updated nonce
    False -> do
      -- Update nonce
      put $ threadState { threadHC = newCurrent }
      -- Continue hashing
      hashcashSolverSTM stmState

-- | Genesis hashcash (number 0)
--
genesisChain = empty :: Map BS.ByteString Hashcash
genesisHashcash = Hashcash "0000000000000000000000000000000000000000000000000000000000000000" 0 0 0
genesisState = HashcashState genesisChain genesisHashcash

-- | Helper function to fork threads
--
forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    handle <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar handle ())
    putStrLn "Finally!"
    return handle

-- main :: IO ()
-- main = evalStateT hashcashSolver genesisState
main :: IO ()
main = do
  state <- atomically $ newTVar genesisState
  -- Fork N number of threads
  threads <- forM [1..numThreads] (\i -> forkThread $ evalStateT (hashcashSolverSTM state) (HCThreadState i $ genesisHashcash { nonce = i }))
  -- Wait till they're all done
  mapM_ takeMVar threads
