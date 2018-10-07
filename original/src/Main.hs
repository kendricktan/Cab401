{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.UnixTime
import Data.Map.Lazy

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256     as SHA256

-- | Globals
difficulty = 6

-- | Hashcash Primitives
data Hashcash = Hashcash { prevHash     :: BS.ByteString  -- Previous Hash
                         , hashcashNo   :: Integer        -- Number X in chain
                         , timestamp  :: UnixTime       -- Timestamp when this was solved (not hashed)
                         , nonce        :: Integer        -- Nonce to satisify constraint
                         } deriving (Show, Eq)

-- | Data Structure to store our successive chains
-- | of valid hashcashes as well as our current problem
data HashcashState = HashcashState { hcChain    :: Map BS.ByteString Hashcash
                                   , hcCurrent  :: Hashcash
                                   } deriving (Show, Eq)

-- | HashCashState (HCS) uses a monad transformer
-- | to store our successive chains of 
-- | valid hashcashes and to do I/O
type HCS a = StateT HashcashState IO a

-- | Get hash of hashcash primitive
--
hcHash :: Hashcash -> BS.ByteString
hcHash h = B16.encode digest
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
validHash = BS.isPrefixOf s
  where s = C8.pack $ take difficulty (repeat '0')

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
      ut <- liftIO getUnixTime
      -- Construct solved hashcash and new current hashcash
      -- and new state
      let solvedHashcash   = current { timestamp = ut }
          newHcCurrent     = Hashcash currentHash (hashcashNo current + 1) ut 0
          newHcChain       = insert currentHash solvedHashcash (hcChain state)
          newState         = HashcashState newHcChain newHcCurrent
      -- Analytics
      liftIO . putStrLn $ "Found solution for hashcashNo [" <> (show . hashcashNo) solvedHashcash <> "]"
      liftIO . putStrLn $ "    timestamp: " <> (show $ utSeconds . timestamp $ solvedHashcash)
      liftIO . putStrLn $ "    nonce:     " <> (show $ nonce solvedHashcash)
      liftIO . putStrLn $ "    hash:      " <> (C8.unpack currentHash)
      put newState
      hashcashSolver
    -- Else continue hashing (with updated current)
    False -> do
      -- Update current
      put $ state { hcCurrent = current }
      -- Continue on our merry way to find a valid hash
      hashcashSolver

-- | Genesis hashcash (number 0)
--
genesisChain = empty :: Map BS.ByteString Hashcash
genesisHashcash = Hashcash "0000000000000000000000000000000000000000000000000000000000000000" 0 (UnixTime 0 0) 0
genesisState = HashcashState genesisChain genesisHashcash

main :: IO ()
main = evalStateT hashcashSolver genesisState
