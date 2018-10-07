{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Map.Lazy
import Data.Time.Clock.POSIX

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256     as SHA256

-- | Globals
difficulty = 5

referenceHash = fst . B16.decode . C8.pack $ zeros <> effs
  where zeros = take difficulty $ repeat '0'
        effs  = take (64 - difficulty) $ repeat 'f'

-- | Hashcash Primitives
data Hashcash = Hashcash { prevHash   :: BS.ByteString  -- Previous Hash
                         , hashcashNo :: Integer        -- Number X in chain
                         , hcEpoch    :: Integer        -- Timestamp when this was solved (not hashed)
                         , nonce      :: Integer        -- Nonce to satisify constraint
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

-- | Genesis hashcash (number 0)
--
genesisChain = empty :: Map BS.ByteString Hashcash
genesisHashcash = Hashcash "0000000000000000000000000000000000000000000000000000000000000000" 0 0 0
genesisState = HashcashState genesisChain genesisHashcash

-- | Time 5 million hashes at difficulty 5
time5million :: IO ()
time5million = do
  let hh = [hcHash $ genesisHashcash { nonce = x } | x <- [0..]]
      gg = take 1000000 hh
      l = Prelude.length $ Prelude.filter validHash gg
  if l > 10000 then putStr "" else putStr ""
  return ()

-- | Main Input
--
main :: IO ()
main = evalStateT hashcashSolver genesisState
