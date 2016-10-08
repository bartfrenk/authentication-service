{-# LANGUAGE FlexibleContexts #-}

module Authentication
  ( AuthToken
  , UserToken
  , Credentials(..)
  , AuthErr(..)
  , authenticate
  , register
  ) where

import Control.Monad.Except
import Crypto.Hash
import qualified Crypto.PubKey.RSA.PSS as PSS
import qualified Crypto.PubKey.RSA as RSA
import Crypto.Random
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Entities
import Types
import Utils (withExceptC)

register
  :: (MonadError AuthErr m, MonadIO m, MonadRandom m)
  => Credentials -> m User
register (Credentials nm pw) = do
  salt <- getRandomBytes 32
  token <- getRandomBytes 32
  now <- liftIO getCurrentTime
  let hashed = computeHash pw salt
  return $ User nm token hashed salt now

authenticate
  :: (MonadIO m, MonadError AuthErr m, MonadRandom m)
  => User -> Credentials -> m AuthToken
authenticate user creds =
  if verify user creds
    then do
      keyID <- getKeyID
      createAuthToken keyID user
    else throwError NonMatchingPassword

verify :: User -> Credentials -> Bool
verify user (Credentials nm pw) =
  let actual = computeHash pw (userSalt user)
      stored = userHash user
  in actual == stored && nm == userName user

computeHash :: Text -> ByteString -> ByteString
computeHash pw salt = BS.pack $ BA.unpack digest
  where
    digest :: Digest SHA256
    digest = hash $ BS.append (encodeUtf8 pw) salt

-- TODO: implement
getKeyID
  :: MonadIO m
  => m KeyID
getKeyID = return 0

-- TODO: implement
getPrivateKey
  :: MonadIO m
  => KeyID -> m RSA.PrivateKey
getPrivateKey _ = snd `fmap` liftIO get
  where
    get :: IO (RSA.PublicKey, RSA.PrivateKey)
    get = RSA.generate 256 65537

createAuthToken
  :: (MonadError AuthErr m, MonadIO m, MonadRandom m)
  => KeyID -> User -> m AuthToken
createAuthToken keyID user = do
  let claim = createClaim user
  signature <- signClaim keyID claim
  return $ AuthToken keyID claim signature

createClaim :: User -> Claim
createClaim user = Claim (Entities.userToken user)

signClaim
  :: (MonadError AuthErr m, MonadIO m, MonadRandom m)
  => KeyID -> Claim -> m Signature
signClaim keyID claim = do
  let enc = encodeClaim claim
  pk <- getPrivateKey keyID
  withExceptC (const SignError) (sign pk enc)
  where
    sign = PSS.sign Nothing (PSS.defaultPSSParams SHA256)

encodeClaim :: Claim -> ByteString
encodeClaim (Claim token) = token
