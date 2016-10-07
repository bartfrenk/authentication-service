{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.Word8 (Word8)

import Entities
import Utils (withExceptC)

-- DISCLAIMER: probably not resistant to timing attacks
-- TODO: Timing attacks that determine whether user account exists
data Credentials = Credentials
  { name :: Text
  , password :: Text
  } deriving (Eq, Show)

data Claim =
  Claim KeyID
        UserToken

data AuthErr
  = UnknownUserName Text
  | NonMatchingPassword
  | DuplicateUserName
  | InvalidUserName
  | InvalidPassword
  | SignError

newtype AuthToken =
  AuthToken ByteString

type UserToken = ByteString

type KeyID = Word8

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
createAuthToken keyID user = signClaim (createClaim keyID user)

createClaim :: KeyID -> User -> Claim
createClaim keyID user = Claim keyID (Entities.userToken user)

signClaim
  :: (MonadError AuthErr m, MonadIO m, MonadRandom m)
  => Claim -> m AuthToken
signClaim claim@(Claim keyID _) = do
  let enc = encodeClaim claim
  pk <- getPrivateKey keyID
  signature <- withExceptC (const SignError) (sign pk enc)
  return $ AuthToken (BS.append enc signature)
  where
    sign = PSS.sign Nothing (PSS.defaultPSSParams SHA256)

encodeClaim :: Claim -> ByteString
encodeClaim (Claim keyID token) = BS.cons keyID token
