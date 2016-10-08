module Types where

import Data.ByteString (ByteString)
import Data.Text
import Data.Word8 (Word8)

-- DISCLAIMER: probably not resistant to timing attacks
-- TODO: Timing attacks that determine whether user account exists
data Credentials = Credentials
  { name :: Text
  , password :: Text
  } deriving (Eq, Show)

data Claim =
  Claim UserToken

data AuthErr
  = UnknownUserName Text
  | NonMatchingPassword
  | DuplicateUserName
  | InvalidUserName
  | InvalidPassword
  | SignError

type Signature = ByteString

data AuthToken =
  AuthToken KeyID
            Claim
            Signature

type UserToken = ByteString

type KeyID = Word8
