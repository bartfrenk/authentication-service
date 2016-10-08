{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word8 (Word8)
import GHC.Generics (Generic)

data AuthToken =
  AuthToken KeyID
            Claim
            Signature
  deriving (Show)

instance ToJSON AuthToken where
  toJSON (AuthToken keyID claim signature) =
    object
      [ "keyID" .= keyID
      , "claim" .= encodeAsText (encodeClaim claim)
      , "signature" .= encodeAsText signature
      ]

newtype UserToken =
  UserToken ByteString
  deriving (Eq, Show)

instance ToJSON UserToken where
  toJSON (UserToken bs) = object ["user" .= encodeAsText bs]

data Credentials = Credentials
  { name :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Credentials

data Claim =
  Claim ByteString
  deriving (Show)

encodeClaim :: Claim -> ByteString
encodeClaim (Claim token) = token

encodeAsText :: ByteString -> Text
encodeAsText = decodeLatin1 . B64.encode

data AuthErr
  = UnknownUserName Text
  | NonMatchingPassword
  | DuplicateUserName
  | InvalidUserName
  | InvalidPassword
  | SignError

type Signature = ByteString

type KeyID = Word8
