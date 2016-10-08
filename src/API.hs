{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API where

import Servant

import Authentication

type AuthenticationAPI
     = "authenticate" :> ReqBody '[JSON] Credentials
                      :> Get '[JSON] AuthToken
  :<|> "register"     :> ReqBody '[JSON] Credentials
                      :> Get '[JSON] UserToken

api :: Proxy AuthenticationAPI
api = Proxy
