module Database where

import Data.Text

import Entities
import Database.Persist.Sql

type UserName = Text

getUser :: UserName -> SqlPersistT m (Maybe User)
getUser = undefined

storeUser :: User -> SqlPersistT m ()
storeUser = undefined
