module Database where

import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import System.IO
import Data.Maybe

import Model.User

type Nick = String

data UserData = UserData { udNick :: Nick
                         , udHandle :: Handle
                         -- , udHandleLock :: Lock
                         }
type UserDataStore = Map Nick UserData
type TestData = String

data Database = Database { dbUserData :: UserDataStore
                         , dbTest :: TestData
                         }
type Transaction = Database -> Database

mkDatabase :: IO (TVar Database)
mkDatabase = newTVarIO $ Database Map.empty "unset"

handleToUser :: Database -> Handle -> Maybe User
handleToUser (Database s _) h = listToMaybe $ Map.keys $ Map.filter ((== h) . udHandle) s

isNicknameInUse :: Database -> Nick -> Bool
isNicknameInUse db n = Map.member n $ dbUserData db

saveNick :: Nick -> Handle -> Database -> Database
saveNick n h db@(Database ud _) = db { dbUserData = Map.insert n (UserData n h) ud}
