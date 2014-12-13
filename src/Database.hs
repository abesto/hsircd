module Database where

import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Data.Maybe

import Model.User

data UserData = UserData { udUser :: User
                         , udHandle :: Handle
                         -- , udHandleLock :: Lock
                         }
type UserDataStore = Map User UserData
type TestData = String

data Database = Database { dbUserData :: UserDataStore
                         , dbTest :: TestData
                         }
type Transaction = Database -> Database

mkDatabase :: IO (TVar Database)
mkDatabase = newTVarIO $ Database Map.empty "unset"

handleToUser :: Database -> Handle -> Maybe User
handleToUser (Database s _) h = listToMaybe $ Map.keys $ Map.filter ((== h) . udHandle) s
