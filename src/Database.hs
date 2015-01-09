module Database where

import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import System.IO
import Data.Maybe

import Model.User

data UserData = UserData { udUser :: User
                         , udHandle :: Handle
                         -- , udHandleLock :: Lock
                         }

mkUserData :: Handle -> UserData
mkUserData = UserData UnregisteredUser

insertUserByNickname :: Nickname -> User -> Handle -> Map Nickname UserData -> Map Nickname UserData
insertUserByNickname n u h = Map.insert n (UserData u h)

data UserDataStore = UserDataStore { byNickname :: Map Nickname UserData }

insertUser :: UserDataStore -> User -> Handle -> UserDataStore
insertUser _ (UserOnlyUser _ _ _ _) _ = error "Tried to insert a UserOnlyUser into a UserDataStore"
insertUser s u@(NicknameOnlyUser n)   h = s { byNickname = insertUserByNickname n u h $ byNickname s}
insertUser s u h                        = s { byNickname = insertUserByNickname (uNickname u) u h $ byNickname s }

deleteUser :: UserDataStore -> Nickname -> UserDataStore
deleteUser s n = s { byNickname = Map.delete n $ byNickname s }

type TestData = String

data Database = Database { dbUserData :: UserDataStore
                         , dbTest :: TestData
                         }
type Transaction = Database -> Database

mkDatabase :: IO (TVar Database)
mkDatabase = newTVarIO $ Database { dbUserData = UserDataStore Map.empty
                                  , dbTest = "unset"
                                  }

handleToUser :: Database -> Handle -> Maybe User
handleToUser db h = fmap udUser $ listToMaybe $ Map.elems $ Map.filter ((== h) . udHandle) (byNickname $ dbUserData db)

userDataByNick :: Database -> Nickname -> Maybe UserData
userDataByNick db n = Map.lookup n (byNickname $ dbUserData db)

handleByNick :: Database -> Nickname -> Maybe Handle
handleByNick db n = fmap udHandle $ userDataByNick db n

userByNick :: Database -> Nickname -> Maybe User
userByNick db n = fmap udUser $ userDataByNick db n

isNicknameInUse :: Database -> Nickname -> Bool
isNicknameInUse db n = Map.member n $ byNickname $ dbUserData db

saveUser :: User -> Handle -> Database -> Database
saveUser u h db@(Database ud _) = db { dbUserData = insertUser ud u h }

freeNickname :: Nickname -> Database -> Database
freeNickname n db = db { dbUserData = deleteUser (dbUserData db) n }
