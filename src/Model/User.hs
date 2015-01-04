{-# LANGUAGE DeriveDataTypeable #-}
module Model.User where

import Data.Data

type Nickname = String

data UserFlags = UserFlags { ufWallops :: Bool
                           , ufInvisible :: Bool
                           }
                 deriving (Data, Typeable)

mkUserFlags :: String -> UserFlags
mkUserFlags s = UserFlags { ufWallops = isSet 2
                          , ufInvisible = isSet 3
                          }
  where n = (read s) :: Int
        isSet :: Integer -> Bool
        isSet x = n `mod` (2 ^ x) == 0

data User = UnregisteredUser
          | NicknameOnlyUser { nuNickname :: Nickname }
          | UserOnlyUser { uuUsername :: String
                         , uuRealname :: String
                         , uuHost :: String
                         , uuFlags :: UserFlags
                         }
          | FullUser { uNickname :: Nickname
                     , uUsername :: String
                     , uRealname :: String
                     , uHost :: String
                     , uFlags :: UserFlags
                     }
            deriving (Data, Typeable)

-- TODO move `error`s to the type system

unsupportedUser :: String -> User -> a
unsupportedUser s u = error $ s ++ (showConstr $ toConstr u)

instance Show User where
  show (FullUser n u _ h _) = n ++ "!" ++ u ++ "@" ++ h
  show u = unsupportedUser "Can only show FullUser, not " u

nickname :: User -> Nickname
nickname u@(FullUser _ _ _ _ _) = uNickname u
nickname u@(NicknameOnlyUser _) = nuNickname u
nickname u = unsupportedUser "Tried to call nickname on a " u

changeNickname :: Nickname -> User -> User
changeNickname n UnregisteredUser = NicknameOnlyUser n
changeNickname n (NicknameOnlyUser _) = NicknameOnlyUser n
changeNickname n (UserOnlyUser u r h f) = FullUser n u r h f
changeNickname n u@(FullUser _ _ _ _ _ ) = u { uNickname = n }

addUserData :: User -> String -> String -> String -> User
addUserData UnregisteredUser username mode realname = UserOnlyUser { uuUsername = username
                                                                   , uuRealname = realname
                                                                   , uuHost = "Host lookup not implemented"
                                                                   , uuFlags = mkUserFlags mode
                                                                   }
addUserData (NicknameOnlyUser nick) username mode realname = changeNickname nick $ addUserData UnregisteredUser username mode realname
addUserData u _ _ _ = unsupportedUser "Tried to call addUserData on a " u
