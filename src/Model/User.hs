module Model.User where

type Nickname = String

data User = UnregisteredUser
          | NicknameOnlyUser { nuNickname :: Nickname }
          | UserOnlyUser { uuUsername :: String
                         , uuRealname :: String
                         , uuHost :: String
                         , uuWallops :: Bool
                         , uuInvisible :: Bool
                         }
          | User { uNickname :: Nickname
                 , uUsername :: String
                 , uRealname :: String
                 , uHost :: String
                 , uWallops :: Bool
                 , uInvisible :: Bool
                 }

nickname :: User -> Nickname
nickname u@(User _ _ _ _ _ _) = uNickname u
nickname u@(NicknameOnlyUser _) = nuNickname u
nickname (UserOnlyUser _ _ _ _ _) = error "Tried to call nickname on a UserOnlyUser"
nickname UnregisteredUser = error "Tried to call nickname on an UnregisteredUser"

changeNickname :: Nickname -> User -> User
changeNickname n UnregisteredUser = NicknameOnlyUser n
changeNickname n (NicknameOnlyUser _) = NicknameOnlyUser n
changeNickname n (UserOnlyUser u r h w i) = User n u r h w i
changeNickname n u@(User _ _ _ _ _ _) = u { uNickname = n }
