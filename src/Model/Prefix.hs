module Model.Prefix where

import Model.User

data Prefix = ServerNamePrefix String
            | UserPrefix { upNickname :: String, upUser :: Maybe String, upHost :: Maybe String}
            deriving (Eq, Show)

prefixToWire :: Prefix -> String
prefixToWire (ServerNamePrefix s) = s
prefixToWire (UserPrefix n u h) = ':' : n ++ f '!' u ++ f '@' h
  where f :: Char -> Maybe String -> String
        f c = maybe [] (c:)

prefixFromUser :: User -> Prefix
prefixFromUser (FullUser n u _ h _) = UserPrefix n (Just u) (Just h)
prefixFromUser (NicknameOnlyUser n) = UserPrefix n Nothing Nothing
prefixFromUser UnregisteredUser = error $ "Can't create prefix from UnregisteredUser"
prefixFromUser (UserOnlyUser _ _ _ _) = error $ "Can't create prefix from UserOnlyUser"
