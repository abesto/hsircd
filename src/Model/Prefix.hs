module Model.Prefix where

import Data.Maybe (maybe)

data Prefix = ServerNamePrefix String
            | UserPrefix { upNickname :: String, upUser :: Maybe String, upHost :: Maybe String}
            deriving (Eq, Show)

prefixToWire :: Prefix -> String
prefixToWire (ServerNamePrefix s) = s
prefixToWire (UserPrefix n u h) = n ++ (f '!' u) ++ (f '@' h)
  where f :: Char -> Maybe String -> String
        f c = maybe [] (c:)

