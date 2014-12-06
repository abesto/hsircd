module Model.Prefix where

data Prefix = ServerNamePrefix String
            | UserPrefix { upNickname :: String, upUser :: Maybe String, upHost :: Maybe String}
            deriving (Eq, Show)
