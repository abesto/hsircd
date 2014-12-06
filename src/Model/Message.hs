module Model.Message where

import Model.Prefix
import Model.Command

data Message = Message { msgPrefix :: Maybe Prefix
                       , msgCommand :: Command
                       , msgParams :: [String]
                       }
               deriving (Eq, Show)
