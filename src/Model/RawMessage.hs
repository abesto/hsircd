module Model.RawMessage where

import Model.Prefix
import Model.Command

data RawMessage = RawMessage (Maybe Prefix) Command [String]
                  deriving (Show, Eq)

mkRaw :: (Maybe Prefix) -> Command -> [String] -> RawMessage
mkRaw = RawMessage
