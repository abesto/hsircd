module Model.Message where

import Data.List (intercalate)

import Model.Prefix
import Model.Command

data Message = Message { msgPrefix :: Maybe Prefix
                       , msgCommand :: Command
                       , msgParams :: [String]
                       }
               deriving (Eq, Show)

messageToWire :: Message -> String
messageToWire (Message p c ps) = intercalate " " $ filter (not . null) $ [(maybe "" prefixToWire p), cmdToWire c] ++ (paramsToWire ps)
  where paramsToWire [] = []
        paramsToWire [x] = [':':x]
        paramsToWire (x:xs) = (removeSpaces x) : (paramsToWire xs)
        removeSpaces = filter $ not . (== ' ')