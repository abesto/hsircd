module Model.Channel where

import Model.Encoding

data ChannelName = ChannelName String
    deriving Show

instance Eq ChannelName where
    (ChannelName a) == (ChannelName b) = (length a == length b) && (and $ zipWith charEquals a b)

data ChannelPrefix = ChannelPrefix Char
    deriving (Eq, Show)

data Channel = Channel {
    channelPrefix :: ChannelPrefix,
    channelName :: ChannelName
} deriving (Eq, Show)

channelToWire :: Channel -> String
channelToWire (Channel (ChannelPrefix p) (ChannelName n)) = p:n
