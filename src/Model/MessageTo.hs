module Model.MessageTo where

import Model.Channel

data MessageTo = MessageToC   Channel
               | MessageToUHS String String String
               | MessageToUH  String String
               | MessageToUS  String String
               | MessageToN   String
               | MessageToNUH String String String
