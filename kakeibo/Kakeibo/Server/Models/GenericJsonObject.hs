{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Kakeibo.Server.Models.GenericJsonObject where

import Data.Aeson
import Data.Text
import GHC.Generics ( Generic )

data JsonMessage = JsonMessage { message :: Text }
    deriving (Generic)
instance FromJSON JsonMessage
instance ToJSON JsonMessage
