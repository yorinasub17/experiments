{-# LANGUAGE OverloadedStrings #-}
module Kakeibo.Server.Views.Login where

import Data.Monoid ( mempty )
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Kakeibo.Server.Models.User

pageHead :: Html
pageHead = H.head $ do
    H.title "Kakeibo"

home :: User -> Html
home user = docTypeHtml $ do
    pageHead
    body $ do
        p "Foo"
        p $ H.toMarkup $ username user
        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty

login :: Html
login = docTypeHtml $ do
    pageHead
    body $ do
        H.form ! method "post" ! action "/login" $ do
            H.label ! for "ident" $ "Username"
            input ! type_ "text" ! name "ident"
            H.label ! for "password" $ "Password"
            input ! type_ "password" ! name "password" ! autocomplete "off"
            input ! type_ "submit" ! value "Log in"
