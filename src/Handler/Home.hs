{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Foundation (Handler)

import Menu (menu)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitle)


getHomeR :: Handler Html
getHomeR = do
    
    defaultLayout $ do
        setTitle "Videre"
        $(widgetFile "homepage")

