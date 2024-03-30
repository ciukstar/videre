{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , AppMessage
      ( MsgAppDocumentation
      )
    )

import Menu (menu)
import Model (statusError)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgAppDocumentation
        $(widgetFile "resources/docs")
