{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Database.Persist (Entity (Entity))

import Foundation.Data
    ( Handler
    , Route (AuthR, MyContactsR)
    , AppMessage
      ( MsgWelcomeTo, MsgAppName, MsgSendMessagesVideochatOrAudioCall
      , MsgLoginToAccessYourContacts, MsgLogIn, MsgShowMyContacts
      )
    )

import Model
    ( statusError
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (Yesod(defaultLayout), getMessages)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitle)


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitle "Videre"
        $(widgetFile "homepage")

