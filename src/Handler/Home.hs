{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, widgetMainMenu, widgetAccount
    , Route (AuthR, MyContactsR)
    , AppMessage
      ( MsgWelcomeTo, MsgAppName, MsgSendMessagesVideochatOrAudioCall
      , MsgLoginToAccessYourContacts, MsgLogIn, MsgShowMyContacts
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (Yesod(defaultLayout), getMessages, newIdent)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAppName
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "homepage")

