{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation
    ( Handler
    , Route (StaticR, HomeR, AuthR, DataR)
    , DataR (RingtonesR)
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgSourceCode, MsgSuperuser, MsgUsername
      , MsgPassword, MsgSearchEngineOptimization, MsgEmail, MsgClientId
      , MsgClientSecret, MsgUsage, MsgOverview, MsgBasicEntities, MsgUser
      , MsgContact, MsgPushSubscription, MsgEntityChat, MsgEntityCall
      , MsgConfiguration, MsgWebRealTimeCommunication, MsgRingtone
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc006
      , MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010, MsgDoc011, MsgDoc012
      , MsgDoc013, MsgDoc014, MsgDoc015, MsgDoc016, MsgDoc017, MsgDoc018
      , MsgDoc019, MsgDoc020, MsgDoc021
      )
    )

import Settings (widgetFile)

import Settings.StaticFiles (img_ERD_Videre_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser, widgetBanner, widgetSnackbar)

import Yesod.Auth (Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages, getUrlRender
    , getMessageRender
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    msgs <- getMessages
    r <- getUrlRender
    m <- getMessageRender
    let t = preEscapedToHtml . m
    let ws = "https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API"
        wrtc = "https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API"
        wp = "https://developer.mozilla.org/en-US/docs/Web/API/Push_API"
        
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgAppDocumentation
        $(widgetFile "resources/docs")
