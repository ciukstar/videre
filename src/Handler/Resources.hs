{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Resources (getDocsR) where

import Database.Persist (Entity(Entity))

import Foundation
    ( Handler, widgetMainMenu, widgetSnackbar, widgetAccount
    , Route (StaticR, HomeR, AuthR, DataR, OnlineChannelR)
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
      , MsgDoc019, MsgDoc020, MsgDoc021, MsgDoc022, MsgDoc023
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_ERD_Videre_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)
import Text.Julius (julius)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages, getUrlRender
    , getMessageRender, newIdent
    )
import Yesod.Core.Widget (setTitleI, toWidget)


getDocsR :: Handler Html
getDocsR = do

    user <- maybeAuth
    
    msgs <- getMessages
    r <- getUrlRender
    m <- getMessageRender
    let t = preEscapedToHtml . m
    let ws = "https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API"
        wrtc = "https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API"
        wp = "https://developer.mozilla.org/en-US/docs/Web/API/Push_API"
        
    setUltDestCurrent
    
    defaultLayout $ do
        setTitleI MsgAppDocumentation
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        
        $(widgetFile "resources/docs")
        
        case user of
          Just (Entity uid _) -> toWidget
            [julius|
                   const channel = new WebSocket('@{OnlineChannelR uid}');
                   |]
          Nothing -> return ()
