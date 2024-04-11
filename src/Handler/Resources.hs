{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Resources (getDocsR) where

import Foundation.Data
    ( Handler
    , Route (StaticR, HomeR)
    , AppMessage
      ( MsgAppDocumentation, MsgAppDescription, MsgErDiagram, MsgDocumentation
      , MsgAppName, MsgIssueTracking, MsgSourceCode, MsgSuperuser, MsgUsername
      , MsgPassword, MsgSearchEngineOptimization, MsgEmail, MsgClientId
      , MsgClientSecret
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005
      )
    )

import Model (statusError)

import Settings (widgetFile)

import Settings.StaticFiles (img_ERD_Videre_svg)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser)

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
