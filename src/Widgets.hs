{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetMenu
  , widgetUser
  , widgetBanner
  , widgetSnackbar
  ) where

import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route
      ( HomeR, DataR, DocsR, AuthR, AccountR, AccountPhotoR, MyContactsR, CallsR
      , AccountRingtonesR
      )
    , DataR (UsersR, TokensR, SubscriptionsR, RingtonesR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers, MsgDocumentation
      , MsgSourceCode, MsgResources, MsgSignIn, MsgUserAccount, MsgSignOut, MsgPhoto
      , MsgCalls, MsgContacts, MsgSubscriptions, MsgRingtones, MsgSettings
      )
    )

import Model
    (statusError, statusSuccess
    , User (User), RingtoneType (RingtoneTypeCallOutgoing)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Handler (getCurrentRoute)
import Data.Text (Text)
import Text.Hamlet (Html)


widgetBanner :: [(Text, Html)] -> Widget
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


widgetSnackbar :: [(Text, Html)] -> Widget
widgetSnackbar msgs = do
    $(widgetFile "widgets/snackbar")


widgetMenu :: Widget
widgetMenu = do
    
    user <- maybeAuth
    curr <- getCurrentRoute
    
    $(widgetFile "widgets/menu")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
