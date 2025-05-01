{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetUser
  , widgetBanner
  , widgetSnackbar
  ) where

import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route
      ( AuthR, AccountR, AccountPhotoR, MyContactsR
      , AccountRingtonesR
      )
    , DataR (UsersR, TokensR, SubscriptionsR, RingtonesR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers, MsgDocumentation
      , MsgSourceCode, MsgResources, MsgSignIn, MsgUserAccount, MsgSignOut, MsgPhoto
      , MsgContacts, MsgSubscriptions, MsgRingtones, MsgSettings
      )
    )

import Model
    (statusError, statusSuccess
    , RingtoneType (RingtoneTypeCallOutgoing)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Data.Text (Text)
import Text.Hamlet (Html)


widgetBanner :: [(Text, Html)] -> Widget
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


widgetSnackbar :: [(Text, Html)] -> Widget
widgetSnackbar msgs = do
    $(widgetFile "widgets/snackbar")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
