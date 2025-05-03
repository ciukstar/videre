{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetUser
  , widgetBanner
  ) where

import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route
      ( AuthR, AccountR, AccountPhotoR
      , AccountRingtonesR
      )
    , AppMessage
      ( MsgSignIn, MsgUserAccount, MsgSignOut, MsgPhoto
      , MsgSettings
      )
    )

import Model
    ( statusError
    , RingtoneType (RingtoneTypeCallOutgoing)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Data.Text (Text)
import Text.Hamlet (Html)


widgetBanner :: [(Text, Html)] -> Widget
widgetBanner msgs = do
    $(widgetFile "widgets/banner")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
