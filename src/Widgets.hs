{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( 
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
    ( RingtoneType (RingtoneTypeCallOutgoing)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Data.Text (Text)
import Text.Hamlet (Html)

