{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetMenu
  , widgetUser
  ) where

import Database.Persist (Entity (Entity))

import Foundation ()
import Foundation.Data
    ( Widget
    , Route (HomeR, DataR, DocsR, AuthR, AccountR, AccountPhotoR)
    , DataR (UsersR, TokensR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers, MsgDocumentation
      , MsgSourceCode, MsgResources, MsgSignIn, MsgUserAccount, MsgSignOut
      , MsgPhoto
      )
    )
    
import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core.Handler (getCurrentRoute)


widgetMenu :: Widget
widgetMenu = do
    curr <- getCurrentRoute
    $(widgetFile "widgets/menu")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
