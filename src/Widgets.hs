{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetMenu
  , widgetUser
  ) where


import Database.Esqueleto.Experimental (selectOne, from, table)
import Database.Persist (Entity (Entity))

import Foundation ()
import Foundation.Data
    ( Widget
    , Route
      ( HomeR, DataR, DocsR, AuthR, AccountR, AccountPhotoR, MyContactsR, CallsR )
    , DataR (UsersR, TokensR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers, MsgDocumentation
      , MsgSourceCode, MsgResources, MsgSignIn, MsgUserAccount, MsgSignOut
      , MsgPhoto, MsgChats, MsgCalls, MsgContacts
      )
    )

import Model (Call)

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (MonadHandler(liftHandler))
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist.Core (YesodPersist(runDB))


widgetMenu :: Widget
widgetMenu = do
    
    user <- maybeAuth
    curr <- getCurrentRoute

    oneCall <- liftHandler $ runDB $ selectOne $ from $ table @Call
    
    $(widgetFile "widgets/menu")


widgetUser :: Widget
widgetUser = do
    user <- maybeAuth
    $(widgetFile "widgets/user")
