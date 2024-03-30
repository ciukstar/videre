{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets
  ( widgetMenu
  , widgetUser
  ) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, just, val, exists
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR, AuthR, AccountR, AccountPhotoR)
    , DataR (UsersR, TokensR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers, MsgDocumentation
      , MsgSourceCode, MsgResources, MsgSignIn, MsgUserAccount, MsgSignOut
      , MsgPhoto
      )
    )
    
import Model
    ( User
    , EntityField(UserId)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (MonadHandler(liftHandler))
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist (YesodPersist(runDB))


widgetMenu :: Widget
widgetMenu = do
    curr <- getCurrentRoute

    user <- maybeAuth
    
    $(widgetFile "widgets/menu")


widgetUser :: Widget
widgetUser = do
    curr <- getCurrentRoute

    user <- maybeAuth
    
    
    $(widgetFile "widgets/user")
