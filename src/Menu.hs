{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Menu (menu) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, just, val, exists
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Widget
    , Route (HomeR, DataR, DocsR)
    , DataR (UsersR, TokensR)
    , AppMessage
      ( MsgWelcome, MsgTokens, MsgMainMenu, MsgData, MsgUsers
      , MsgDocumentation, MsgSourceCode, MsgResources
      )
    )
    
import Model
    ( User
    , EntityField(UserId)
    )

import Settings (widgetFile)

import Yesod.Auth (maybeAuth)
import Yesod.Core (MonadHandler(liftHandler))
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist (YesodPersist(runDB))

menu :: Widget
menu = do
    curr <- getCurrentRoute

    user <- maybeAuth
    
    $(widgetFile "menu")
