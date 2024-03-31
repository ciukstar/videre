{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Contacts (getContactsR) where

import ChatRoom (YesodChat (getBacklink, getAccountPhotoRoute))
import ChatRoom.Data (Route (ChatRoomR))

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second))

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&)), Value (unValue)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, App
    , Route (AccountPhotoR, ChatR, ContactsR)
    , AppMessage (MsgNoUsersYet)
    )

import Model
    ( statusError
    , UserId, User (User), UserPhoto (UserPhoto)
    , EntityField (UserId, UserPhotoUser, UserPhotoAttribution)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser)

import Yesod.Core (Yesod(defaultLayout), getMessages)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitle)
import Yesod.Persist.Core (YesodPersist(runDB))


getContactsR :: UserId -> Handler Html
getContactsR uid = do
    
    accounts <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        orderBy [desc (x ^. UserId)]
        return (x, h ?. UserPhotoAttribution) )

    msgs <- getMessages
    
    setUltDestCurrent
    defaultLayout $ do
        setTitle "Videre"
        $(widgetFile "contacts/contacts")


instance YesodChat App where
    getBacklink :: UserId -> UserId -> Handler (Route App)
    getBacklink sid _ = return $ ContactsR sid

    getAccountPhotoRoute :: UserId -> Handler (Route App)
    getAccountPhotoRoute uid = return $ AccountPhotoR uid
