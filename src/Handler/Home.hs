{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Control.Monad (join)

import Data.Bifunctor (Bifunctor(second))

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&)), Value (unValue)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route (AccountPhotoR)
    , AppMessage (MsgNoUsersYet)
    )

import Model
    ( statusError
    , User (User), UserPhoto (UserPhoto)
    , EntityField (UserId, UserPhotoUser, UserPhotoAttribution)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser)

import Yesod.Core (Yesod(defaultLayout), getMessages)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitle)
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do
    
    accounts <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        orderBy [desc (x ^. UserId)]
        return (x, h ?. UserPhotoAttribution) )

    msgs <- getMessages
    
    setUltDestCurrent
    defaultLayout $ do
        setTitle "Videre"
        $(widgetFile "homepage")

