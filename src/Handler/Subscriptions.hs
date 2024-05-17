{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Subscriptions
  ( getSubscriptionsR
  , getUserSubscriptionsR
  ) where

import Control.Monad (join)

import Data.Bifunctor (second, Bifunctor (bimap))

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, leftJoin, on, just, orderBy, desc, unValue
    , (:&)((:&)), (^.), (==.), (?.), (>.)
    , SqlExpr, Value (Value), subSelectCount, where_, val, innerJoin
    )
import Database.Persist (Entity (Entity))

import Foundation ()
import Foundation.Data
    ( Handler
    , Route (DataR)
    , DataR (UserPhotoR, SubscriptionsR, UserSubscriptionsR)
    , AppMessage
      ( MsgSubscriptions, MsgNoSubscriptionsYet, MsgTheUserSubscriptions, MsgBack
      , MsgPhoto
      )
    )

import Model
    ( statusSuccess, statusError
    , PushSubscriptionId, PushSubscription (PushSubscription)
    , UserId, User (User), UserPhoto
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, PushSubscriptionPublisher
      )
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetMenu, widgetUser)

import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Handler (getMessages)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getUserSubscriptionsR :: UserId -> Handler Html
getUserSubscriptionsR uid = do


    user <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. UserId ==. val uid
        return (x, h ?. UserPhotoAttribution) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/user/subscriptions")


getSubscriptionsR :: Handler Html
getSubscriptionsR = do
    
    subscriptions <- (bimap (second (join . unValue)) unValue <$>) <$> runDB ( select $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)

        let count :: SqlExpr (Value Int)
            count = subSelectCount $ do
                ps <- from $ table @PushSubscription
                where_ $ ps ^. PushSubscriptionPublisher ==. x ^. UserId

        where_ $ count >. val 0
            
        orderBy [desc (x ^. UserId)]
        return ((x, h ?. UserPhotoAttribution), count) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/subscriptions")
