{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Subscriptions
  ( getSubscriptionsR
  , getUserSubscriptionsR
  , getUserSubscriptionR
  , postUserSubscriptionDeleR
  ) where

import Control.Monad (join)

import Data.Bifunctor (second, Bifunctor (bimap))

import Database.Esqueleto.Experimental
    ( selectOne, select, from, table, leftJoin, on, just, orderBy, desc, unValue
    , (:&)((:&)), (^.), (==.), (?.), (>.)
    , SqlExpr, Value, subSelectCount, where_, val, innerJoin
    )
import Database.Persist (Entity (Entity))
import qualified Database.Persist as P (delete)

import Foundation
    ( Handler, Form, widgetMainMenu, widgetSnackbar
    , Route (DataR)
    , DataR
      ( UserPhotoR, SubscriptionsR, UserSubscriptionsR, UserSubscriptionR
      , UserSubscriptionDeleR
      )
    , AppMessage
      ( MsgSubscriptions, MsgNoSubscriptionsYet, MsgTheUserSubscriptions
      , MsgBack, MsgPhoto, MsgSubscription, MsgEndpoint, MsgUserAgent, MsgDele
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgCancel, MsgInvalidFormData
      , MsgRecordDeleted
      )
    )

import Model
    ( statusSuccess, statusError
    , PushSubscriptionId, PushSubscription (PushSubscription)
    , UserId, User (User), UserPhoto
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, PushSubscriptionPublisher
      , PushSubscriptionSubscriber, PushSubscriptionId
      )
    )
    
import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetUser, widgetBanner)

import Yesod.Core (Yesod(defaultLayout), whamlet, addMessageI, redirect, newIdent)
import Yesod.Core.Handler (getMessages)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form (FormResult(FormSuccess))
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))


postUserSubscriptionDeleR :: UserId -> PushSubscriptionId -> Handler Html
postUserSubscriptionDeleR uid sid = do
    ((fr,fw),et) <- runFormPost formUserSubscriptionDelete
    case fr of
      FormSuccess () -> do
          _ <- runDB $ P.delete sid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR $ UserSubscriptionsR uid
      _otherwise -> do     

          subscription <- runDB $ selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionId ==. val sid
              return x
        
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSubscriptions
              $(widgetFile "data/subscriptions/user/subscription")
              


getUserSubscriptionR :: UserId -> PushSubscriptionId -> Handler Html
getUserSubscriptionR uid sid = do

    subscription <- runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionId ==. val sid
        return x
    
    (fw,et) <- generateFormPost formUserSubscriptionDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        $(widgetFile "data/subscriptions/user/subscription")


formUserSubscriptionDelete :: Form ()
formUserSubscriptionDelete extra = return (pure () ,[whamlet|^{extra}|])


getUserSubscriptionsR :: UserId -> Handler Html
getUserSubscriptionsR uid = do

    user <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. UserId ==. val uid
        return (x, h ?. UserPhotoAttribution) )

    subscriptions <- (second (second (join . unValue)) <$>) <$> runDB ( select $ do
        x :& u :& h <- from $ table @PushSubscription
            `innerJoin` table @User `on` (\(x :& u) -> x ^. PushSubscriptionPublisher ==. u ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& u :& h) -> just (u ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. PushSubscriptionSubscriber ==. val uid
        return (x, (u, h ?. UserPhotoAttribution)) )
    
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
                where_ $ ps ^. PushSubscriptionSubscriber ==. x ^. UserId

        where_ $ count >. val 0
            
        orderBy [desc (x ^. UserId)]
        return ((x, h ?. UserPhotoAttribution), count) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSubscriptions
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/subscriptions/subscriptions")
