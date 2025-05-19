{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Contacts
  ( getContactsR
  , postContactsR
  , getMyContactsR
  , getContactR
  , postContactRemoveR
  , postPushSubscriptionsR
  , deletePushSubscriptionsR
  , getCallsR
  , getCalleesR
  , putPushSubscriptionEndpointR
  , postPushSubscriptionsDeleR
  ) where

import ChatRoom
    ( YesodChat
      ( getBacklink, getAccountPhotoRoute, getContactRoute, getAppHttpManager
      , getStaticRoute, getVideoPushRoute, getVideoOutgoingRoute, getAppSettings
      , getUserRingtoneAudioRoute, getDefaultRingtoneAudioRoute, getVapidKeys
      )
    )
import ChatRoom.Data (Route (ChatRoomR))

import Control.Applicative (liftA3)
import Control.Lens ((.~), (?~))
import Control.Monad (join, forM_, forM)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON, object)
import qualified Data.Aeson as A
    ( object, Value (Bool), Result( Success, Error ), (.=)
    )
import Data.Bifunctor (Bifunctor(second, bimap, first))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just, update, set
    , (^.), (?.), (==.), (:&) ((:&)), (&&.), (!=.), (||.)
    , Value (unValue), innerJoin, val, where_, selectOne, delete, unionAll_
    , SqlExpr
    )
import Database.Esqueleto.Experimental as E ((=.), subSelectCount, Value (Value))
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, upsertBy
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ( PersistStoreWrite (delete), (=.) )
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( App (appHttpManager, appSettings), Handler, Form
    , getVAPIDKeys, widgetMainMenu, widgetSnackbar, widgetAccount
    , Route
      ( AccountPhotoR, ChatR, ContactsR, MyContactsR, ContactR, ContactRemoveR
      , PushSubscriptionsR, StaticR, VideoR, CallsR, PushSubscriptionsDeleR
      , CalleesR, HomeR, UserRingtoneAudioR, DefaultRingtoneAudioR
      )
    , AppMessage
      ( MsgAppName, MsgContacts, MsgNoRegisteredUsersYet, MsgCalls
      , MsgAdd, MsgInvalidFormData, MsgNewContactsAdded, MsgViewContact
      , MsgContact, MsgPhoto, MsgDele, MsgConfirmPlease, MsgRecordDeleted
      , MsgSubscribeToNotifications, MsgNotGeneratedVAPID, MsgCancel
      , MsgRemoveAreYouSure, MsgSubscriptionSucceeded, MsgSubscriptionFailed
      , MsgRemove, MsgSubscriptionCanceled, MsgSubscribe
      , MsgPostpone, MsgYourContactListIsEmpty, MsgYouMightWantToAddAFew
      , MsgAllowToBeNotifiedBy, MsgYouAreNotSubscribedToNotificationsFrom
      , MsgSelectCalleeToCall, MsgYouAreNotYetInContactListOfUser
      , MsgOutgoingCall, MsgCallDeclined, MsgClose, MsgCalleeDeclinedTheCall
      , MsgIncomingAudioCallFrom, MsgVideoCall, MsgIncomingVideoCallFrom
      , MsgCallCanceledByCaller, MsgAudioCall, MsgUserIsNowAvailable
      , MsgUserAppearsToBeUnavailable, MsgUserSubscribedOnThisDevice
      , MsgCancelThisSubscription, MsgAudio, MsgYouHaveNotMadeAnyCallsYet
      , MsgUserYouSeemsUnsubscribed, MsgCallerCalleeSubscriptionLoopWarning
      , MsgAllowToBeNotifiedBySelectedUsers, MsgBack, MsgUsers
      )
    )

import Material3 (md3widgetSwitch)

import Model
    ( statusError, statusSuccess
    , paramEndpoint, paramBacklink, localStorageEndpoint
    , UserId, User (User, userName, userEmail), UserPhoto
    , Chat (chatDelivered, chatRead)
    , ContactId, Contact (Contact), PushSubscription (PushSubscription)
    , Call (Call), CallType (CallTypeAudio, CallTypeVideo)
    , RingtoneId, Ringtone (Ringtone), UserRingtone
    , RingtoneType (RingtoneTypeCallOutgoing), DefaultRingtone
    , PushMsgType
      ( PushMsgTypeAudioCall, PushMsgTypeVideoCall, PushMsgTypeCancel
      , PushMsgTypeDecline, PushMsgTypeAccept, PushMsgTypeRefresh
      )
    , Unique (UniquePushSubscription)
    , CallStatus
      ( CallStatusAccepted, CallStatusDeclined, CallStatusCanceled, CallStatusEnded
      )
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, ContactOwner, ContactEntry
      , ContactId, ChatRecipient, ChatCreated, PushSubscriptionSubscriber
      , PushSubscriptionEndpoint
      , PushSubscriptionP256dh, PushSubscriptionAuth, PushSubscriptionPublisher
      , ChatAuthor, UserName, UserEmail, CallCaller, CallCallee
      , CallStart, ChatMessage, CallType, PushSubscriptionUserAgent, RingtoneId
      , UserRingtoneRingtone, UserRingtoneUser, UserRingtoneType, DefaultRingtoneType
      , DefaultRingtoneRingtone      
      )
    )

import Network.HTTP.Client.Conduit (Manager)
import Network.HTTP.Types.Status (status400)

import Settings
    ( widgetFile, AppSettings (appRtcPeerConnectionConfig, appSuperuser)
    , Superuser (Superuser, superuserUsername)
    )
import Settings.StaticFiles
    ( img_call_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    , img_notifications_24dp_FILL0_wght400_GRAD0_opsz24_svg
    , ringtones_outgoing_call_galaxy_ringtones_1_mp3
    )

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import VideoRoom
    ( YesodVideo
      ( getRtcPeerConnectionConfig, getAppHttpManager, getStaticRoute
      , getAppSettings, getHomeRoute, getVapidKeys
      )
    , Route (RoomR)
    )
import VideoRoom.Data (Route (PushMessageR))

import Web.WebPush
    ( VAPIDKeys, vapidPublicKeyBytes
    , mkPushNotification, pushMessage, sendPushNotification
    , pushSenderEmail, pushExpireInSeconds, pushTopic, PushTopic (PushTopic)
    , pushUrgency, PushUrgency (PushUrgencyHigh)
    )

import Yesod.Auth (maybeAuth, YesodAuth (maybeAuthId))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, handlerToWidget, addMessageI
    , addMessage, toHtml, getYesod, invalidArgsI, MonadHandler (liftHandler)
    , getMessageRender, lookupHeader, getCurrentRoute
    )
import Yesod.Core.Handler
    ( setUltDestCurrent, newIdent, redirect, lookupGetParam, sendStatusJSON
    , getUrlRender
    )
import Yesod.Core.Widget (setTitleI, whamlet, toWidget)
import Yesod.Form.Input (runInputGet, ireq, runInputPost, iopt)
import Yesod.Form.Fields
    ( OptionList(olOptions), optionsPairs, multiSelectField, hiddenField
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    , urlField, textField, checkBoxField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Core.Json (parseCheckJsonBody, returnJson)
import Yesod.Form.Types
    ( Field (fieldView), FieldView (fvInput, fvId)
    , FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsTooltip, fsId, fsName, fsAttrs, fsLabel)
    )
import Yesod.Static (StaticRoute)
import Yesod.Persist.Core (YesodPersist(runDB))


getCalleesR :: UserId -> Handler Html
getCalleesR uid = do

    endpoint <- lookupGetParam paramEndpoint

    caller <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    callees <- (unwrap <$>) <$> runDB ( select $ do

        x :& e :& h <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactOwner
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactEntry
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint

        where_ $ x ^. ContactOwner ==. val uid

        orderBy [desc (e ^. UserId)]
        return (x ^. ContactId, (e, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

    outgoingCallRingtone <- resolveOutgoingCallRingtone uid

    rndr <- getUrlRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContacts
        idAudioOutgoingCallRingtone <- newIdent
        $(widgetFile "calls/callees/callees")
  where
      unwrap = bimap unValue (second (bimap (join . unValue) (bimap unValue (bimap unValue unValue))))


getCallsR :: UserId -> Handler Html
getCallsR uid = do

    endpoint <- lookupGetParam paramEndpoint
    
    calls <- (unwrap <$>) <$> runDB ( select $ do

        x :& caller :& callerPhoto :& callee :& calleePhoto :& c <- from $ table @Call
            `innerJoin` table @User
            `on` (\(x :& caller) -> x ^. CallCaller ==. caller ^. UserId)
            `leftJoin` table @UserPhoto
            `on` (\(_ :& caller :& callerPhoto) -> just (caller ^. UserId) ==. callerPhoto ?. UserPhotoUser)
            `innerJoin` table @User
            `on` (\(x :& _ :& _ :& callee) -> x ^. CallCallee ==. callee ^. UserId)
            `leftJoin` table @UserPhoto
            `on` (\(_ :& _ :& _ :& callee :& calleePhoto) -> just (callee ^. UserId) ==. calleePhoto ?. UserPhotoUser)
            `innerJoin` table @Contact
            `on` ( \(x :& _ :& _ :& _ :& _ :& c) ->
                     ( c ^. ContactOwner ==. val uid )
                       &&. ( (c ^. ContactEntry ==. x ^. CallCaller) ||. (c ^. ContactEntry ==. x ^. CallCallee) )
                 )

        let subscriptions1 :: SqlExpr (Value Int)
            subscriptions1 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCallee
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCaller
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let loops1 :: SqlExpr (Value Int)
            loops1 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCaller
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCallee
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let accessible1 :: SqlExpr (Value Int)
            accessible1 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCaller
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCallee
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint

        let subscriptions2 :: SqlExpr (Value Int)
            subscriptions2 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCaller
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCallee
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let loops2 :: SqlExpr (Value Int)
            loops2 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCallee
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCaller
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let accessible2 :: SqlExpr (Value Int)
            accessible2 = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. CallCallee
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. CallCaller
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint

        where_ ( x ^. CallCaller ==. val uid ||. x ^. CallCallee ==. val uid )

        orderBy [desc (x ^. CallStart)]
        return ( x
               , ( ((caller, callerPhoto ?. UserPhotoAttribution), (callee, calleePhoto ?. UserPhotoAttribution))
                 , (c,((subscriptions1,(loops1, accessible1)),(subscriptions2,(loops2, accessible2))))
                 )
               ) )

    outgoingCallRingtone <- resolveOutgoingCallRingtone uid

    msgr <- getMessageRender
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCalls
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        idAudioOutgoingCallRingtone <- newIdent
        $(widgetFile "calls/calls")
  where
      unwrap = second (first (bimap (second (join . unValue)) (second (join . unValue))))


resolveOutgoingCallRingtone :: UserId -> Handler (Route App, Text)
resolveOutgoingCallRingtone uid = do
        userOutgoingCallRingtone <- runDB $ selectOne $ do
            x :& t <- from $ table @Ringtone `innerJoin` table @UserRingtone
                `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. UserRingtoneRingtone)
            where_ $ t ^. UserRingtoneUser ==. val uid
            where_ $ t ^. UserRingtoneType ==. val RingtoneTypeCallOutgoing
            return x
        case userOutgoingCallRingtone of
          Just (Entity rid (Ringtone _ mime _)) -> return (UserRingtoneAudioR uid rid, mime)
          Nothing -> do
              defaultOutgoingCallRingtone <- runDB $ selectOne $ do
                  x :& t <- from $ table @Ringtone `innerJoin` table @DefaultRingtone
                      `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. DefaultRingtoneRingtone)
                  where_ $ t ^. DefaultRingtoneType ==. val RingtoneTypeCallOutgoing
                  return x
              case defaultOutgoingCallRingtone of
                Just (Entity rid (Ringtone _ mime _)) -> return (DefaultRingtoneAudioR rid, mime)
                Nothing -> return (StaticR ringtones_outgoing_call_galaxy_ringtones_1_mp3, "audio/mpeg")


putPushSubscriptionEndpointR :: Handler ()
putPushSubscriptionEndpointR = do
    endpoint <- runInputPost $ ireq urlField paramEndpoint
    p256dh <- runInputPost $ ireq textField "p256dh"
    auth <- runInputPost $ ireq textField "auth"
    oldEndpoint <- runInputPost $ ireq urlField "oldendpoint"

    runDB $ update $ \x -> do
        set x [ PushSubscriptionEndpoint E.=. val endpoint
              , PushSubscriptionP256dh E.=. val p256dh
              , PushSubscriptionAuth E.=. val auth
              ]
        where_ $ x ^. PushSubscriptionEndpoint ==. val oldEndpoint


postPushSubscriptionsDeleR :: UserId -> ContactId -> UserId -> Handler Html
postPushSubscriptionsDeleR sid cid pid = do
    
    rndr <- getUrlRender
    backlink <- fromMaybe (rndr HomeR) <$> runInputGet ( iopt textField paramBacklink )

    ((fr,_),_) <- runFormPost $ formSubscriptionDelete Nothing

    case fr of
      FormSuccess endpoint -> do
          runDB $ delete $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionSubscriber ==. val sid
              where_ $ x ^. PushSubscriptionPublisher ==. val pid
              where_ $ x ^. PushSubscriptionEndpoint ==. val endpoint
          redirect (ContactR pid sid cid,[(paramBacklink, backlink)])
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (ContactR pid sid cid,[(paramBacklink, backlink)])


formSubscriptionDelete :: Maybe Text -> Form Text
formSubscriptionDelete endpoint extra = do
    (endpointR, endpointV) <- mreq hiddenField "" endpoint
    return (endpointR,[whamlet|^{extra} ^{fvInput endpointV}|])


deletePushSubscriptionsR :: UserId -> UserId -> Handler A.Value
deletePushSubscriptionsR sid pid = do

    endpoint <- lookupGetParam paramEndpoint

    case endpoint of
      Just x -> do
          runDB $ delete $ do
              y <- from $ table @PushSubscription
              where_ $ y ^. PushSubscriptionSubscriber ==. val sid
              where_ $ y ^. PushSubscriptionPublisher ==. val pid
              where_ $ y ^. PushSubscriptionEndpoint ==. val x
          addMessageI statusSuccess MsgSubscriptionCanceled
      Nothing -> do
          runDB $ delete $ do
              y <- from $ table @PushSubscription
              where_ $ y ^. PushSubscriptionSubscriber ==. val sid
              where_ $ y ^. PushSubscriptionPublisher ==. val pid
          addMessageI statusSuccess MsgSubscriptionCanceled
    returnJson $ A.object [ "data" A..= A.object [ "success" A..= A.Bool True ] ]


postPushSubscriptionsR :: UserId -> UserId -> Handler A.Value
postPushSubscriptionsR sid pid = do
    result <- parseCheckJsonBody
    case result of

      A.Success (PushSubscription _ _ psEndpoint psKeyP256dh psKeyAuth _) -> do
          ua <- (decodeUtf8 <$>) <$> lookupHeader "User-Agent"
          _ <- runDB $ upsertBy (UniquePushSubscription sid pid psEndpoint)
              (PushSubscription sid pid psEndpoint psKeyP256dh psKeyAuth ua)
              [ PushSubscriptionP256dh P.=. psKeyP256dh
              , PushSubscriptionAuth P.=. psKeyAuth
              , PushSubscriptionUserAgent P.=. ua
              ]
          addMessageI statusSuccess MsgSubscriptionSucceeded
          returnJson $ A.object [ "data" A..= A.object [ "success" A..= A.Bool True ] ]

      A.Error msg -> do
          addMessageI statusSuccess MsgSubscriptionFailed
          sendStatusJSON status400 (A.object [ "msg" A..= msg ])


formSubscribe :: Text -> VAPIDKeys -> UserId -> UserId -> ContactId -> Bool -> Form Bool
formSubscribe backlink vapidKeys sid pid cid notif extra = do

    let subscriberId = pack $ show (fromSqlKey sid)
    let publisherId = pack $ show (fromSqlKey pid)

    publisherName <- liftHandler $ maybe "" ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val pid
        return (x ^. UserName, x ^. UserEmail) )

    (r,v) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = []
        } ( pure notif )

    idFormFieldSubscribe <- newIdent

    let applicationServerKey = vapidPublicKeyBytes vapidKeys
    return (r, $(widgetFile "contacts/push/subscription/form")) 


postContactRemoveR :: UserId -> UserId -> ContactId -> Handler Html
postContactRemoveR uid rid cid = do
    
    rndr <- getUrlRender
    backlink <- fromMaybe (rndr HomeR) <$> runInputGet ( iopt textField paramBacklink )

    ((fr,_),_) <- runFormPost formContactRemove

    case fr of
      FormSuccess () -> do
          runDB $ P.delete cid
          runDB $ delete $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionPublisher ==. val rid
              where_ $ x ^. PushSubscriptionSubscriber ==. val uid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyContactsR uid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect (ContactR uid rid cid,[(paramBacklink, backlink)])


getContactR :: UserId -> UserId -> ContactId -> Handler Html
getContactR sid pid cid = do

    rndr <- getUrlRender
    backlink <- fromMaybe (rndr HomeR) <$> runInputGet ( iopt textField paramBacklink )
    endpoint <- lookupGetParam paramEndpoint

    contact <- (second (bimap (join . unValue) (bimap unValue (bimap unValue unValue))) <$>) <$> runDB ( selectOne $ do
        x :& e :& h <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactOwner
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactEntry
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint

        where_ $ x ^. ContactId ==. val cid
        return (e, (h ?. UserPhotoAttribution, (subscriptions, (loops, accessible)))) )

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          permission <- (\case Just _ -> True; Nothing -> False) <$> runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionSubscriber ==. val sid
              where_ $ x ^. PushSubscriptionPublisher ==. val pid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          (fw3,et3) <- generateFormPost $ formSubscriptionDelete endpoint
          (fw2,et2) <- generateFormPost $ formSubscribe backlink vapidKeys sid pid cid permission

          (fw0,et0) <- generateFormPost formContactRemove

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgViewContact
              idOverlay <- newIdent
              idDialogRemove <- newIdent
              $(widgetFile "contacts/contact")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContactRemove :: Form ()
formContactRemove extra = return (FormSuccess (),[whamlet|#{extra}|])


getMyContactsR :: UserId -> Handler Html
getMyContactsR uid = do

    endpoint <- lookupGetParam paramEndpoint

    contacts <- (second (second (bimap (join . unValue) (bimap unValue (bimap unValue unValue)))) <$>) <$> runDB ( select $ do
        x :& u :& p <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)

        let subscriptions :: SqlExpr (Value Int)
            subscriptions = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactOwner
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactEntry
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let loops :: SqlExpr (Value Int)
            loops = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) ==. val endpoint

        let accessible :: SqlExpr (Value Int)
            accessible = subSelectCount $ do
                y <- from $ table @PushSubscription
                where_ $ y ^. PushSubscriptionSubscriber ==. x ^. ContactEntry
                where_ $ y ^. PushSubscriptionPublisher ==. x ^. ContactOwner
                where_ $ just (y ^. PushSubscriptionEndpoint) !=. val endpoint

        where_ $ x ^. ContactOwner ==. val uid
        return (x, (u, (p ?. UserPhotoAttribution, (subscriptions, (loops, accessible))))) )

    entries <- (sortDescByTime <$>) <$> forM contacts $ \(c,(i@(Entity iid _),p)) -> do
        x <- (bimap unValue (bimap unValue (bimap unValue unValue)) <$>) <$> runDB ( selectOne $ do
            y@(time, (_, (_, _))) <- from $ ( do
                    z <- from $ ( do
                                      chat <- from $ table @Chat
                                      where_ $ chat ^. ChatAuthor ==. val uid
                                      where_ $ chat ^. ChatRecipient ==. val iid
                                      return chat
                                )
                         `unionAll_` ( do
                                           chat <- from $ table @Chat
                                           where_ $ chat ^. ChatAuthor ==. val iid
                                           where_ $ chat ^. ChatRecipient ==. val uid
                                           return chat
                                     )
                    return ( z ^. ChatCreated
                           , (z ^. ChatMessage
                             , ( val CallTypeAudio
                               , val False
                               )
                             )
                           ) )
                `unionAll_` ( do
                    z <- from $ ( do
                                      call <- from $ table @Call
                                      where_ $ call ^. CallCaller ==. val uid
                                      where_ $ call ^. CallCallee ==. val iid
                                      return call
                                )
                         `unionAll_` ( do
                                           call <- from $ table @Call
                                           where_ $ call ^. CallCaller ==. val iid
                                           where_ $ call ^. CallCallee ==. val uid
                                           return call
                                     )
                    return ( z ^. CallStart
                           , ( val ("Media call" :: Text)
                             , ( z ^. CallType
                               , val True
                               )
                             )
                           ) )
            orderBy [desc time]
            return y )
        return (c,(i,(p,x)))

    rndr <- getUrlRender
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAppName
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "contacts/contacts")
  where
      sortDescByTime = sortBy (\(Entity cid1 _,(_,(_,mt1))) (Entity cid2 _,(_,(_,mt2))) -> case (mt1,mt2) of
                                  (Just (t1,_), Just (t2,_)) -> compare t2 t1
                                  (Just _, Nothing) -> LT
                                  (Nothing, Just _) -> GT
                                  (Nothing, Nothing) -> compare cid1 cid2

                              )


postContactsR :: UserId -> Handler Html
postContactsR uid = do

    users <- runDB $ select $ do
        x <- from $ table @User
        where_ $ x ^. UserId !=. val uid
        orderBy [desc (x ^. UserId)]
        return x

    idFormPostContacts <- newIdent
    idOverlay <- newIdent
    idDialogSubscribe <- newIdent

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          ((fr,fw),et) <- runFormPost $ formContacts
              uid users vapidKeys idFormPostContacts idDialogSubscribe idOverlay Nothing

          case fr of
            FormSuccess (contacts, subscription, location) -> do
                now <- liftIO getCurrentTime
                forM_ contacts $ \(Entity eid _) -> do
                    _ <- runDB $ upsert (Contact uid eid now) []
                    case subscription of
                      Just (endpoint,p256dh,auth) ->  do
                          ua <- (decodeUtf8 <$>) <$> lookupHeader "User-Agent"
                          _ <- runDB $ upsertBy (UniquePushSubscription uid eid endpoint)
                              (PushSubscription uid eid endpoint p256dh auth ua)
                              [ PushSubscriptionP256dh P.=. p256dh
                              , PushSubscriptionAuth P.=. auth
                              , PushSubscriptionUserAgent P.=. ua
                              ]

                          subscriptions <- (bimap unValue (bimap unValue unValue) <$>) <$> runDB ( select $ do
                              x <- from $ table @PushSubscription
                              where_ $ x ^. PushSubscriptionPublisher ==. val uid
                              where_ $ x ^. PushSubscriptionSubscriber ==. val eid
                              return ( x ^. PushSubscriptionEndpoint
                                     , ( x ^. PushSubscriptionP256dh
                                       , x ^. PushSubscriptionAuth
                                       )
                                     ) )

                          Superuser {..} <- appSuperuser . appSettings <$> getYesod
                          manager <- appHttpManager <$> getYesod

                          msgr <- getMessageRender
                          urlr <- getUrlRender
                          user <- maybeAuth

                          forM_ subscriptions $ \(endpoint', (p256dh', auth')) -> do
                              let notification = mkPushNotification endpoint' p256dh' auth'
                                      & pushMessage .~ object
                                          [ "messageType" A..= PushMsgTypeRefresh
                                          , "title" A..= msgr MsgAppName
                                          , "icon" A..= urlr (StaticR img_notifications_24dp_FILL0_wght400_GRAD0_opsz24_svg)
                                          , "image" A..= (urlr . AccountPhotoR . entityKey <$> user)
                                          , "body" A..= ( msgr . MsgUserIsNowAvailable .
                                                          (\u -> fromMaybe (userEmail u) (userName u)) . entityVal <$> user
                                                        )
                                          , "senderId" A..= uid
                                          ]
                                      & pushSenderEmail .~ superuserUsername
                                      & pushExpireInSeconds .~ 30 * 60
                                      & pushTopic ?~ (PushTopic . pack . show $ PushMsgTypeRefresh)
                                      & pushUrgency ?~ PushUrgencyHigh

                              result <- sendPushNotification vapidKeys manager notification

                              case result of
                                Left ex -> do
                                    liftIO $ print ex
                                Right () -> return ()
                              return ()
                      Nothing -> return ()
                addMessageI statusSuccess MsgNewContactsAdded
                redirect location
                
            FormFailure errs -> defaultLayout $ do
                setTitleI MsgContacts
                forM_ errs $ \err -> addMessage statusError (toHtml err)
                msgs <- getMessages
                idFabAdd <- newIdent
                backlink <- getUrlRender >>= \rndr -> return $ rndr $ MyContactsR uid
                $(widgetFile "users/contacts")
                
            FormMissing -> defaultLayout $ do
                setTitleI MsgContacts
                addMessageI statusError MsgInvalidFormData
                msgs <- getMessages
                idFabAdd <- newIdent
                backlink <- getUrlRender >>= \rndr -> return $ rndr $ MyContactsR uid
                $(widgetFile "users/contacts")
                
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


getContactsR :: UserId -> Handler Html
getContactsR uid = do

    users <- runDB $ select $ do
        x <- from $ table @User
        where_ $ x ^. UserId !=. val uid
        orderBy [desc (x ^. UserId)]
        return x

    idFormPostContacts <- newIdent
    idDialogSubscribe <- newIdent
    idOverlay <- newIdent
    rndr <- getUrlRender
    backlink <- fromMaybe (rndr HomeR) <$> runInputGet ( iopt textField paramBacklink )

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          (fw,et) <- generateFormPost $ formContacts
              uid users vapidKeys idFormPostContacts idDialogSubscribe idOverlay (Just backlink)

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgContacts
              idFabAdd <- newIdent
              $(widgetFile "users/contacts")
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContacts :: UserId -> [Entity User] -> VAPIDKeys -> Text -> Text -> Text -> Maybe Text
             -> Form ([Entity User], Maybe (Text,Text,Text),Text)
formContacts uid options vapidKeys idFormPostContacts idDialogSubscribe idOverlay backlink extra = do

    (endpointR, endpointV) <- mopt hiddenField "" Nothing
    (p256dhR, p256dhV) <- mopt hiddenField "" Nothing
    (authR, authV) <- mopt hiddenField "" Nothing
    (locationR, locationV) <- mreq hiddenField "" backlink
    (usersR,usersV) <- mreq (usersFieldList (option <$> options)) "" Nothing

    idDialogSubscribeContent <- newIdent
    idButtonSubmitNoSubscription <- newIdent
    idButtonSubmitWithSubscription <- newIdent

    let applicationServerKey = vapidPublicKeyBytes vapidKeys

    return ( (,,) <$> usersR <*> (liftA3 (,,) <$> endpointR <*> p256dhR <*> authR) <*> locationR
           , $(widgetFile "users/form")
           )
  where
      option e@(Entity _ (User email _ _ _ _ _ _ _)) = (email,e)

      usersFieldList :: RenderMessage App msg => [(msg, Entity User)] -> Field Handler [Entity User]
      usersFieldList = usersField . optionsPairs

      usersField :: Handler (OptionList (Entity User)) -> Field Handler [Entity User]
      usersField ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs x isReq -> do
              opts <- olOptions <$> handlerToWidget ioptlist
              
              let sel (Left _) _ = False
                  sel (Right vals) opt = optionInternalValue opt `elem` vals
                  
              let iopts = zip [1 :: Int .. ] opts
              toWidget [cassius|
                  ##{theId}
                      div.content
                          min-width: 0
                          .headline, .supporting-text
                              white-space:nowrap
                              overflow: hidden
                              text-overflow: ellipsis
              |]
              [whamlet|
                <ul.list.border.large-space ##{theId} *{attrs}>
                  $forall (i,opt) <- iopts
                    <li.wave onclick="document.getElementById('#{theId}-#{i}').click()">
                      <img.circle src=@{AccountPhotoR (entityKey $ optionInternalValue opt)} loading=lazy alt=_{MsgPhoto}>
                      
                      <div.content.max>
                        <div.headline.large-text>
                          $maybe name <- userName $ entityVal $ optionInternalValue opt
                            #{name}
                        <div.supporting-text.secondary-text>
                          #{optionDisplay opt}

                      <label.checkbox>
                        <input type=checkbox ##{theId}-#{i} name=#{name} value=#{optionExternalValue opt}
                          :sel x opt:checked :isReq:required=true>
                        <span>
              |]
          }


instance YesodVideo App where

    getRtcPeerConnectionConfig :: Handler (Maybe A.Value)
    getRtcPeerConnectionConfig = getYesod <&> (appRtcPeerConnectionConfig . appSettings)

    getAppHttpManager :: Handler Manager
    getAppHttpManager = getYesod <&> appHttpManager

    getHomeRoute :: Handler Text
    getHomeRoute = do
        rndr <- getUrlRender
        rndr . fromMaybe HomeR <$> getCurrentRoute 

    getStaticRoute :: StaticRoute -> Handler (Route App)
    getStaticRoute = return . StaticR

    getAppSettings :: Handler AppSettings
    getAppSettings = getYesod >>= \app -> return $ appSettings app

    getVapidKeys :: Handler (Maybe VAPIDKeys)
    getVapidKeys = getVAPIDKeys


instance YesodChat App where

    getAppHttpManager :: Handler Manager
    getAppHttpManager = getYesod >>= \app -> return $ appHttpManager app

    getBacklink :: UserId -> UserId -> Handler (Route App)
    getBacklink sid _ = return $ MyContactsR sid

    getAccountPhotoRoute :: UserId -> Handler (Route App)
    getAccountPhotoRoute uid = return $ AccountPhotoR uid

    getContactRoute :: UserId -> UserId -> ContactId -> Handler (Route App)
    getContactRoute uid rid cid = return $ ContactR uid rid cid

    getStaticRoute :: StaticRoute -> Handler (Route App)
    getStaticRoute = return . StaticR

    getVideoPushRoute :: UserId -> ContactId -> UserId -> Handler (Route App)
    getVideoPushRoute sid cid rid = return $ VideoR $ PushMessageR sid cid rid

    getVideoOutgoingRoute :: UserId -> ContactId -> UserId -> Bool -> Handler (Route App)
    getVideoOutgoingRoute sid cid rid polite = return $ VideoR $ RoomR sid cid rid polite

    getUserRingtoneAudioRoute :: UserId -> RingtoneId -> Handler (Route App)
    getUserRingtoneAudioRoute uid rid = return $ UserRingtoneAudioR uid rid
    
    getDefaultRingtoneAudioRoute :: RingtoneId -> Handler (Route App)
    getDefaultRingtoneAudioRoute rid = return $ DefaultRingtoneAudioR rid

    getAppSettings :: Handler AppSettings
    getAppSettings = getYesod >>= \app -> return $ appSettings app

    getVapidKeys :: Handler (Maybe VAPIDKeys)
    getVapidKeys = getVAPIDKeys
