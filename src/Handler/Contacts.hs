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
  ) where

import ChatRoom
    ( YesodChat
      ( getBacklink, getAccountPhotoRoute, getContactRoute, getAppHttpManager
      , getStaticRoute, getVideoPushRoute, getVideoOutgoingRoute, getAppSettings
      )
    )
import ChatRoom.Data (Route (ChatRoomR))

import Control.Applicative (liftA3)
import Control.Lens ((.~), (?~))
import Control.Monad (join, forM_, forM)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON, object)
import qualified Data.Aeson as A
    ( object, Value (Bool), Result( Success, Error ), (.=) )
import Data.Bifunctor (Bifunctor(second, bimap, first))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (pack, Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just, update, set
    , (^.), (?.), (==.), (:&) ((:&)), (&&.), (!=.), (||.)
    , Value (unValue), innerJoin, val, where_, selectOne
    , delete, unionAll_
    )
import Database.Esqueleto.Experimental as E ((=.))
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, upsertBy
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ( PersistStoreWrite (delete), (=.) )
import Database.Persist.Sql (fromSqlKey)

import Foundation (Form, getVAPIDKeys)
import Foundation.Data
    ( Handler, App (appHttpManager, appSettings)
    , Route
      ( AccountPhotoR, ChatR, ContactsR, MyContactsR, ContactR, ContactRemoveR
      , PushSubscriptionsR, StaticR, VideoR, CallsR, CalleesR
      )
    , AppMessage
      ( MsgAppName, MsgContacts, MsgNoRegisteredUsersYet, MsgCalls
      , MsgAdd, MsgInvalidFormData, MsgNewContactsAdded, MsgViewContact
      , MsgContact, MsgPhoto, MsgDele, MsgConfirmPlease, MsgRecordDeleted
      , MsgSubscribeToNotifications, MsgNotGeneratedVAPID, MsgCancel
      , MsgRemoveAreYouSure, MsgSubscriptionSucceeded, MsgSubscriptionFailed
      , MsgRemove, MsgSubscriptionCanceled, MsgAllowToBeNotified, MsgNotNow
      , MsgAllow, MsgYourContactListIsEmpty, MsgYouMightWantToAddAFew
      , MsgAllowToBeNotifiedBy, MsgUserHasNotAddedYouToHisContactListYet
      , MsgYouAreNotSubscribedToNotificationsFrom, MsgSelectCalleeToCall
      , MsgYouHaveNotMadeAnyCallsYet, MsgOutgoingCall, MsgCallDeclined, MsgClose
      , MsgCalleeDeclinedTheCall, MsgIncomingAudioCallFrom, MsgVideoCall
      , MsgIncomingVideoCallFrom , MsgCallCanceledByCaller, MsgAudio
      , MsgAudioCall, MsgUserIsNowAvailable
      )
    )

import Material3 (md3mreq, md3switchField)

import Model
    ( statusError, statusSuccess, keyWebPushSubscriptionEndpoint
    , UserId, User (User, userName, userEmail), UserPhoto, Chat
    , ContactId, Contact (Contact), PushSubscription (PushSubscription)
    , Call (Call), CallType (CallTypeAudio, CallTypeVideo)
    , PushMsgType
      ( PushMsgTypeAudioCall, PushMsgTypeVideoCall, PushMsgTypeCancel
      , PushMsgTypeDecline, PushMsgTypeAccept, PushMsgTypeRefresh
      )
    , Unique (UniquePushSubscription)
    , CallStatus
      ( CallStatusAccepted, CallStatusDeclined, CallStatusCanceled, CallStatusEnded )
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, ContactOwner, ContactEntry
      , ContactId, ChatInterlocutor, ChatCreated, PushSubscriptionSubscriber
      , PushSubscriptionEndpoint
      , PushSubscriptionP256dh, PushSubscriptionAuth, PushSubscriptionPublisher
      , ChatUser, UserName, UserEmail, CallCaller, CallCallee
      , CallStart, ChatMessage, CallType, PushSubscriptionId
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
    )

import Text.Cassius (cassiusFile)
import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS), juliusFile)
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import VideoRoom
    ( YesodVideo
      ( getRtcPeerConnectionConfig, getAppHttpManager, getStaticRoute
      , getAppSettings
      )
    , Route (RoomR)
    )
import VideoRoom.Data (Route (PushMessageR))

import Web.WebPush
    ( VAPIDKeys, vapidPublicKeyBytes
    , mkPushNotification, pushMessage, sendPushNotification
    , pushSenderEmail, pushExpireInSeconds, pushTopic, PushTopic (PushTopic)
    , pushUrgency, PushUrgency (PushUrgencyLow)
    )

import Widgets (widgetMenu, widgetUser)

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, handlerToWidget, addMessageI
    , addMessage, toHtml, getYesod, invalidArgsI, MonadHandler (liftHandler)
    , ToWidget (toWidget), getMessageRender
    )
import Yesod.Core.Handler
    ( setUltDestCurrent, newIdent, redirect, lookupGetParam, sendStatusJSON
    , getUrlRender
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Input (runInputGet, ireq, runInputPost)
import Yesod.Form.Fields
    ( OptionList(olOptions), optionsPairs, multiSelectField, hiddenField
    , Option (optionInternalValue, optionExternalValue, optionDisplay), urlField, textField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Core.Json (parseCheckJsonBody, returnJson)
import Yesod.Form.Types
    ( Field (fieldView), FieldView (fvInput, fvLabel, fvId)
    , FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Static (StaticRoute)
import Yesod.Persist.Core (YesodPersist(runDB))


getCalleesR :: UserId -> Handler Html
getCalleesR uid = do

    endpoint <- lookupGetParam "endpoint"

    callees <- (bimap unValue (second (first (join . unValue))) <$>) <$> runDB ( select $ do

        x :& e :& h :& s :& a <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& s) -> ( just (e ^. UserId) ==. s ?. PushSubscriptionPublisher )

                                            &&. ( s ?. PushSubscriptionSubscriber ==. just (val uid) )
                                            &&. ( s ?. PushSubscriptionEndpoint ==. val endpoint )
            )
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& _ :& a) -> ( just (e ^. UserId) ==. a ?. PushSubscriptionSubscriber )

                                            &&. ( a ?. PushSubscriptionPublisher ==. just (val uid) )
            )
        where_ $ x ^. ContactOwner ==. val uid

        orderBy [desc (e ^. UserId)]
        return (x ^. ContactId, (e, (h ?. UserPhotoAttribution, (s, a)))) )

    rndr <- getUrlRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContacts
        idFabAdd <- newIdent
        toWidget $(cassiusFile "static/css/app-snackbar.cassius")
        toWidget $(juliusFile "static/js/app-snackbar.julius")
        $(widgetFile "calls/callees/callees")


getCallsR :: UserId -> Handler Html
getCallsR uid = do

    calls <- (second (first (bimap (second (join . unValue)) (second (join . unValue)))) <$>) <$> runDB ( select $ do

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

        where_ ( x ^. CallCaller ==. val uid ||. x ^. CallCallee ==. val uid )

        orderBy [desc (x ^. CallStart)]
        return (x, (((caller, callerPhoto ?. UserPhotoAttribution), (callee, calleePhoto ?. UserPhotoAttribution)), c)) )

    msgr <- getMessageRender
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCalls

        idFabAdd <- newIdent

        toWidget $(cassiusFile "static/css/app-snackbar.cassius")
        toWidget $(juliusFile "static/js/app-snackbar.julius")
        $(widgetFile "calls/calls")


putPushSubscriptionEndpointR :: Handler ()
putPushSubscriptionEndpointR = do    
    endpoint <- runInputPost $ ireq urlField "endpoint"
    p256dh <- runInputPost $ ireq textField "p256dh"
    auth <- runInputPost $ ireq textField "auth"
    oldEndpoint <- runInputPost $ ireq urlField "oldendpoint"

    runDB $ update $ \x -> do
        set x [ PushSubscriptionEndpoint E.=. val endpoint
              , PushSubscriptionP256dh E.=. val p256dh
              , PushSubscriptionAuth E.=. val auth
              ]
        where_ $ x ^. PushSubscriptionEndpoint ==. val oldEndpoint


deletePushSubscriptionsR :: UserId -> UserId -> Handler A.Value
deletePushSubscriptionsR sid pid = do

    endpoint <- lookupGetParam "endpoint"

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

      A.Success ps@(PushSubscription _ _ psEndpoint psKeyP256dh psKeyAuth) -> do
          _ <- runDB $ upsertBy (UniquePushSubscription sid pid psEndpoint) ps
               [ PushSubscriptionP256dh P.=. psKeyP256dh
               , PushSubscriptionAuth P.=. psKeyAuth
               ]
          addMessageI statusSuccess MsgSubscriptionSucceeded
          returnJson $ A.object [ "data" A..= A.object [ "success" A..= A.Bool True ] ]

      A.Error msg -> do
          addMessageI statusSuccess MsgSubscriptionFailed
          sendStatusJSON status400 (A.object [ "msg" A..= msg ])


formSubscribe :: VAPIDKeys -> UserId -> UserId -> ContactId -> Bool -> Form Bool
formSubscribe vapidKeys sid pid cid notif extra = do

    let subscriberId = pack $ show (fromSqlKey sid)
    let publisherId = pack $ show (fromSqlKey pid)

    publisherName <- liftHandler $ maybe "" ((\(x,y) -> fromMaybe y x) . bimap unValue unValue) <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val pid
        return (x ^. UserName, x ^. UserEmail) )

    (r,v) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("icons","")]
        } ( pure notif )

    idFormFieldSubscribe <- newIdent

    let applicationServerKey = vapidPublicKeyBytes vapidKeys
    return (r, $(widgetFile "my/contacts/push/subscription/form"))


postContactRemoveR :: UserId -> UserId -> ContactId -> Handler Html
postContactRemoveR uid rid cid = do
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
          redirect $ ContactR uid rid cid


getContactR :: UserId -> UserId -> ContactId -> Handler Html
getContactR sid pid cid = do

    endpoint <- lookupGetParam "endpoint"

    contact <- (second (first (join . unValue)) <$>) <$> runDB ( selectOne $ do
        x :& e :& h :& s :& s' <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)

            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& s) -> ( just (e ^. UserId) ==. s ?. PushSubscriptionPublisher )

                                            &&. ( s ?. PushSubscriptionSubscriber ==. just (val sid) )
                                            &&. ( s ?. PushSubscriptionEndpoint ==. val endpoint )
            )
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& _ :& s') -> ( just (e ^. UserId) ==. s' ?. PushSubscriptionSubscriber )

                                            &&. ( s' ?. PushSubscriptionPublisher ==. just (val sid) )
            )
        where_ $ x ^. ContactId ==. val cid
        return (e, (h ?. UserPhotoAttribution, (s, s'))) )

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          permission <- (\case Just _ -> True; Nothing -> False) <$> runDB ( selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionSubscriber ==. val sid
              where_ $ x ^. PushSubscriptionPublisher ==. val pid
              where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
              return x )

          (fw2,et2) <- generateFormPost $ formSubscribe vapidKeys sid pid cid permission

          (fw,et) <- generateFormPost formContactRemove

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgViewContact
              idDialogRemove <- newIdent

              toWidget $(cassiusFile "static/css/app-snackbar.cassius")
              toWidget $(juliusFile "static/js/app-snackbar.julius")
              $(widgetFile "my/contacts/contact")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContactRemove :: Form ()
formContactRemove extra = return (FormSuccess (),[whamlet|#{extra}|])


getMyContactsR :: UserId -> Handler Html
getMyContactsR uid = do

    endpoint <- lookupGetParam "endpoint"

    contacts <- (second (second (first (join . unValue))) <$>) <$> runDB ( select $ do
        x :& u :& p :& s :& a <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& s) -> ( just (e ^. UserId) ==. s ?. PushSubscriptionPublisher )

                                            &&. ( s ?. PushSubscriptionSubscriber ==. just (val uid) )
                                            &&. ( s ?. PushSubscriptionEndpoint ==. val endpoint )
            )
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& _ :& a) -> ( just (e ^. UserId) ==. a ?. PushSubscriptionSubscriber )

                                            &&. ( a ?. PushSubscriptionPublisher ==. just (val uid) )
            )
        where_ $ x ^. ContactOwner ==. val uid
        return (x, (u, (p ?. UserPhotoAttribution, (s, a)))) )

    entries <- (sortDescByTime <$>) <$> forM contacts $ \(c,(i@(Entity iid _),p)) -> do
        x <- (bimap unValue (bimap unValue (bimap unValue unValue)) <$>) <$> runDB ( selectOne $ do
            y@(time, (_, (_, _))) <- from $ ( do
                    z <- from $ ( do
                                      chat <- from $ table @Chat
                                      where_ $ chat ^. ChatUser ==. val uid
                                      where_ $ chat ^. ChatInterlocutor ==. val iid
                                      return chat
                                )
                         `unionAll_` ( do
                                           chat <- from $ table @Chat
                                           where_ $ chat ^. ChatUser ==. val iid
                                           where_ $ chat ^. ChatInterlocutor ==. val uid
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
        idFabAdd <- newIdent
        toWidget $(cassiusFile "static/css/app-snackbar.cassius")
        toWidget $(juliusFile "static/js/app-snackbar.julius")
        $(widgetFile "my/contacts/contacts")
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
    idDialogSubscribe <- newIdent

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          ((fr,fw),et) <- runFormPost $ formContacts
              uid users vapidKeys idFormPostContacts idDialogSubscribe Nothing

          case fr of
            FormSuccess (contacts, subscription, location) -> do
                now <- liftIO getCurrentTime
                forM_ contacts $ \(Entity eid _) -> do
                    _ <- runDB $ upsert (Contact uid eid now) []
                    case subscription of
                      Just (endpoint,p256dh,auth) ->  do
                          Entity sid _ <- runDB $ upsertBy (UniquePushSubscription uid eid endpoint)
                              (PushSubscription uid eid endpoint p256dh auth)
                              [ PushSubscriptionP256dh P.=. p256dh
                              , PushSubscriptionAuth P.=. auth
                              ]

                          subscriptions <- runDB $ select $ do
                              x <- from $ table @PushSubscription
                              where_ $ x ^. PushSubscriptionId !=. val sid
                              return x

                          Superuser {..} <- appSuperuser . appSettings <$> getYesod
                          manager <- appHttpManager <$> getYesod

                          msgr <- getMessageRender
                          urlr <- getUrlRender
                          user <- maybeAuth

                          forM_ subscriptions $ \(Entity _ (PushSubscription _ _ endpoint' p256dh' auth')) -> do
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
                                      & pushExpireInSeconds .~ 60
                                      & pushTopic ?~ (PushTopic . pack . show $ PushMsgTypeRefresh)
                                      & pushUrgency ?~ PushUrgencyLow

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
                $(widgetFile "contacts/contacts")
            FormMissing -> defaultLayout $ do
                setTitleI MsgContacts
                addMessageI statusError MsgInvalidFormData
                msgs <- getMessages
                idFabAdd <- newIdent
                backlink <- getUrlRender >>= \rndr -> return $ rndr $ MyContactsR uid
                $(widgetFile "contacts/contacts")
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
    backlink <- runInputGet $ ireq urlField "backlink"

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          (fw,et) <- generateFormPost $ formContacts
              uid users vapidKeys idFormPostContacts idDialogSubscribe (Just backlink)

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgContacts
              idFabAdd <- newIdent
              toWidget $(cassiusFile "static/css/app-snackbar.cassius")
              toWidget $(juliusFile "static/js/app-snackbar.julius")
              $(widgetFile "contacts/contacts")
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContacts :: UserId -> [Entity User] -> VAPIDKeys -> Text -> Text -> Maybe Text
             -> Form ([Entity User], Maybe (Text,Text,Text),Text)
formContacts uid options vapidKeys idFormPostContacts idDialogSubscribe backlink extra = do

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
           , $(widgetFile "contacts/form")
           )
  where
      option e@(Entity _ (User email _ _ _ _ _ _ _)) = (email,e)

      usersFieldList :: RenderMessage App msg => [(msg, Entity User)] -> Field Handler [Entity User]
      usersFieldList = usersField . optionsPairs

      usersField :: Handler (OptionList (Entity User)) -> Field Handler [Entity User]
      usersField ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs eval _idReq -> do
              opts <- olOptions <$> handlerToWidget ioptlist
              let optselected (Left _) _ = False
                  optselected (Right vals) opt = optionInternalValue opt `elem` vals
              [whamlet|
                <md-list ##{theId}>
                  $forall opt <- opts
                    <md-list-item type=button onclick="this.querySelector('md-checkbox').click()">
                      <img slot=start src=@{AccountPhotoR (entityKey $ optionInternalValue opt)}
                        width=56 height=56 loading=lazy style="clip-path:circle(50%)">

                      <div slot=headline style="white-space:nowrap">
                        $maybe name <- userName $ entityVal $ optionInternalValue opt
                          #{name}
                      <div slot=supporting-text style="white-space:nowrap">
                        #{optionDisplay opt}

                      <md-checkbox touch-target=wrapper slot=end onclick="event.stopPropagation()"
                        name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected eval opt:checked>
              |]
          }


instance YesodVideo App where

    getRtcPeerConnectionConfig :: Handler (Maybe A.Value)
    getRtcPeerConnectionConfig = getYesod <&> (appRtcPeerConnectionConfig . appSettings)

    getAppHttpManager :: Handler Manager
    getAppHttpManager = getYesod <&> appHttpManager

    getStaticRoute :: StaticRoute -> Handler (Route App)
    getStaticRoute = return . StaticR

    getAppSettings :: Handler AppSettings
    getAppSettings = getYesod >>= \app -> return $ appSettings app


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

    getAppSettings :: Handler AppSettings
    getAppSettings = getYesod >>= \app -> return $ appSettings app
