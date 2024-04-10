{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Contacts
  ( getContactsR
  , postContactsR
  , getMyContactsR
  , getContactR
  , postContactRemoveR
  , postPushSubscriptionsR
  , deletePushSubscriptionsR
  ) where

import ChatRoom
    ( YesodChat
      ( getBacklink, getAccountPhotoRoute, getContactRoute, getAppHttpManager
      , getStaticRoute, getVideoPushRoute, getVideoOutgoingRoute
      )
    )
import ChatRoom.Data (Route (ChatRoomR))

import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON)
import qualified Data.Aeson as A
    ( object, Value (Bool), Result( Success, Error ), (.=) )
import Data.Bifunctor (Bifunctor(second, bimap, first))
import Data.Functor ((<&>))
import Data.Text (pack, unpack, Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&)), (&&.)
    , Value (unValue), innerJoin, val, where_, selectOne, max_
    , subSelectMaybe, delete
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, upsertBy, (=.)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ( PersistStoreWrite (delete) )
import Database.Persist.Sql (fromSqlKey)

import Foundation (Form)
import Foundation.Data
    ( Handler, App (appHttpManager, appSettings)
    , Route
      ( AccountPhotoR, ChatR, ContactsR, MyContactsR, ContactR, ContactRemoveR
      , PushSubscriptionsR, StaticR, VideoR
      )
    , AppMessage
      ( MsgNoContactsYet, MsgAppName, MsgContacts, MsgNoRegisteredUsersYet
      , MsgAdd, MsgInvalidFormData, MsgNewContactsAdded, MsgViewContact
      , MsgContact, MsgPhoto, MsgDele, MsgConfirmPlease, MsgRecordDeleted
      , MsgSubscribeToNotifications, MsgNotGeneratedVAPID, MsgCancel
      , MsgRemoveAreYouSure, MsgSubscriptionSucceeded, MsgSubscriptionFailed
      , MsgRemove, MsgSubscriptionCanceled, MsgAllowToBeNotified, MsgNotNow
      , MsgAllow
      )
    )

import Material3 (md3mreq, md3switchField)

import Model
    ( statusError, statusSuccess
    , UserId, User (User, userName), UserPhoto, Chat (Chat)
    , ContactId, Contact (Contact), PushSubscription (PushSubscription)
    , Token, Store
    , StoreType
      ( StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession )
    , apiInfoVapid, secretVolumeVapid, Unique (UniquePushSubscription)
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, ContactOwner, ContactEntry
      , ContactId, ChatInterlocutor, ChatCreated, PushSubscriptionSubscriber
      , PushSubscriptionEndpoint, TokenApi, TokenId, TokenStore, StoreToken
      , PushSubscriptionP256dh, PushSubscriptionAuth, PushSubscriptionPublisher
      , StoreVal, ChatUser
      )
    )

import Network.HTTP.Client.Conduit (Manager)
import Network.HTTP.Types.Status (status400)

import Settings (widgetFile, AppSettings (appRtcPeerConnectionConfig))

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (RenderMessage, SomeMessage (SomeMessage))

import VideoRoom
    ( YesodVideo (getRtcPeerConnectionConfig, getAppHttpManager)
    , Route (OutgoingR)
    )
import VideoRoom.Data (Route (PushMessageR))

import Web.WebPush
    ( VAPIDKeys, vapidPublicKeyBytes, readVAPIDKeys
    , VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    )

import Widgets (widgetMenu, widgetUser)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, handlerToWidget, addMessageI
    , addMessage, toHtml, getYesod, invalidArgsI
    )
import Yesod.Core.Handler
    ( setUltDestCurrent, newIdent, redirect, lookupGetParam, sendStatusJSON
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( OptionList(olOptions), optionsPairs, multiSelectField
    , Option (optionInternalValue, optionExternalValue, optionDisplay), hiddenField
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost, mopt)
import Yesod.Core.Json (parseCheckJsonBody, returnJson)
import Yesod.Form.Types
    ( Field (fieldView), FieldView (fvInput, fvLabel, fvId)
    , FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Static (StaticRoute)
import Control.Applicative (liftA3)


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
               [ PushSubscriptionP256dh =. psKeyP256dh
               , PushSubscriptionAuth =. psKeyAuth
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
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyContactsR uid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ ContactR uid rid cid


getVAPIDKeys :: Handler (Maybe VAPIDKeys)
getVAPIDKeys = do

    storeType <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val apiInfoVapid
        return (x ^. TokenId, x ^. TokenStore) )

    let readTriple (s,x,y) = VAPIDKeysMinDetails s x y

    details <- case storeType of
      Just (_, StoreTypeGoogleSecretManager) -> do
          liftIO $ (readTriple <$>) . readMaybe <$> readFile' secretVolumeVapid

      Just (tid, StoreTypeDatabase) -> do
          ((readTriple <$>) . readMaybe . unpack . unValue =<<) <$> runDB ( selectOne $ do
              x <-from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              return $ x ^. StoreVal )

      Just (_,StoreTypeSession) -> return Nothing
      Nothing -> return Nothing

    return $ readVAPIDKeys <$> details



getContactR :: UserId -> UserId -> ContactId -> Handler Html
getContactR sid pid cid = do

    contact <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& e :& h <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. ContactId ==. val cid
        return (e, h ?. UserPhotoAttribution) )

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          endpoint <- lookupGetParam "endpoint"

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
              $(widgetFile "my/contacts/contact")

      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContactRemove :: Form ()
formContactRemove extra = return (FormSuccess (),[whamlet|#{extra}|])


getMyContactsR :: UserId -> Handler Html
getMyContactsR uid = do

    endpoint <- lookupGetParam "endpoint"

    entries <- (bimap unValue (second (first (join . unValue))) <$>) <$> runDB ( select $ do

        x :& e :& h :& c :& s <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
            `leftJoin` table @Chat `on`
            (
                \(x :& _ :& _ :& c) -> ( (just (x ^. ContactEntry) ==. c ?. ChatUser)
                                         &&.
                                         (just (x ^. ContactOwner) ==. c ?. ChatInterlocutor)
                                       )
                &&.
                c ?. ChatCreated ==. subSelectMaybe ( do
                                                           y <- from $ table @Chat
                                                           where_ $ just (y ^. ChatUser) ==. c ?. ChatUser
                                                           where_ $ just (y ^. ChatInterlocutor) ==. c ?. ChatInterlocutor
                                                           return $ max_ (y ^. ChatCreated)
                                                     )
            )
            `leftJoin` table @PushSubscription `on`
            (
                \(_ :& e :& _ :& _ :& s) -> ( just (e ^. UserId) ==. s ?. PushSubscriptionPublisher )

                                            &&. ( s ?. PushSubscriptionSubscriber ==. just (val uid) )
                                            &&. ( s ?. PushSubscriptionEndpoint ==. val endpoint )
            )
        where_ $ x ^. ContactOwner ==. val uid

        orderBy [desc (e ^. UserId)]
        return (x ^. ContactId, (e, (h ?. UserPhotoAttribution, (c, s)))) )

    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAppName
        idFabAdd <- newIdent
        $(widgetFile "my/contacts/contacts")


postContactsR :: UserId -> Handler Html
postContactsR uid = do

    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x

    idFormPostContacts <- newIdent
    idDialogSubscribeToPushNotifications <- newIdent

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

        ((fr,fw),et) <- runFormPost $ formContacts users vapidKeys idFormPostContacts idDialogSubscribeToPushNotifications

        case fr of
          FormSuccess (contacts, subscription) -> do
              now <- liftIO getCurrentTime
              forM_ contacts $ \(Entity eid _) -> do
                  _ <- runDB $ upsert (Contact uid eid now) []
                  case subscription of
                    Just (endpoint,p256dh,auth) ->  do
                        _ <- runDB $ upsertBy (UniquePushSubscription uid eid endpoint)
                            (PushSubscription uid eid endpoint p256dh auth)
                            [ PushSubscriptionP256dh =. p256dh
                            , PushSubscriptionAuth =. auth
                            ]
                        return ()
                    Nothing -> return ()
              addMessageI statusSuccess MsgNewContactsAdded
              redirect $ MyContactsR uid
          FormFailure errs -> defaultLayout $ do
              setTitleI MsgContacts
              forM_ errs $ \err -> addMessage statusError (toHtml err)
              msgs <- getMessages
              idFabAdd <- newIdent
              $(widgetFile "contacts/contacts")
          FormMissing -> defaultLayout $ do
              setTitleI MsgContacts
              addMessageI statusError MsgInvalidFormData
              msgs <- getMessages
              idFabAdd <- newIdent
              $(widgetFile "contacts/contacts")
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


getContactsR :: UserId -> Handler Html
getContactsR uid = do

    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x

    idFormPostContacts <- newIdent
    idDialogSubscribeToPushNotifications <- newIdent

    mVAPIDKeys <- getVAPIDKeys

    case mVAPIDKeys of
      Just vapidKeys -> do

          (fw,et) <- generateFormPost $ formContacts users vapidKeys idFormPostContacts idDialogSubscribeToPushNotifications

          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgContacts
              idFabAdd <- newIdent
              $(widgetFile "contacts/contacts")
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]


formContacts :: [Entity User] -> VAPIDKeys -> Text -> Text -> Form ([Entity User], Maybe (Text,Text,Text))
formContacts options vapidKeys idFormPostContacts idDialogSubscribeToPushNotifications extra = do

    (endpointR, endpointV) <- mopt hiddenField "" Nothing
    (p256dhR, p256dhV) <- mopt hiddenField "" Nothing
    (authR, authV) <- mopt hiddenField "" Nothing
    (usersR,usersV) <- mreq (usersFieldList (option <$> options)) "" Nothing

    idButtonSubmitNoSubscription <- newIdent
    idButtonSubmitWithSubscription <- newIdent

    let applicationServerKey = vapidPublicKeyBytes vapidKeys

    return ( (,) <$> usersR <*> (liftA3 (,,) <$> endpointR <*> p256dhR <*> authR)
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

                      <div slot=headline>
                        $maybe name <- userName $ entityVal $ optionInternalValue opt
                          #{name}
                      <div slot=supporting-text>
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

    getVideoPushRoute :: Handler (Route App)
    getVideoPushRoute = return $ VideoR PushMessageR

    getVideoOutgoingRoute :: UserId -> UserId -> Handler (Route App)
    getVideoOutgoingRoute sid rid = return $ VideoR $ OutgoingR sid rid
