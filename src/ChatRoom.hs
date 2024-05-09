{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (ChatRoomR, AcknowledgeR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~),(?~))
import Control.Monad (forever, forM_)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set, select
    , (^.), (==.), (=.)
    , just, Value (unValue, Value), Entity (entityVal), not_, unionAll_, orderBy, desc
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert))
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Data.Aeson (object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Function ((&))
import qualified Data.Map as M ( Map, lookup, insert, alter, fromListWith, toList )
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Foundation.Data
    ( AppMessage
      ( MsgPhoto, MsgMessage, MsgViewContact, MsgActions, MsgNewMessage
      , MsgPushNotificationExcception, MsgVideoCall, MsgAudioCall
      , MsgOutgoingCall, MsgCallDeclined, MsgCalleeDeclinedTheCall
      , MsgCancel, MsgClose, MsgAppName, MsgCallCanceledByCaller
      , MsgIncomingAudioCallFrom, MsgIncomingVideoCallFrom 
      )
    )

import Network.HTTP.Client.Conduit (Manager)

import Model
    ( UserId, User (User, userName, userEmail), statusError
    , Chat (Chat, chatMessage, chatCreated, chatUser, chatInterlocutor)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , PushSubscription (PushSubscription), secretVolumeVapid, apiInfoVapid
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , ContactId, Token, Store, Call
    , PushMsgType
      ( PushMsgTypeVideoCall, PushMsgTypeAudioCall, PushMsgTypeMessage
      , PushMsgTypeCancel, PushMsgTypeDecline, PushMsgTypeAccept
      , PushMsgTypeRefresh
      )
    , CallType (CallTypeAudio, CallTypeVideo)
    , EntityField
      ( UserId, ChatStatus, ChatInterlocutor, ChatUser, ChatCreated, TokenApi
      , PushSubscriptionSubscriber, TokenId, TokenStore, StoreToken, StoreVal
      , ChatReceived, ChatId, ChatNotified, PushSubscriptionPublisher
      , CallCaller, CallCallee, CallType, CallStart, ChatMessage
      )
    )

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings
    ( widgetFile, Superuser (Superuser, superuserUsername)
    , AppSettings (appSuperuser)
    )
import Settings.StaticFiles
    ( img_chat_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    )

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)

import Web.WebPush
    ( mkPushNotification, VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    , readVAPIDKeys, pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushTopic, PushTopic (PushTopic), pushUrgency
    , PushUrgency (PushUrgencyLow)
    )

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor, getSubYesod, newIdent
    )
import Yesod.Core.Handler
    ( getUrlRender, getRouteToParent, addMessageI
    , getMessageRender
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Fields (FormMessage, intField)
import Yesod.Form.Input (runInputPost, ireq)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.Static (StaticRoute)
import Yesod.WebSockets (WebSocketsT, sourceWS, sendTextData, race_, webSockets)


class ( Yesod m, RenderMessage m FormMessage, RenderMessage m AppMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlBackend
      ) => YesodChat m where

    getAppHttpManager :: HandlerFor m Manager
    getBacklink :: UserId -> UserId -> HandlerFor m (Route m)
    getAccountPhotoRoute :: UserId -> HandlerFor m (Route m)
    getContactRoute :: UserId -> UserId -> ContactId -> HandlerFor m (Route m)
    getStaticRoute :: StaticRoute -> HandlerFor m (Route m)
    getVideoPushRoute :: HandlerFor m (Route m)
    getVideoOutgoingRoute :: UserId -> UserId -> HandlerFor m (Route m)
    getAppSettings :: HandlerFor m AppSettings


type ChatHandler a = forall m. YesodChat m => SubHandlerFor ChatRoom m a


postAcknowledgeR :: ChatHandler ()
postAcknowledgeR = do

    cid <- liftHandler $ toSqlKey <$> runInputPost (ireq intField "cid")
    now <- liftIO getCurrentTime

    liftHandler $ runDB $ update $ \x -> do
        set x [ ChatReceived =. just (val now)
              , ChatNotified =. val True
              , ChatStatus =. val ChatMessageStatusRead
              ]
        where_ $ x ^. ChatId ==. val cid


getChatRoomR :: UserId -> UserId -> ContactId -> ChatHandler Html
getChatRoomR sid rid cid = do

    backlink <- liftHandler $ getBacklink sid rid
    photos <- liftHandler $ getAccountPhotoRoute sid
    photor <- liftHandler $ getAccountPhotoRoute rid
    contact  <- liftHandler $ getContactRoute sid rid cid
    icon <- liftHandler $ getStaticRoute img_call_FILL0_wght400_GRAD0_opsz24_svg
    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    video <- liftHandler getVideoPushRoute
    outgoing <- liftHandler $ getVideoOutgoingRoute sid rid

    interlocutor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x

    subscribed <- liftHandler $ isJust <$> runDB ( selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        return x )

    accessible <- liftHandler $ isJust <$> runDB ( selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        return x )


    liftHandler $ runDB $ update $ \x -> do
        set x [ChatStatus =. val ChatMessageStatusRead]
        where_ $ x ^. ChatInterlocutor ==. val sid
        where_ $ x ^. ChatUser ==. val rid
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

    webSockets (chatApp sid rid cid)

    chats <- liftHandler $ groupByDay <$> runDB ( select $ do
        (uid,iid,time,msg,ctype,media) <- from $
            ( do
                  x <- from $ ( do
                                    x <- from $ table @Chat
                                    where_ $ x ^. ChatUser ==. val sid
                                    where_ $ x ^. ChatInterlocutor ==. val rid
                                    return x
                              )
                       `unionAll_`
                       ( do
                             x <- from $ table @Chat
                             where_ $ x ^. ChatInterlocutor ==. val sid
                             where_ $ x ^. ChatUser ==. val rid
                             return x
                       )
                  return ( x ^. ChatUser
                         , x ^. ChatInterlocutor
                         , x ^. ChatCreated
                         , x ^. ChatMessage
                         , val CallTypeAudio
                         , val False
                         )
                    )
            `unionAll_`
            ( do
                  x <- from $ ( do
                                    x <- from $ table @Call
                                    where_ $ x ^. CallCaller ==. val sid
                                    where_ $ x ^. CallCallee ==. val rid
                                    return x
                              )
                      `unionAll_`
                      ( do
                            x <- from $ table @Call
                            where_ $ x ^. CallCallee ==. val sid
                            where_ $ x ^. CallCaller ==. val rid
                            return x
                      )
                  return ( x ^. CallCaller
                         , x ^. CallCallee
                         , x ^. CallStart
                         , val ("Media call" :: Text)
                         , x ^. CallType
                         , val True
                         )
            )
        orderBy [desc time]
        return (uid,iid,time,msg,ctype,media) )

    toParent <- getRouteToParent

    let channel = fromSqlKey cid

    callerName <- liftHandler $ resolveName <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x )

    msgr <- getMessageRender
    liftHandler $ defaultLayout $ do

        idButtonVideoCall <- newIdent
        idButtonAudioCall <- newIdent
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        idButtonSend <- newIdent
        idDialogOutgoingCall <- newIdent
        idButtonOutgoingCallCancel <- newIdent
        idDialogCallDeclined <- newIdent

        $(widgetFile "chat/room")

    where
      resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)

      groupByDay = M.toList . groupByKey (\(_, _, Value t, _, _, _) -> utctDay t)


chatApp :: YesodChat m
        => UserId -- ^ user
        -> UserId -- ^ interlocutor
        -> ContactId
        -> WebSocketsT (SubHandlerFor ChatRoom m) ()
chatApp userId interlocutorId contactId = do

    let channelId = S.fromList [userId, interlocutorId]

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    writeChan <- atomically $ case maybeChan of
      Nothing -> do
          chan <- newBroadcastTChan
          writeTVar channelMapTVar $ M.insert channelId (chan,1) channelMap
          return chan
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          return writeChan

    readChan <- atomically $ dupTChan writeChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (
             \msg -> do
                 now <- liftIO getCurrentTime
                 let chat = Chat userId interlocutorId now msg ChatMessageStatusUnread Nothing False
                 cid <- liftHandler (runDB $ insert chat)
                 atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText $ object
                     [ "cid" .= cid
                     , "user" .= chatUser chat
                     , "interlocutor" .= chatInterlocutor chat
                     , "created" .= chatCreated chat
                     , "message" .= chatMessage chat
                     ]

                 _ <- forkIO $ do
                     threadDelay (5 * 1000000)

                     chat' <- liftHandler $ runDB $ selectOne $ do
                         x <- from $ table @Chat
                         where_ $ x ^. ChatId ==. val cid
                         where_ $ not_ $ x ^. ChatNotified
                         where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread
                         return x

                     case chat' of
                       Just (Entity _ (Chat uid iid _ message _ _ _)) -> do

                           storeType <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
                               x <- from $ table @Token
                               where_ $ x ^. TokenApi ==. val apiInfoVapid
                               return (x ^. TokenId, x ^. TokenStore) )

                           let readTriple (s,x,y) = VAPIDKeysMinDetails s x y

                           details <- case storeType of
                             Just (_, StoreTypeGoogleSecretManager) -> do
                                 liftIO $ (readTriple <$>) . readMaybe <$> readFile' secretVolumeVapid

                             Just (tid, StoreTypeDatabase) -> do
                                 liftHandler $ ((readTriple <$>) . readMaybe . unpack . unValue =<<) <$> runDB
                                     (
                                         selectOne $ do
                                           x <-from $ table @Store
                                           where_ $ x ^. StoreToken ==. val tid
                                           return $ x ^. StoreVal
                                     )

                             Just (_,StoreTypeSession) -> return Nothing
                             Nothing -> return Nothing

                           case details of
                             Just vapidKeysMinDetails -> do

                                 subscriptions <- liftHandler $ runDB $ select $ do
                                     x <- from $ table @PushSubscription
                                     where_ $ x ^. PushSubscriptionSubscriber ==. val iid
                                     where_ $ x ^. PushSubscriptionPublisher ==. val uid
                                     return x

                                 sender <- liftHandler $ runDB $ selectOne $ do
                                     x <- from $ table @User
                                     where_ $ x ^. UserId ==. val uid
                                     return x

                                 let vapidKeys = readVAPIDKeys vapidKeysMinDetails
                                 iconr <- liftHandler $ getStaticRoute img_chat_FILL0_wght400_GRAD0_opsz24_svg
                                 urlr <- getUrlRender
                                 msgr <- getMessageRender
                                 tpr <- getRouteToParent
                                 Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

                                 forM_ subscriptions $ \(Entity _ (PushSubscription sid pid endpoint p256dh auth)) -> do
                                     photor <- liftHandler $ getAccountPhotoRoute pid
                                     let notification = mkPushNotification endpoint p256dh auth
                                             & pushMessage .~ object
                                                 [ "messageType" .= PushMsgTypeMessage
                                                 , "title" .= (msgr MsgAppName <> ": " <> msgr MsgNewMessage)
                                                 , "icon" .= urlr iconr
                                                 , "image" .= urlr photor
                                                 , "body" .= message
                                                 , "reply" .= urlr (tpr $ ChatRoomR sid pid contactId)
                                                 , "senderId" .= pid
                                                 , "senderName" .= (
                                                       (\u -> fromMaybe (userEmail u) (userName u)) . entityVal <$> sender
                                                                   )
                                                 , "recipientId" .= sid
                                                 ]
                                             & pushSenderEmail .~ superuserUsername
                                             & pushExpireInSeconds .~ 30 * 60
                                             & pushTopic ?~ (PushTopic . pack . show $ PushMsgTypeMessage)
                                             & pushUrgency ?~ PushUrgencyLow

                                     manager <- liftHandler getAppHttpManager

                                     result <- sendPushNotification vapidKeys manager notification
                                     
                                     case result of
                                       Left ex -> do
                                           liftIO $ print ex
                                           liftHandler $ addMessageI statusError MsgPushNotificationExcception
                                       Right () -> liftHandler $ runDB $ update $ \x -> do
                                           set x [ChatNotified =. val True]
                                           where_ $ x ^. ChatId ==. val cid
                             Nothing -> do
                                 liftIO $ print @Text "No VAPID details"
                                 liftHandler $ addMessageI statusError MsgPushNotificationExcception

                       Nothing -> return ()

                 return ()
             ) ))
    case e of
      Left _ -> do
          m <- readTVarIO channelMapTVar
          let newChannelMap = M.alter userLeftChannel channelId m
          atomically $ writeTVar channelMapTVar newChannelMap
      Right () -> return ()


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


groupByKey :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupByKey key = M.fromListWith (<>) . fmap (\x -> (key x,[x]))


instance YesodChat m => YesodSubDispatch ChatRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
