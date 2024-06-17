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
    , Route (ChatRoomR, AcknowledgeR, ChatChannelR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~),(?~))
import Control.Monad (forever, forM_)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set, select, orderBy, desc
    , (^.), (==.), (!=.), (=.), (:&)((:&))
    , just, Value (Value), Entity (entityVal), not_, unionAll_
    , innerJoin, on
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert))
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Data.Aeson (object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import qualified Data.Map as M
    ( Map, lookup, insert, alter, fromListWith, toList )
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Foundation
    ( AppMessage
      ( MsgPhoto, MsgMessage, MsgViewContact, MsgActions, MsgNewMessage
      , MsgPushNotificationExcception, MsgVideoCall, MsgAudioCall
      , MsgOutgoingCall, MsgCallDeclined, MsgCalleeDeclinedTheCall
      , MsgCancel, MsgClose, MsgCallCanceledByCaller, MsgIncomingAudioCallFrom
      , MsgIncomingVideoCallFrom, MsgCallerCalleeSubscriptionLoopWarning
      , MsgUserYouSeemsUnsubscribed, MsgUserAppearsToBeUnavailable, MsgAppName
      , MsgSubscribe
      )
    )

import Network.HTTP.Client.Conduit (Manager)
import Network.HTTP.Types (extractPath)

import Model
    ( statusError, paramEndpoint, paramBacklink
    , UserId, User (User, userName, userEmail)
    , Chat (Chat, chatMessage, chatCreated, chatUser, chatInterlocutor)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , PushSubscription (PushSubscription)
    , ContactId, Call, Ringtone (Ringtone)
    , PushMsgType
      ( PushMsgTypeVideoCall, PushMsgTypeAudioCall, PushMsgTypeChat
      , PushMsgTypeCancel, PushMsgTypeDecline, PushMsgTypeAccept
      , PushMsgTypeRefresh
      )
    , CallType (CallTypeAudio, CallTypeVideo)
    , RingtoneId, UserRingtone, DefaultRingtone
    , RingtoneType
      ( RingtoneTypeCallOutgoing, RingtoneTypeChatOutgoing, RingtoneTypeChatIncoming)
    , EntityField
      ( UserId, ChatStatus, ChatInterlocutor, ChatUser, ChatCreated
      , PushSubscriptionSubscriber
      , ChatReceived, ChatId, ChatNotified, PushSubscriptionPublisher, CallType
      , CallCaller, CallCallee, CallStart, ChatMessage, PushSubscriptionEndpoint
      , RingtoneId, UserRingtoneRingtone, UserRingtoneUser, UserRingtoneType
      , DefaultRingtoneType, DefaultRingtoneRingtone
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
    , ringtones_outgoing_call_galaxy_ringtones_1_mp3
    , ringtones_outgoing_message_ringtone_1_mp3
    , ringtones_incoming_message_ringtone_1_mp3
    )

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))

import Web.WebPush
    ( mkPushNotification
    , pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushTopic, PushTopic (PushTopic), pushUrgency
    , PushUrgency (PushUrgencyHigh), VAPIDKeys
    )

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor, getSubYesod, newIdent
    , lookupGetParam, getSubCurrentRoute
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
    getVideoPushRoute :: UserId -> ContactId -> UserId -> HandlerFor m (Route m)
    getVideoOutgoingRoute :: UserId -> ContactId -> UserId -> Bool -> HandlerFor m (Route m)
    getUserRingtoneAudioRoute :: UserId -> RingtoneId -> HandlerFor m (Route m)
    getDefaultRingtoneAudioRoute :: RingtoneId -> HandlerFor m (Route m)
    getAppSettings :: HandlerFor m AppSettings
    getVapidKeys :: HandlerFor m (Maybe VAPIDKeys)


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


getChatChannelR :: UserId -> ContactId -> UserId -> ChatHandler ()
getChatChannelR sid cid rid = webSockets (chatApp sid rid cid)


getChatRoomR :: UserId -> ContactId -> UserId -> ChatHandler Html
getChatRoomR sid cid rid = do

    backlink <- liftHandler $ getBacklink sid rid
    photos <- liftHandler $ getAccountPhotoRoute sid
    photor <- liftHandler $ getAccountPhotoRoute rid
    contact  <- liftHandler $ getContactRoute sid rid cid
    icon <- liftHandler $ getStaticRoute img_call_FILL0_wght400_GRAD0_opsz24_svg
    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    video <- liftHandler $ getVideoPushRoute sid cid rid
    outgoing <- liftHandler $ getVideoOutgoingRoute sid cid rid False

    user <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x

    endpoint <- liftHandler $ lookupGetParam paramEndpoint

    subscribed <- liftHandler $ isJust <$> runDB ( selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val sid
        where_ $ x ^. PushSubscriptionPublisher ==. val rid
        where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
        return x )

    interlocutor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x

    accessible <- liftHandler $ isJust <$> runDB ( selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        where_ $ just (x ^. PushSubscriptionEndpoint) !=. val endpoint
        return x )
    
    loop <- liftHandler $ isJust <$> runDB ( selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
        return x )

    liftHandler $ runDB $ update $ \x -> do
        set x [ChatStatus =. val ChatMessageStatusRead]
        where_ $ x ^. ChatInterlocutor ==. val sid
        where_ $ x ^. ChatUser ==. val rid
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

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
    curr <- (toParent <$>) <$> getSubCurrentRoute
    rndr <- getUrlRender

    callerName <- liftHandler $ resolveName <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x )

    outgoingCallRingtoneR <- liftHandler $ do
        userRingtone <- liftHandler $ runDB $ selectOne $ do
            x :& t <- from $ table @Ringtone `innerJoin` table @UserRingtone
                `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. UserRingtoneRingtone)
            where_ $ t ^. UserRingtoneUser ==. val sid
            where_ $ t ^. UserRingtoneType ==. val RingtoneTypeCallOutgoing
            return x
        case userRingtone of
          Just (Entity tid (Ringtone _ mime _)) -> getUserRingtoneAudioRoute sid tid >>= \r -> return (r, mime)
          Nothing -> do
              defaultRingtone <- liftHandler $ runDB $ selectOne $ do
                    x :& t <- from $ table @Ringtone `innerJoin` table @DefaultRingtone
                        `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. DefaultRingtoneRingtone)
                    where_ $ t ^. DefaultRingtoneType ==. val RingtoneTypeCallOutgoing
                    return x
              case defaultRingtone of
                Just (Entity did (Ringtone _ mime _)) -> getDefaultRingtoneAudioRoute did >>= \r -> return (r, mime)
                Nothing -> getStaticRoute ringtones_outgoing_call_galaxy_ringtones_1_mp3 >>= \r -> return (r, "audio/mpeg")

    incomingChatRingtoneR <- liftHandler $ do
        userRingtone <- liftHandler $ runDB $ selectOne $ do
            x :& t <- from $ table @Ringtone `innerJoin` table @UserRingtone
                `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. UserRingtoneRingtone)
            where_ $ t ^. UserRingtoneUser ==. val sid
            where_ $ t ^. UserRingtoneType ==. val RingtoneTypeChatIncoming
            return x
        case userRingtone of
          Just (Entity tid (Ringtone _ mime _)) -> getUserRingtoneAudioRoute sid tid >>= \r -> return (r, mime)
          Nothing -> do
              defaultRingtone <- liftHandler $ runDB $ selectOne $ do
                    x :& t <- from $ table @Ringtone `innerJoin` table @DefaultRingtone
                        `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. DefaultRingtoneRingtone)
                    where_ $ t ^. DefaultRingtoneType ==. val RingtoneTypeChatIncoming
                    return x
              case defaultRingtone of
                Just (Entity did (Ringtone _ mime _)) -> getDefaultRingtoneAudioRoute did >>= \r -> return (r, mime)
                Nothing -> getStaticRoute ringtones_incoming_message_ringtone_1_mp3 >>= \r -> return (r, "audio/mpeg")

    outgoingChatRingtoneR <- liftHandler $ do
        userRingtone <- liftHandler $ runDB $ selectOne $ do
            x :& t <- from $ table @Ringtone `innerJoin` table @UserRingtone
                `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. UserRingtoneRingtone)
            where_ $ t ^. UserRingtoneUser ==. val sid
            where_ $ t ^. UserRingtoneType ==. val RingtoneTypeChatOutgoing
            return x
        case userRingtone of
          Just (Entity tid (Ringtone _ mime _)) -> getUserRingtoneAudioRoute sid tid >>= \r -> return (r, mime)
          Nothing -> do
              defaultRingtone <- liftHandler $ runDB $ selectOne $ do
                    x :& t <- from $ table @Ringtone `innerJoin` table @DefaultRingtone
                        `on` (\(x :& t) -> x ^. RingtoneId ==. t ^. DefaultRingtoneRingtone)
                    where_ $ t ^. DefaultRingtoneType ==. val RingtoneTypeChatOutgoing
                    return x
              case defaultRingtone of
                Just (Entity did (Ringtone _ mime _)) -> getDefaultRingtoneAudioRoute did >>= \r -> return (r, mime)
                Nothing -> getStaticRoute ringtones_outgoing_message_ringtone_1_mp3 >>= \r -> return (r, "audio/mpeg")

    msgr <- getMessageRender
    liftHandler $ defaultLayout $ do

        idButtonVideoCall <- newIdent
        idButtonAudioCall <- newIdent
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        idButtonSend <- newIdent
        idAudioOutgoingChat <- newIdent
        idAudioIncomingChat <- newIdent
        idDialogOutgoingCall <- newIdent
        idAudioOutgoingCallRingtone <- newIdent
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

                           mVapidKeys <- liftHandler getVapidKeys
                           case mVapidKeys of
                             Just vapidKeys -> do

                                 subscriptions <- liftHandler $ runDB $ select $ do
                                     x <- from $ table @PushSubscription
                                     where_ $ x ^. PushSubscriptionPublisher ==. val uid
                                     where_ $ x ^. PushSubscriptionSubscriber ==. val iid
                                     return x

                                 sender <- liftHandler $ runDB $ selectOne $ do
                                     x <- from $ table @User
                                     where_ $ x ^. UserId ==. val uid
                                     return x

                                 
                                 iconr <- liftHandler $ getStaticRoute img_chat_FILL0_wght400_GRAD0_opsz24_svg
                                 urlr <- getUrlRender
                                 msgr <- getMessageRender
                                 tpr <- getRouteToParent
                                 Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

                                 let expath = decodeUtf8 . extractPath . encodeUtf8 . urlr

                                 forM_ subscriptions $ \(Entity _ (PushSubscription sid pid endpoint p256dh auth _)) -> do
                                     photor <- liftHandler $ getAccountPhotoRoute pid
                                     let notification = mkPushNotification endpoint p256dh auth
                                             & pushMessage .~ object
                                                 [ "title" .= (msgr MsgAppName <> ": " <> msgr MsgNewMessage)
                                                 , "icon" .= expath iconr
                                                 , "image" .= expath photor
                                                 , "body" .= message
                                                 , "messageType" .= PushMsgTypeChat
                                                 , "targetRoom" .= (expath . tpr $ ChatRoomR sid contactId pid)
                                                 , "senderId" .= pid
                                                 , "senderName" .= (
                                                       (\u -> fromMaybe (userEmail u) (userName u)) . entityVal <$> sender
                                                                   )
                                                 , "recipientId" .= sid
                                                 ]
                                             & pushSenderEmail .~ superuserUsername
                                             & pushExpireInSeconds .~ 30 * 60
                                             & pushTopic ?~ (PushTopic . pack . show $ PushMsgTypeChat)
                                             & pushUrgency ?~ PushUrgencyHigh

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
