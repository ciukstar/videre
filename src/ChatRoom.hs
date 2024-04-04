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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (ChatRoomR, AcknowledgeR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~))
import Control.Monad (forever, forM_)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set, select, orderBy, desc
    , (^.), (==.), (=.), (&&.), (||.)
    , just, Value (unValue), Entity (entityVal), not_
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert))
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Data.Aeson (object, (.=), ToJSON, toJSON)
import qualified Data.Aeson as A (Value (String))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Function ((&))
import qualified Data.Map as M ( Map, lookup, insert, alter, fromListWith, toList )
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Foundation.Data
    ( AppMessage
      ( MsgPhoto, MsgMessage, MsgViewContact, MsgActions, MsgNewMessage
      , MsgNoMessagesExchangedYet, MsgPushNotificationExcception
      )
    )

import Network.HTTP.Client.Conduit (Manager)

import Model
    ( UserId, User (User, userName, userEmail)
    , Chat (Chat, chatMessage, chatCreated, chatUser, chatInterlocutor)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , PushSubscription (PushSubscription), secretVolumeVapid, apiInfoVapid
    , StoreType (StoreTypeGoogleSecretManager, StoreTypeDatabase, StoreTypeSession)
    , ContactId, Token, Store
    , EntityField
      ( UserId, ChatStatus, ChatInterlocutor, ChatUser, ChatCreated, TokenApi
      , PushSubscriptionSubscriber, TokenId, TokenStore, StoreToken, StoreVal
      , ChatReceived, ChatId, ChatNotified
      ), statusError
    )

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import System.IO (readFile')

import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Web.WebPush
    ( mkPushNotification, VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    , readVAPIDKeys, pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification
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
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.WebSockets (WebSocketsT, sourceWS, sendTextData, race_, webSockets)
import Yesod.Form.Input (runInputPost, ireq)
import Text.Julius (RawJS(rawJS))
import Settings.StaticFiles (img_chat_FILL0_wght400_GRAD0_opsz24_svg)
import Yesod.Static (StaticRoute)


class ( Yesod m, RenderMessage m FormMessage, RenderMessage m AppMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlBackend
      ) => YesodChat m where
    getAppHttpManager :: HandlerFor m Manager
    getBacklink :: UserId -> UserId -> HandlerFor m (Route m)
    getAccountPhotoRoute :: UserId -> HandlerFor m (Route m)
    getContactRoute :: UserId -> UserId -> ContactId -> HandlerFor m (Route m)
    getStaticRoute :: StaticRoute -> HandlerFor m (Route m) 


type ChatHandler a = forall m. YesodChat m => SubHandlerFor ChatRoom m a


data PushMsgType = PushMsgTypeMessage
    deriving (Eq, Show, Read)

instance ToJSON PushMsgType where
    toJSON :: PushMsgType -> A.Value
    toJSON = A.String . pack . show


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
    photo <- liftHandler $ getAccountPhotoRoute rid
    contact  <- liftHandler $ getContactRoute sid rid cid

    interlocutor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x

    liftHandler $ runDB $ update $ \x -> do
        set x [ChatStatus =. val ChatMessageStatusRead]
        where_ $ x ^. ChatInterlocutor ==. val sid
        where_ $ x ^. ChatUser ==. val rid
        where_ $ x ^. ChatStatus ==. val ChatMessageStatusUnread

    webSockets (chatApp sid rid cid)

    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _ _ _)) -> utctDay t) <$> runDB ( select $ do
        x <- from $ table @Chat
        where_ $ ( (x ^. ChatUser ==. val sid)
                   &&. (x ^. ChatInterlocutor ==. val rid)
                 ) ||. ( (x ^. ChatUser ==. val rid)
                         &&. (x ^. ChatInterlocutor ==. val sid)
                       )
        orderBy [desc (x ^. ChatCreated)]
        return x )

    toParent <- getRouteToParent

    liftHandler $ defaultLayout $ do
        idMenuItemViewContact <- newIdent
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        $(widgetFile "chat/room")


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

                                 forM_ subscriptions $ \(Entity _ (PushSubscription sid pid endpoint p256dh auth)) -> do
                                     photor <- liftHandler $ getAccountPhotoRoute pid
                                     let notification = mkPushNotification endpoint p256dh auth
                                             & pushMessage .~ object
                                                 [ "messageType" .= PushMsgTypeMessage
                                                 , "topic" .= PushMsgTypeMessage
                                                 , "title" .= msgr MsgNewMessage
                                                 , "icon" .= urlr iconr
                                                 , "reply" .= urlr (tpr $ ChatRoomR sid pid contactId)
                                                 , "senderId" .= pid
                                                 , "senderName" .= ( (userName . entityVal <$> sender)
                                                                     <|> (Just . userEmail . entityVal <$> sender)
                                                                   )
                                                 , "senderPhoto" .= urlr photor
                                                 , "recipientId" .= sid
                                                 , "body" .= message
                                                 ]
                                             & pushSenderEmail .~ ("ciukstar@gmail.com" :: Text)
                                             & pushExpireInSeconds .~ 60 * 60

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
