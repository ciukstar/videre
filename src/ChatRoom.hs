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

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (ChatRoomR)
    )

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Monad (forever)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set, select, orderBy, desc
    , (^.), (==.), (=.), (&&.), (||.)
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert_))
import Database.Persist.Sql (SqlBackend, fromSqlKey)

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as M ( Map, lookup, insert, alter, fromListWith, toList )
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Foundation.Data
    ( AppMessage (MsgPhoto, MsgMessage, MsgViewContact, MsgActions)
    )

import Model
    ( UserId, User (User), Chat (Chat), ContactId
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , EntityField (UserId, ChatStatus, ChatInterlocutor, ChatUser, ChatTimemark)
    )

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor, getSubYesod, newIdent
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Fields (FormMessage)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.WebSockets (WebSocketsT, sourceWS, sendTextData, race_, webSockets)
import qualified Data.Set as S


class ( Yesod m, RenderMessage m FormMessage, RenderMessage m AppMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlBackend
      ) => YesodChat m where
    getBacklink :: UserId -> UserId -> HandlerFor m (Route m)
    getAccountPhotoRoute :: UserId -> HandlerFor m (Route m)
    getContactRoute :: UserId -> UserId -> ContactId -> HandlerFor m (Route m)


type ChatHandler a = forall m. YesodChat m => SubHandlerFor ChatRoom m a


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

    webSockets (chatApp sid rid)


    chats <- liftHandler $ M.toList . groupByKey (\(Entity _ (Chat _ _ t _ _)) -> utctDay t) <$> runDB ( select $ do
        x <- from $ table @Chat
        where_ $ ( (x ^. ChatUser ==. val sid)
                   &&. (x ^. ChatInterlocutor ==. val rid)
                 ) ||. ( (x ^. ChatUser ==. val rid)
                         &&. (x ^. ChatInterlocutor ==. val sid)
                       )
        orderBy [desc (x ^. ChatTimemark)]
        return x )

    
    liftHandler $ defaultLayout $ do
        idChatOutput <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        $(widgetFile "chat/room")


chatApp :: YesodChat m
        => UserId -- ^ user
        -> UserId -- ^ interlocutor
        -> WebSocketsT (SubHandlerFor ChatRoom m) ()
chatApp uid iid = do

    let channelId = S.fromList [uid,iid]

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

                              m <- readTVarIO channelMapTVar

                              let n = maybe 0 snd (M.lookup channelId m)
                              let status = if n > 1 then ChatMessageStatusRead else ChatMessageStatusUnread

                              now <- liftIO getCurrentTime
                              let chat = Chat uid iid now msg status
                              atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText chat
                              liftHandler (runDB $ insert_ chat)
                          )
                    ))
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
