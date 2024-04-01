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
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity), PersistStoreWrite (insert_))
import Database.Persist.Sql (SqlBackend)

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as M ( Map, lookup, insert, alter, fromListWith, toList )
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime)

import Foundation.Data
    ( AppMessage (MsgPhoto)
    )
    
import Model
    ( UserId, User (User), Chat (Chat)
    , ChatMessageStatus (ChatMessageStatusRead, ChatMessageStatusUnread)
    , EntityField (UserId)
    )

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor, getSubYesod
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Fields (FormMessage)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.WebSockets (WebSocketsT, sourceWS, sendTextData, race_)


class ( Yesod m, RenderMessage m FormMessage, RenderMessage m AppMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlBackend
      ) => YesodChat m where
    getBacklink :: UserId -> UserId -> HandlerFor m (Route m)
    getAccountPhotoRoute :: UserId -> HandlerFor m (Route m)
    


type ChatHandler a = forall m. YesodChat m => SubHandlerFor ChatRoom m a


getChatRoomR :: UserId -> UserId -> ChatHandler Html
getChatRoomR sid rid = do
    backlink <- liftHandler $ getBacklink sid rid
    photo <- liftHandler $ getAccountPhotoRoute rid

    user <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x

    liftHandler $ defaultLayout $ do
        $(widgetFile "chat/room")




chatApp :: YesodChat m
        => Text -- ^ Channel Id
        -> Entity User -- ^ user
        -> Entity User -- ^ interlocutor
        -> WebSocketsT (SubHandlerFor ChatRoom m) ()
chatApp pid (Entity uid _) (Entity iid _) = do

    let channelId = pid -- pack $ show $ fromSqlKey pid

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


instance YesodChat m => YesodSubDispatch ChatRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
