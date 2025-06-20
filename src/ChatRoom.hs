{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), Line (Line), resourcesChatRoom
    , Route
      ( ChatRoomR, ChatChannelR, ChatMsgRemoveR, ChatMsgRemoveUndoR
      , ChatMsgDeleteR, ChatMsgReadR, ChatMsgDeliveredR, ChatDeleteR
      )
    )
    
import CMark (commonmarkToHtml)

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~),(?~))
import Control.Monad (forever, forM_)
import Control.Concurrent.STM.TChan
    ( writeTChan, dupTChan, readTChan, newBroadcastTChan
    )

import Data.Aeson
    ( decode, object, Value, ToJSON (toJSON), FromJSON (parseJSON)
    , (.=), (.:), (.:?)
    , withObject
    )
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser)
import Data.Function ((&))
import qualified Data.Map as M
    ( Map, lookup, insert, alter, fromListWith, toList, filterWithKey
    )
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set, select
    , (^.), (?.), (==.), (!=.), (=.), (:&)((:&))
    , just, Entity (entityVal), not_, unionAll_
    , innerJoin, leftJoin, on, delete, isNothing_
    )
import Database.Persist (Entity (Entity), insert)
import Database.Persist.Sql (SqlBackend, fromSqlKey)

import Foundation
    ( AppMessage
      ( MsgPhoto, MsgMessage, MsgViewContact, MsgActions, MsgNewMessage
      , MsgPushNotificationExcception, MsgVideoCall, MsgAudioCall, MsgBack
      , MsgOutgoingCall, MsgCallDeclined, MsgCalleeDeclinedTheCall, MsgChats
      , MsgCancel, MsgClose, MsgCallCanceledByCaller, MsgIncomingAudioCallFrom
      , MsgIncomingVideoCallFrom, MsgCallerCalleeSubscriptionLoopWarning
      , MsgUserYouSeemsUnsubscribed, MsgUserAppearsToBeUnavailable, MsgAppName
      , MsgSubscribe, MsgDele, MsgCopy, MsgRemovedByRecipient, MsgContentCopied
      , MsgRemoved, MsgAnotherAccountAccessProhibited, MsgMessageDeleted
      , MsgMessageRemoved, MsgAuthenticationRequired, MsgUndo, MsgDele, MsgReply
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgChatDeletedSuccessfully
      , MsgRemove, MsgInvalidFormData, MsgDeleteChat, MsgOnline
      )
    )

import Network.HTTP.Client.Conduit (Manager)
import Network.HTTP.Types (extractPath)

import Model
    ( msgError, msgSuccess, statusError, paramEndpoint, paramBacklink
    , UserId, User (User, userName, userEmail)
    , ChatId, Chat (Chat, chatMessage, chatCreated, chatAuthor, chatRecipient)
    , PushSubscription (PushSubscription)
    , ContactId
    , Ringtone (Ringtone)
    , PushMsgType
      ( PushMsgTypeVideoCall, PushMsgTypeAudioCall, PushMsgTypeChat
      , PushMsgTypeCancel, PushMsgTypeDecline, PushMsgTypeAccept
      , PushMsgTypeRefresh
      )
    , RingtoneId, UserRingtone, DefaultRingtone
    , RingtoneType
      ( RingtoneTypeCallOutgoing, RingtoneTypeChatOutgoing, RingtoneTypeChatIncoming
      )
    , WsMessageType
      ( WsMessageTypeChat, WsMessageTypeDelete, WsMessageTypeRemove
      , WsMessageTypeDelivered, WsMessageTypeRead, WsMessageTypeUndo
      , WsMessageTypeTyping, WsMessageTypeOnline, WsMessageTypeOffline
      )
    , ChatType (ChatTypeMessage, ChatTypeVideoCall, ChatTypeAudioCall)
    , EntityField
      ( UserId, ChatAuthor, ChatRecipient
      , PushSubscriptionSubscriber, ChatTimeDelivered
      , ChatId, PushSubscriptionPublisher
      , PushSubscriptionEndpoint
      , RingtoneId, UserRingtoneRingtone, UserRingtoneUser, UserRingtoneType
      , DefaultRingtoneType, DefaultRingtoneRingtone, ChatRemovedAuthor
      , ChatRemovedRecipient, ChatTimeRead, ChatRead, ChatDelivered, ChatReply
      )
    )

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM (atomically, readTVarIO, writeTVar)

import Settings
    ( AppSettings (appSuperuser), Superuser (Superuser, superuserUsername)
    , widgetFile
    )
import Settings.StaticFiles
    ( img_chat_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_FILL0_wght400_GRAD0_opsz24_svg
    , img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    , ringtones_outgoing_call_galaxy_ringtones_1_mp3
    , ringtones_outgoing_message_ringtone_1_mp3
    , ringtones_incoming_message_ringtone_1_mp3
    , img_wallpaper_pattern_svg
    )

import Text.Blaze.Html (preEscapedText)
import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))

import Web.WebPush
    ( mkPushNotification, pushUrgency
    , pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushTopic, PushTopic (PushTopic)
    , PushUrgency (PushUrgencyHigh), VAPIDKeys
    )

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor, getSubYesod, newIdent
    , lookupGetParam, getSubCurrentRoute, getMessages, permissionDeniedI
    , redirect
    )
import Yesod.Core.Handler
    ( getUrlRender, getRouteToParent, addMessageI
    , getMessageRender
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Core.Widget (setTitleI, whamlet, WidgetFor)
import Yesod.Form.Fields (FormMessage)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))
import Yesod.Static (StaticRoute)
import Yesod.WebSockets (WebSocketsT, sourceWS, sendTextData, race_, webSockets)
import qualified Data.Text.Lazy.Encoding as TL
import Yesod.Form.Functions (runFormPost, generateFormPost)
import Yesod.Form.Types (MForm, FormResult (FormSuccess))


class ( Yesod m, RenderMessage m FormMessage, RenderMessage m AppMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlBackend
      ) => YesodChat m where

    getAppHttpManager :: HandlerFor m Manager
    getHomeRoute :: HandlerFor m (Route m)
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
    getMaybeAuthId :: HandlerFor m (Maybe UserId)


type ChatHandler a = forall m. YesodChat m => SubHandlerFor ChatRoom m a


postChatDeleteR :: UserId -> ContactId -> UserId -> ChatHandler ()
postChatDeleteR sid cid rid = do

    checkAuthorized sid

    ((fr,_),_) <- runFormPost formChatDelete

    rtp <- getRouteToParent
    
    case fr of
      FormSuccess () -> do

          liftHandler $ runDB $ delete $ do
              x <- from $ table @Chat
              where_ $ x ^. ChatAuthor ==. val sid
              where_ $ x ^. ChatRecipient ==. val rid

          liftHandler $ runDB $ update $ \x -> do
              set x [ ChatRemovedRecipient =. val True ]
              where_ $ x ^. ChatAuthor ==. val rid
              where_ $ x ^. ChatRecipient ==. val sid
          
          addMessageI msgSuccess MsgChatDeletedSuccessfully
          redirect $ rtp $ ChatRoomR sid cid rid
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ rtp $ ChatRoomR sid cid rid


formChatDelete :: forall m. Html -> MForm (SubHandlerFor ChatRoom m) (FormResult (), WidgetFor m ())
formChatDelete extra = return (pure (), [whamlet|^{extra}|])


postChatMsgRemoveUndoR :: UserId -> ContactId -> UserId -> ChatId -> ChatHandler Value
postChatMsgRemoveUndoR sid _cid rid xid = do

    checkAuthorized sid
    
    liftHandler $ runDB $ update $ \x -> do
        set x [ChatRemovedRecipient =. val False]
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatRecipient ==. val sid
    
    liftHandler $ runDB $ update $ \x -> do
        set x [ChatRemovedAuthor =. val False]
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatAuthor ==. val sid

    chat <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatId ==. val xid
        return x

    let channelId = Line (sid, rid)

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap
    
    let response = object [ "chatId" .= xid
                          , "type" .= WsMessageTypeUndo
                          , "source" .= sid
                          , "recipient" .= rid
                          , "message" .= (commonmarkToHtml [] . chatMessage . entityVal <$> chat)
                          ]

    atomically $ case maybeChan of
      Nothing -> return ()
      
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText response

    return response


deleteChatMsgRemoveR :: UserId -> ContactId -> UserId -> ChatId -> ChatHandler Value
deleteChatMsgRemoveR sid cid rid xid = do

    checkAuthorized sid
    
    chat <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatId ==. val xid
        return x

    case chat of
      Just (Entity _ (Chat aid' rid' _ _ _ _ _ _ _ _ _ _ _))
          | aid' == sid -> liftHandler $ runDB $ update $ \x -> do
                set x [ ChatRemovedAuthor =. val True ]
                where_ $ x ^. ChatId ==. val xid
                where_ $ x ^. ChatAuthor ==. val sid
                                                                 
          | rid' == rid -> liftHandler $ runDB $ update $ \x -> do
                set x [ ChatRemovedRecipient =. val True ]
                where_ $ x ^. ChatId ==. val xid
                where_ $ x ^. ChatRecipient ==. val sid
      
          | otherwise -> return ()
          
      Nothing -> return ()
    

    let channelId = Line (sid, rid)

    ChatRoom channelMapTVar <- getSubYesod
    channelMap <- readTVarIO channelMapTVar
    let maybeChan = M.lookup channelId channelMap
    rndr <- getUrlRender
    rtp <- getRouteToParent
    let expath = decodeUtf8 . extractPath . encodeUtf8 . rndr
    
    let response = object [ "chatId" .= xid
                          , "type" .= WsMessageTypeRemove
                          , "source" .= sid
                          , "recipient" .= rid
                          , "links" .= object
                            [ "undo" .= expath (rtp $ ChatMsgRemoveUndoR sid cid rid xid) 
                            ]
                          ]

    atomically $ case maybeChan of
      Nothing -> return ()
      
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText response

    return response


deleteChatMsgDeleteR :: UserId -> ContactId -> UserId -> ChatId -> ChatHandler Value
deleteChatMsgDeleteR sid _cid rid xid = do

    checkAuthorized sid

    liftHandler $ runDB $ delete $ do
        x <- from $ table @Chat
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatAuthor ==. val sid

    let channelId = Line (sid, rid)

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    let response = object [ "chatId" .= xid
                          , "type" .= WsMessageTypeDelete
                          , "source" .= sid
                          , "recipient" .= rid
                          , "links" .= object []
                          ]

    atomically $ case maybeChan of
      Nothing -> return ()
      
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText response

    return response


postChatMsgReadR :: UserId -> ContactId -> UserId -> ChatId -> ChatHandler ()
postChatMsgReadR sid _cid rid xid = do

    checkAuthorized sid
    
    now <- liftIO getCurrentTime

    liftHandler $ runDB $ update $ \x -> do
        set x [ ChatDelivered =. val True
              , ChatTimeDelivered =. just (val now)
              , ChatRead =. val True
              , ChatTimeRead =. just (val now)
              ]
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatRecipient ==. val sid
        where_ $ isNothing_ $ x ^. ChatTimeDelivered

    liftHandler $ runDB $ update $ \x -> do
        set x [ ChatDelivered =. val True
              , ChatRead =. val True
              , ChatTimeRead =. just (val now)
              ]
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatRecipient ==. val sid
        where_ $ not_ $ isNothing_ $ x ^. ChatTimeDelivered

    let channelId = Line (sid, rid)

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    let response = object [ "chatId" .= xid
                          , "type" .= WsMessageTypeRead
                          , "author" .= rid
                          , "recipient" .= sid
                          ]

    atomically $ case maybeChan of
      Nothing -> return ()
      
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText response


postChatMsgDeliveredR :: UserId -> ContactId -> UserId -> ChatId -> ChatHandler ()
postChatMsgDeliveredR sid _cid rid xid = do

    checkAuthorized sid
    
    now <- liftIO getCurrentTime

    liftHandler $ runDB $ update $ \x -> do
        set x [ ChatDelivered =. val True
              , ChatTimeDelivered =. just (val now)
              ]
        where_ $ x ^. ChatId ==. val xid
        where_ $ x ^. ChatRecipient ==. val sid

    let channelId = Line (sid, rid)

    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup channelId channelMap

    let response = object [ "chatId" .= xid
                          , "type" .= WsMessageTypeDelivered
                          , "author" .= rid
                          , "recipient" .= sid
                          ]

    atomically $ case maybeChan of
      Nothing -> return ()
      
      Just (writeChan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          writeTChan writeChan $ toStrict $ encodeToLazyText response


getChatChannelR :: UserId -> ContactId -> UserId -> ChatHandler ()
getChatChannelR sid cid rid = do
    checkAuthorized sid
    webSockets (chatApp sid cid rid)


checkAuthorized :: UserId -> ChatHandler ()
checkAuthorized sid = do

    muid <- liftHandler getMaybeAuthId

    case muid of
      Nothing -> permissionDeniedI MsgAuthenticationRequired
      
      Just uid | uid /= sid -> permissionDeniedI MsgAnotherAccountAccessProhibited
               | otherwise -> return ()


getChatRoomR :: UserId -> ContactId -> UserId -> ChatHandler Html
getChatRoomR sid cid rid = do

    checkAuthorized sid
    
    backlink <- liftHandler $ getBacklink sid rid
    
    photos <- liftHandler $ getAccountPhotoRoute sid
    photor <- liftHandler $ getAccountPhotoRoute rid
    contact  <- liftHandler $ getContactRoute sid rid cid
    icon <- liftHandler $ getStaticRoute img_call_FILL0_wght400_GRAD0_opsz24_svg
    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    video <- liftHandler $ getVideoPushRoute sid cid rid
    outgoing <- liftHandler $ getVideoOutgoingRoute sid cid rid False
    wallpaper <- liftHandler $ getStaticRoute img_wallpaper_pattern_svg

    user <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x

    endpoint <- liftHandler $ lookupGetParam paramEndpoint

    subscribed <- liftHandler $ (isJust <$>) $ runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val sid
        where_ $ x ^. PushSubscriptionPublisher ==. val rid
        where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
        return x

    interlocutor <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x

    online <- do
        channels <- (\(ChatRoom x) -> readTVarIO x) =<< getSubYesod
        let line = Line (sid, rid)
        return $ not $ null $ M.filterWithKey
            (\l@(Line (s,r)) (_,n) -> let onThisLine = (l == line) && (((s == rid) && (n > 0)) || ((r == rid) && (n > 1)))
                                          onAnotherFullLine = (l /= line) && ((s == rid) || (r == rid)) && (n > 1)
                                          onAnotherHalfLine = (l /= line) && (s == rid) && (n == 1)
                                      in
                                        onThisLine || onAnotherFullLine || onAnotherHalfLine
            )
            channels
        
    accessible <- liftHandler $ (isJust <$>) $ runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        where_ $ just (x ^. PushSubscriptionEndpoint) !=. val endpoint
        return x 
    
    loop <- liftHandler $ (isJust <$>) $ runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        where_ $ just (x ^. PushSubscriptionEndpoint) ==. val endpoint
        return x 

    (fw0,et0) <- generateFormPost formChatDelete

    chats <- liftHandler $ runDB $ select $ from $
        ( do
              x :& o :& r :& a <- from $ table @Chat
                  `innerJoin` table @User `on` (\(x :& o) -> x ^. ChatAuthor ==. o ^. UserId)
                  `leftJoin` table @Chat `on` (\(x :& _ :& r) -> x ^. ChatReply ==. r ?. ChatId)
                  `leftJoin` table @User `on` (\(_ :& _ :& r :& a) -> r ?. ChatAuthor ==. a ?. UserId)
              where_ $ x ^. ChatAuthor ==. val sid
              where_ $ x ^. ChatRecipient ==. val rid
              return (x,(o,(r,a)))
        )
        `unionAll_`
        ( do
              x :& o :& r :& a <- from $ table @Chat
                  `innerJoin` table @User `on` (\(x :& o) -> x ^. ChatAuthor ==. o ^. UserId)
                  `leftJoin` table @Chat `on` (\(x :& _ :& r) -> x ^. ChatReply ==. r ?. ChatId)
                  `leftJoin` table @User `on` (\(_ :& _ :& r :& a) -> r ?. ChatAuthor ==. a ?. UserId)
              where_ $ x ^. ChatRecipient ==. val sid
              where_ $ x ^. ChatAuthor ==. val rid
              where_ $ not_ $ x ^. ChatRemovedRecipient
              return (x,(o,(r,a)))
        )

    let dayLogs = sortBy (\(d1,_) (d2,_) -> compare d1 d2)
             $ (sortBy (\(Entity _ c1,_) (Entity _ c2,_) -> compare (chatCreated c1) (chatCreated c2)) <$>)
            <$> groupByDay chats
    
    rtp <- getRouteToParent
    curr <- (rtp <$>) <$> getSubCurrentRoute
    rndr <- getUrlRender

    callerName <- liftHandler $ (resolveName <$>) $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x 

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
    msgs <- getMessages
    
    liftHandler $ defaultLayout $ do
        setTitleI MsgChats
        idDivInterlocutorPhoto <- newIdent
        idBadgeInterlocutorStatus <- newIdent
        idDivInterlocutorName <- newIdent
        idInterlocutorStatus <- newIdent
        idButtonVideoCall <- newIdent
        idButtonAudioCall <- newIdent
        idMenuChat <- newIdent
        idItemDleteChat <- newIdent
        idMain <- newIdent
        idChatOutput <- newIdent
        classBubbleRow <- newIdent
        idBubbleRowPref <- newIdent
        classBubble <- newIdent
        idBubblePref <- newIdent
        classReplyRef <- newIdent
        classBlockquoteReplyRef <- newIdent
        classBubbleContent <- newIdent
        classBubbleStatusLine <- newIdent
        classBubbleStatus <- newIdent
        classRemoved <- newIdent
        classMenuAnchor <- newIdent
        idBubbleMenuPref <- newIdent
        classActionReply <- newIdent
        classActionCopy <- newIdent
        classActionDelete <- newIdent
        idOverlayDialogDeletePref <- newIdent
        classDeleteActions <- newIdent
        idDialogDeletePref <- newIdent
        idTypingIndicatorRow <- newIdent
        idTypingIndicator <- newIdent
        idTypingDots <- newIdent
        idMessageForm <- newIdent
        idMessageInput <- newIdent
        idButtonSend <- newIdent
        idAudioOutgoingChat <- newIdent
        idAudioIncomingChat <- newIdent
        idFormDeleteChat <- newIdent
        idOverlayDialogOutgoingCall <- newIdent
        idDialogOutgoingCall <- newIdent
        idAudioOutgoingCallRingtone <- newIdent
        idButtonOutgoingCallCancel <- newIdent
        idOverlayDialogCallDeclined <- newIdent
        idDialogCallDeclined <- newIdent
        idOverlayDialogDeleteChat <- newIdent
        idDialogDeleteChat <- newIdent
        
        $(widgetFile "chat/room")

    where      
      resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)

      groupByDay = M.toList . groupByKey (\(Entity _ (Chat _ _ _ t _ _ _ _ _ _ _ _ _),_) -> utctDay t)


data WsocketsMessage = WsocketsMessage
    { wsmType :: !WsMessageType
    , wsmMessage :: !Text
    , wsmReply :: !(Maybe ChatId)
    }
    deriving (Show, Read)

instance FromJSON WsocketsMessage where
    parseJSON :: Value -> Parser WsocketsMessage
    parseJSON = withObject "WsocketsMessage" $ \o -> do
        wsmType <- o .: "type"
        wsmMessage <- o .: "message"
        wsmReply <- o .:? "reply"
        return WsocketsMessage {..}


instance ToJSON WsocketsMessage where
    toJSON :: WsocketsMessage -> Value
    toJSON (WsocketsMessage {..}) = object [ "type" .= wsmType
                                           , "message" .= wsmMessage
                                           , "reply" .= wsmReply
                                           ]


chatApp :: YesodChat m => UserId -> ContactId -> UserId -> WebSocketsT (SubHandlerFor ChatRoom m) ()
chatApp authorId contactId recipientId = do
    let channelId = Line (authorId, recipientId)
    
    ChatRoom channelMapTVar <- getSubYesod

    channelMap <- readTVarIO channelMapTVar
    
    let maybeChan = M.lookup channelId channelMap

    writeChan <- case maybeChan of
      Nothing -> do
          chan <- atomically newBroadcastTChan
          atomically $ writeTVar channelMapTVar $ M.insert channelId (chan,1) channelMap 
          return chan
          
      Just (writeChan,_) -> do
          atomically $ writeTVar channelMapTVar $ M.alter userJoinedChannel channelId channelMap
          channels <- readTVarIO channelMapTVar
          forM_ channels $ \(chan,n) -> do 
              atomically $ writeTChan chan $ toStrict $ encodeToLazyText $ object
                  [ "type" .= WsMessageTypeOnline
                  , "user" .= authorId
                  , "users" .= n
                  ]
          return writeChan

    readChan <- atomically $ dupTChan writeChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (runConduit $ (sourceWS .|) $ mapM_C $ \json -> do
              let input = decode (TL.encodeUtf8 json)
              case input of
                Nothing -> return ()
                
                Just (WsocketsMessage WsMessageTypeTyping _ _) -> do

                  atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText $ object
                      [ "type" .= WsMessageTypeTyping
                      , "author" .= authorId
                      , "recipient" .= recipientId
                      ]
                      
                Just (WsocketsMessage _type msg reply) -> do
                  now <- liftIO getCurrentTime
                  let chat = Chat authorId recipientId ChatTypeMessage now msg
                                  False Nothing False Nothing False False Nothing reply
                  cid <- liftHandler $ runDB $ insert chat

                  author <- liftHandler $ runDB $ selectOne $ do
                      x <- from $ table @User
                      where_ $ x ^. UserId ==. val (chatAuthor chat)
                      return x

                  replied <- case reply of
                    Nothing -> return Nothing
                    Just rid' -> liftHandler $ runDB $ selectOne $ do
                        x :& o <- from $ table @Chat
                            `innerJoin` table @User `on` (\(x :& o) -> x ^. ChatAuthor ==. o ^. UserId)
                        where_ $ x ^. ChatId ==. val rid'
                        return (x,o)
                  
                  rndr <- getUrlRender
                  rtp <- getRouteToParent

                  let expath = decodeUtf8 . extractPath . encodeUtf8 . rndr

                  atomically $ writeTChan writeChan $ toStrict $ encodeToLazyText $ object $
                      [ "type" .= WsMessageTypeChat
                      , "chatId" .= cid
                      , "author" .= chatAuthor chat
                      , "authorName" .= resolveName author
                      , "recipient" .= chatRecipient chat
                      , "created" .= chatCreated chat
                      , "message" .= commonmarkToHtml [] (chatMessage chat)
                      , "links" .= object
                        [ "delivered" .= expath (rtp $ ChatMsgDeliveredR recipientId contactId authorId cid)
                        , "read" .= expath (rtp $ ChatMsgReadR recipientId contactId authorId cid)
                        , "dismiss" .= expath (rtp $ ChatMsgRemoveR recipientId contactId authorId cid)
                        , "remove" .= expath (rtp $ ChatMsgRemoveR authorId contactId recipientId cid)
                        , "delete" .= expath (rtp $ ChatMsgDeleteR authorId contactId recipientId cid)
                        ]
                      ] <> maybe [] (\(Entity cid' (Chat aid _ _ _ msg' _ _ _ _ _ _ _ _),a) ->
                                       ["replied" .= object
                                        [ "type" .= WsMessageTypeChat
                                        , "chatId" .= cid'
                                        , "author" .= aid             
                                        , "authorName" .= resolveName (Just a)
                                        , "message" .= commonmarkToHtml [] msg'
                                        ]
                                       ]
                                    ) replied
                      
                  _ <- forkIO $ do
                      threadDelay 2000000

                      chat' <- liftHandler $ runDB $ selectOne $ do
                          x <- from $ table @Chat
                          where_ $ x ^. ChatId ==. val cid
                          where_ $ not_ $ x ^. ChatDelivered
                          where_ $ not_ $ x ^. ChatRead
                          return x

                      case chat' of
                        Just (Entity _ (Chat aid rid _ _ message _ _ _ _ _ _ _ _)) -> do

                            mVapidKeys <- liftHandler getVapidKeys
                            case mVapidKeys of
                              Just vapidKeys -> do

                                  subscriptions <- liftHandler $ runDB $ select $ do
                                      x <- from $ table @PushSubscription
                                      where_ $ x ^. PushSubscriptionPublisher ==. val aid
                                      where_ $ x ^. PushSubscriptionSubscriber ==. val rid
                                      return x

                                  sender <- liftHandler $ runDB $ selectOne $ do
                                      x <- from $ table @User
                                      where_ $ x ^. UserId ==. val aid
                                      return x


                                  iconr <- liftHandler $ getStaticRoute img_chat_FILL0_wght400_GRAD0_opsz24_svg
                                  msgr <- getMessageRender
                                  Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

                                  forM_ subscriptions $ \(Entity _ (PushSubscription sid pid endpoint p256dh auth _)) -> do
                                      photor <- liftHandler $ getAccountPhotoRoute pid
                                      let notification = mkPushNotification endpoint p256dh auth
                                              & pushMessage .~ object
                                                  [ "title" .= (msgr MsgAppName <> ": " <> msgr MsgNewMessage)
                                                  , "icon" .= expath iconr
                                                  , "image" .= expath photor
                                                  , "body" .= message
                                                  , "messageType" .= PushMsgTypeChat
                                                  , "targetRoom" .= (expath . rtp $ ChatRoomR sid contactId pid)
                                                  , "senderId" .= pid
                                                  , "senderName" .= (
                                                        (\u -> fromMaybe (userEmail u) (userName u)) . entityVal <$> sender
                                                                    )
                                                  , "recipientId" .= sid
                                                  , "links" .= object
                                                    [ "delivered" .= ( expath . rtp $
                                                                       ChatMsgDeliveredR recipientId contactId authorId cid
                                                                     )
                                                    ]
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
                                            set x [ChatDelivered =. val True]
                                            where_ $ x ^. ChatId ==. val cid

                              Nothing -> do
                                  liftIO $ print @Text "No VAPID details"
                                  liftHandler $ addMessageI statusError MsgPushNotificationExcception

                        Nothing -> return ()

                  return ()
        )
    case e of
      Left _ -> do
          channels <- readTVarIO channelMapTVar
          let newChannels = M.alter userLeftChannel channelId channels
          atomically $ do
              writeTVar channelMapTVar newChannels
              forM_ newChannels $ \(chan,n) -> do 
                  writeTChan chan $ toStrict $ encodeToLazyText $ object
                      [ "type" .= WsMessageTypeOffline
                      , "user" .= authorId
                      , "users" .= n
                      ]
                  
      Right () -> return ()

  where
      resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)


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
