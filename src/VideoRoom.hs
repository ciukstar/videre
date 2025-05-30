{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module VideoRoom
  ( module VideoRoom.Data, module VideoRoom
  ) where

import Conduit ((.|), mapM_C, runConduit, MonadIO (liftIO))

import Control.Applicative ((<|>))
import Control.Lens ((.~), (?~))
import Control.Monad (forever, forM_)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val
    , (^.), (==.), (=.)
    , just, update, set
    )
import Database.Persist (Entity (Entity), entityVal, insert, insert_)
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A (Value)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import qualified Data.Map as M ( lookup, insert, alter )
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Foundation
    ( AppMessage
      ( MsgNotGeneratedVAPID, MsgCallEnded, MsgInterlocutorEndedSession
      , MsgEndSession, MsgAppName, MsgBack, MsgVideoCall, MsgAudioCall
      )
    )

import Model
    ( UserId, User (User, userEmail, userName)
    , paramBacklink
    , PushSubscription (PushSubscription), ContactId
    , UserPhoto (UserPhoto)
    , Call (Call, callCaller, callCallee, callStart, callEnd, callType, callStatus)
    , CallType (CallTypeVideo, CallTypeAudio)
    , CallStatus
      ( CallStatusAccepted, CallStatusDeclined, CallStatusEnded
      , CallStatusCanceled
      )
    , Chat (Chat), ChatType (ChatTypeVideoCall, ChatTypeAudioCall)
    , PushMsgType
      ( PushMsgTypeVideoCall, PushMsgTypeEndSession, PushMsgTypeAudioCall
      , PushMsgTypeAccept, PushMsgTypeDecline, PushMsgTypeCancel
      )
    , EntityField
      ( UserId
      , UserPhotoUser, PushSubscriptionSubscriber
      , PushSubscriptionPublisher, CallId, CallStatus, CallEnd
      )
    )

import Network.HTTP.Client (Manager)
import Network.HTTP.Types (extractPath)

import UnliftIO.Exception (try, SomeException)
import UnliftIO.STM
    ( atomically, readTVarIO, writeTVar, newTQueue, readTQueue, writeTQueue
    )

import Settings
    ( widgetFile, AppSettings (appSuperuser)
    , Superuser (Superuser, superuserUsername)
    )
import Settings.StaticFiles
    ( img_call_end_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)
import Text.Julius (RawJS(rawJS))
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)

import VideoRoom.Data
    ( resourcesVideoRoom, channelMapTVar
    , VideoRoom (VideoRoom), ChanId (ChanId)
    , Route (WebSoketR, PushMessageR, RoomR, PhotoR)
    )

import Web.WebPush
    ( mkPushNotification, pushMessage, pushSenderEmail, pushExpireInSeconds
    , sendPushNotification, pushUrgency, PushUrgency (PushUrgencyHigh)
    , pushTopic, PushTopic (PushTopic), VAPIDKeys
    )

import Yesod
    ( Yesod, YesodSubDispatch, yesodSubDispatch , mkYesodSubDispatch
    , SubHandlerFor, MonadHandler (liftHandler) , getSubYesod
    , Application, newIdent , YesodPersist (YesodPersistBackend)
    , RenderMessage , FormMessage, HandlerFor, lookupPostParam, getRouteToParent
    )
import Yesod.Core (defaultLayout)
import Yesod.Core.Content (TypedContent (TypedContent), toContent)
import Yesod.Core.Handler (invalidArgsI, getUrlRender, getMessageRender)
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Input (runInputGet, runInputPost, ireq, iopt)
import Yesod.Form.Fields (boolField, intField, textField)
import Yesod.Persist.Core (runDB)
import Yesod.Static (StaticRoute)
import Yesod.WebSockets
    ( WebSocketsT, sendTextData, race_, sourceWS, webSockets
    )


class YesodVideo m where
    getRtcPeerConnectionConfig :: HandlerFor m (Maybe A.Value)
    getAppHttpManager :: HandlerFor m Manager
    getHomeRoute :: HandlerFor m Text
    getStaticRoute :: StaticRoute -> HandlerFor m (Route m)
    getAppSettings :: HandlerFor m AppSettings
    getVapidKeys :: HandlerFor m (Maybe VAPIDKeys)


getRoomR :: (Yesod m, YesodVideo m)
         => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
         => (RenderMessage m AppMessage, RenderMessage m FormMessage)
         => UserId -> ContactId -> UserId -> Bool -> SubHandlerFor VideoRoom m Html
getRoomR sid cid rid polite = do

    backlink <- liftHandler $ getHomeRoute >>= \r -> fromMaybe r <$> runInputGet ( iopt textField paramBacklink )

    videor <- runInputGet (ireq boolField "videor")
    audior <- runInputGet (ireq boolField "audior")

    videos <- runInputGet (ireq boolField "videos")
    audios <- runInputGet (ireq boolField "audios")

    let visibilityVideoR = if videor then "visible" else "hidden" :: Text
    let visibilityPlaceholderR = if videor then "hidden" else "visible" :: Text

    let visibilityVideoS = if videos then "visible" else "hidden" :: Text
    let visibilityPlaceholderS = if videos then "hidden" else "visible" :: Text

    toParent <- getRouteToParent

    config <- liftHandler $ fromMaybe (object []) <$> getRtcPeerConnectionConfig

    interlocutorName <- liftHandler $ resolveName <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val rid
        return x )

    msgr <- getMessageRender
    iconCallEnd <- liftHandler $ getStaticRoute img_call_end_FILL0_wght400_GRAD0_opsz24_svg

    liftHandler $ defaultLayout $ do
        idButtonExitSession <- newIdent
        idWrapperVideoRemote <- newIdent
        idVideoRemote <- newIdent
        idImgVideoRemotePlaceholder <- newIdent
        idWrapperVideoSelf <- newIdent
        idVideoSelf <- newIdent
        idImgVideoSelfPlaceholder <- newIdent
        idButtonSwitchVideocam <- newIdent
        idButtonVideoSwitch <- newIdent
        idButtonAudioSwitch <- newIdent
        idButtonEndSession <- newIdent
        idOverlay <- newIdent
        idDialogCallEnded <- newIdent
        $(widgetFile "video/session")
  where
      
      resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)


getWebSoketR :: (Yesod m, YesodPersist m, YesodPersistBackend m ~ SqlBackend)
             => ContactId -> Bool -> SubHandlerFor VideoRoom m ()
getWebSoketR cid polite = webSockets (wsApp cid polite)


postPushMessageR :: (Yesod m, YesodVideo m)
                 => (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
                 => (RenderMessage m FormMessage, RenderMessage m AppMessage)
                 => UserId -> ContactId -> UserId -> SubHandlerFor VideoRoom m ()
postPushMessageR sid cid rid = do

    messageType <- (\x -> x <|> Just PushMsgTypeVideoCall) . (readMaybe . unpack =<<) <$> lookupPostParam "messageType"
    messageTitle <- runInputPost $ iopt textField "title"
    icon <- lookupPostParam "icon"

    videor <- runInputPost $ ireq boolField "videor"
    audior <- runInputPost $ ireq boolField "audior"
    videos <- runInputPost $ ireq boolField "videos"
    audios <- runInputPost $ ireq boolField "audios"

    callId <- (toSqlKey <$>) <$> runInputPost (iopt intField "callId")

    messageBody <- runInputPost $ iopt textField "body"

    sender <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val sid
        return x

    subscriptions <- liftHandler $ runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionSubscriber ==. val rid
        where_ $ x ^. PushSubscriptionPublisher ==. val sid
        return x

    toParent <- getRouteToParent
    urlr <- getUrlRender
    manager <- liftHandler getAppHttpManager
    mVapidKeys <- liftHandler getVapidKeys

    msgr <- getMessageRender

    case mVapidKeys of
      Just vapidKeys -> do

          Superuser {..} <- liftHandler $ appSuperuser <$> getAppSettings

          now <- liftIO getCurrentTime

          call <- case messageType of
            Just PushMsgTypeVideoCall -> do
                liftHandler $ (pure <$>) $ runDB $ do
                    let call@(Call {..}) = Call { callCaller = sid
                                                , callCallee = rid
                                                , callStart = now
                                                , callEnd = Nothing
                                                , callType = CallTypeVideo
                                                , callStatus = Nothing
                                                }
                    cId <- insert call
                    insert_ $ Chat callCaller callCallee ChatTypeVideoCall now (msgr MsgVideoCall)
                                   False Nothing False Nothing False False (Just cId) Nothing
                    return cId
                    
            Just PushMsgTypeAudioCall -> do
                let call@(Call {..}) = Call { callCaller = sid
                                            , callCallee = rid
                                            , callStart = now
                                            , callEnd = Nothing
                                            , callType = CallTypeAudio
                                            , callStatus = Nothing
                                            }
                liftHandler $ (pure <$>) $ runDB $ do   
                    cId <- insert call
                    insert_ $ Chat callCaller callCallee ChatTypeAudioCall now (msgr MsgAudioCall)
                                   False Nothing False Nothing False False (Just cId) Nothing
                    return cId                        
                    
            _otherwise -> return Nothing

          let expath = decodeUtf8 . extractPath . encodeUtf8 . urlr

          forM_ subscriptions $ \(Entity _ (PushSubscription _ _ endpoint p256dh auth _)) -> do
                let notification = mkPushNotification endpoint p256dh auth
                        & pushMessage .~ object [ "title" .= messageTitle
                                                , "icon" .= icon
                                                , "image" .= (expath . toParent . PhotoR $ sid)
                                                , "body" .= messageBody
                                                , "messageType" .= messageType
                                                , "targetRoom" .= (expath . toParent $ RoomR rid cid sid True)
                                                , "targetPush" .= (expath . toParent $ PushMessageR rid cid sid)
                                                , "senderId" .= sid
                                                , "senderName" .= ( (userName . entityVal <$> sender)
                                                                    <|> (Just . userEmail . entityVal <$> sender)
                                                                  )
                                                , "recipientId" .= rid
                                                , "videor" .= videor
                                                , "audior" .= audior
                                                , "videos" .= videos
                                                , "audios" .= audios
                                                , "callId" .= (fromSqlKey <$> call)
                                                ]
                        & pushSenderEmail .~ superuserUsername
                        & pushExpireInSeconds .~ 60
                        & pushUrgency ?~ PushUrgencyHigh
                        & pushTopic .~ (PushTopic . pack . show <$> messageType)

                result <- sendPushNotification vapidKeys manager notification

                case result of
                  Left ex -> do
                      liftIO $ print ex
                  Right () -> do
                      case (messageType, callId) of

                        (Just PushMsgTypeAccept, Just cid') ->
                            liftHandler $ runDB $ update $ \x -> do
                            set x [CallStatus =. just (val CallStatusAccepted)]
                            where_ $ x ^. CallId ==. val cid'

                        (Just PushMsgTypeDecline, Just cid') ->
                            liftHandler $ runDB $ update $ \x -> do
                            set x [CallStatus =. just (val CallStatusDeclined)]
                            where_ $ x ^. CallId ==. val cid'

                        (Just PushMsgTypeEndSession, Just cid') ->
                            liftHandler $ runDB $ update $ \x -> do
                            set x [ CallStatus =. just (val CallStatusEnded)
                                  , CallEnd =. just (val now)
                                  ]
                            where_ $ x ^. CallId ==. val cid'

                        (Just PushMsgTypeCancel, Just cid') ->
                            liftHandler $ runDB $ update $ \x -> do
                            set x [ CallStatus =. just (val CallStatusCanceled)
                                  , CallEnd =. just (val now)
                                  ]
                            where_ $ x ^. CallId ==. val cid'

                        _otherwise -> return ()

                      return ()

      Nothing -> liftHandler $ invalidArgsI [MsgNotGeneratedVAPID]


userJoinedChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userJoinedChannel Nothing = Nothing
userJoinedChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers + 1)


userLeftChannel :: Num b => Maybe (a,b) -> Maybe (a,b)
userLeftChannel Nothing = Nothing
userLeftChannel (Just (writeChan,numUsers)) = Just (writeChan,numUsers - 1)


wsApp :: ContactId -> Bool -> WebSocketsT (SubHandlerFor VideoRoom m) ()
wsApp cid polite = do

    VideoRoom {..} <- getSubYesod

    channelMap <- readTVarIO channelMapTVar

    let maybeChan = M.lookup cid channelMap

    (chan,peer) <- atomically $ case maybeChan of
      Nothing -> do
          chan <- newTQueue
          peer <- newTQueue
          writeTVar channelMapTVar $ M.insert cid ((chan,peer),1) channelMap
          return (chan,peer)
      Just (chan,_) -> do
          writeTVar channelMapTVar $ M.alter userJoinedChannel cid channelMap
          return chan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTQueue (if polite then chan else peer)) >>= sendTextData)
        (runConduit (sourceWS .| mapM_C (
                          \msg -> do
                              -- liftIO $ print msg
                              atomically $ writeTQueue (if not polite then chan else peer) msg
                          )
                    ))
    case e of
      Left _ -> do
          m <- readTVarIO channelMapTVar
          let newChannelMap = M.alter userLeftChannel cid m
          atomically $ writeTVar channelMapTVar newChannelMap
      Right () -> return ()


getPhotoR :: (YesodPersist m, YesodPersistBackend m ~ SqlBackend)
          => UserId -> SubHandlerFor VideoRoom m TypedContent
getPhotoR uid = do
    photo <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    return $ case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> TypedContent "image/svg+xml" $ toContent [st|<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M480-480q-66 0-113-47t-47-113q0-66 47-113t113-47q66 0 113 47t47 113q0 66-47 113t-113 47ZM160-160v-112q0-34 17.5-62.5T224-378q62-31 126-46.5T480-440q66 0 130 15.5T736-378q29 15 46.5 43.5T800-272v112H160Zm80-80h480v-32q0-11-5.5-20T700-306q-54-27-109-40.5T480-360q-56 0-111 13.5T260-306q-9 5-14.5 14t-5.5 20v32Zm240-320q33 0 56.5-23.5T560-640q0-33-23.5-56.5T480-720q-33 0-56.5 23.5T400-640q0 33 23.5 56.5T480-560Zm0-80Zm0 400Z"/></svg>|]


instance ( Yesod m, YesodVideo m, RenderMessage m AppMessage
         , YesodPersist m, YesodPersistBackend m ~ SqlBackend
         , RenderMessage m FormMessage
         ) => YesodSubDispatch VideoRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv VideoRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesVideoRoom)
