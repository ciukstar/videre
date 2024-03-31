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

module ChatRoom (module ChatRoom.Data, module ChatRoom) where

import ChatRoom.Data
    ( ChatRoom (ChatRoom), resourcesChatRoom
    , Route (ChatRoomR)
    , ChatRoomMessage (MsgPhoto)
    )

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.), SqlBackend
    )
import Database.Persist (Entity (Entity))
    
import Model (UserId, User (User), EntityField (UserId))

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod (defaultLayout), mkYesodSubDispatch, SubHandlerFor
    , YesodSubDispatch (yesodSubDispatch), MonadHandler (liftHandler)
    , Application, RenderMessage, HandlerFor
    )
import Yesod.Core.Types (YesodSubRunnerEnv)
import Yesod.Form.Fields (FormMessage)
import Yesod.Persist.Core (YesodPersist(runDB, YesodPersistBackend))


class ( Yesod m, RenderMessage m FormMessage
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


instance YesodChat m => YesodSubDispatch ChatRoom m where
    yesodSubDispatch :: YesodSubRunnerEnv ChatRoom m -> Application
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChatRoom)
