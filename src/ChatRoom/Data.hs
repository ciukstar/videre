{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module ChatRoom.Data where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)

import qualified Data.Map as M
import Data.Text (Text)

import Model (UserId, ContactId, ChatId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)


newtype Line = Line (UserId, UserId)

instance Eq Line where
    (==) :: Line -> Line -> Bool
    Line l1 == Line l2@(s',r') = l1 == l2 || l1 == (r',s')

instance Ord Line where
    (<=) :: Line -> Line -> Bool
    Line (s,r) <= Line (s',r') = s <= s' && r <= r'


newtype ChatRoom = ChatRoom (TVar (M.Map Line (TChan Text, Int)))

mkYesodSubData "ChatRoom" [parseRoutes|
/room/#UserId/#ContactId/#UserId/channel ChatChannelR     GET

/room/#UserId/#ContactId/#UserId/delete            ChatDeleteR        POST
/room/#UserId/#ContactId/#UserId/#ChatId/undo      ChatMsgRemoveUndoR POST
/room/#UserId/#ContactId/#UserId/#ChatId/remove    ChatMsgRemoveR     DELETE
/room/#UserId/#ContactId/#UserId/#ChatId/delete    ChatMsgDeleteR     DELETE
/room/#UserId/#ContactId/#UserId/#ChatId/read      ChatMsgReadR       POST
/room/#UserId/#ContactId/#UserId/#ChatId/delivered ChatMsgDeliveredR  POST

/room/#UserId/#ContactId/#UserId ChatRoomR GET
|]

