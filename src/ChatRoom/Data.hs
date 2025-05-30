{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatRoom.Data where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

import Model (UserId, ContactId, ChatId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChatRoom = ChatRoom (TVar (M.Map (S.Set UserId) (TChan Text, Int)))

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

