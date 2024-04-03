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

import Model (UserId, ContactId)

import Yesod.Core (renderRoute)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

newtype ChatRoom = ChatRoom (TVar (M.Map (S.Set UserId) (TChan Text, Int)))

mkYesodSubData "ChatRoom" [parseRoutes|
/acknowledge AcknowledgeR POST
/#UserId/#UserId/#ContactId/room ChatRoomR GET
|]
