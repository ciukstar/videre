{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatRoom.Data where

import Model (UserId)

import Text.Shakespeare.I18N (RenderMessage (renderMessage))

import Yesod.Core (renderRoute, mkMessage)
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

data ChatRoom = ChatRoom

mkMessage "ChatRoom" "messages" "en"

mkYesodSubData "ChatRoom" [parseRoutes|
/#UserId/#UserId/room ChatRoomR GET
|]
