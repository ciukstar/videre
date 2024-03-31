{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Foundation.Data where

import ChatRoom.Data (ChatRoom)

import Import.NoFoundation (Static, Manager, AppSettings)
import Database.Persist.Sql (ConnectionPool)
import Yesod.Core.Types     (Logger)
import Yesod.Core (mkMessage)
import Text.Shakespeare.I18N (RenderMessage (renderMessage))


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , getChatRoom    :: ChatRoom
    }

mkMessage "App" "messages" "en"
