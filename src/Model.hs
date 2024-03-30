{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators #-}

module Model where

import ClassyPrelude.Yesod
    ( Typeable, mkMigrate, mkPersist, persistFileWith
    , share, sqlSettings
    )

import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.String (String)
import Data.Text (Text)
import Data.Time.Calendar (Day)

import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.TH (derivePersistField)

import Text.Read (Read)
import Text.Show (Show)
import Text.Hamlet (Html)


data StoreType = StoreTypeDatabase | StoreTypeSession | StoreTypeGoogleSecretManager
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"

data AuthenticationType = UserAuthTypePassword
                        | UserAuthTypeEmail
                        | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")



gmailSender :: Text
gmailSender = "gmail_sender"

gmailAccessTokenExpiresIn :: Text
gmailAccessTokenExpiresIn = "gmail_access_token_expires_in"

gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"

gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"

apiInfoGoogle :: Text
apiInfoGoogle = "GOOGLE_API"


secretVolumeGmail :: String
secretVolumeGmail = "/grt/gmail_refresh_token"

secretVapid :: Text
secretVapid = "vapid_min_details"

apiInfoVapid :: Text
apiInfoVapid = "VAPID"

statusSuccess :: Text
statusSuccess = "success"

statusError :: Text
statusError = "error"


ultDestKey :: Text
ultDestKey = "_ULT"
