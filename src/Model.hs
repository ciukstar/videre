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
{-# LANGUAGE InstanceSigs #-}

module Model where

import Control.Applicative (pure)
import Control.Monad (return)

import ClassyPrelude.Yesod
    ( Typeable, mkMigrate, mkPersist, persistFileWith
    , share, sqlSettings
    )

import Data.Aeson
    ( Value (String), ToJSON, toJSON, FromJSON, parseJSON, withObject
    , (.:)
    )
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)

import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq (Eq ((==)))
import Data.Ord (Ord)
import Data.Function ((.), ($))
import Data.String (String)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)

import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.TH (derivePersistField)

import Text.Read (Read)
import Text.Show (Show (show))
import Text.Hamlet (Html)


data PushMsgType = PushMsgTypeMessage | PushMsgTypeVideoCall | PushMsgTypeAudioCall
                 | PushMsgTypeAccept | PushMsgTypeDecline | PushMsgTypeCancel
                 | PushMsgTypeIgnore | PushMsgTypeEndSession
    deriving (Eq, Show, Read)


instance ToJSON PushMsgType where
    toJSON :: PushMsgType -> Data.Aeson.Value
    toJSON = String . pack . show


data ChatMessageStatus = ChatMessageStatusRead | ChatMessageStatusUnread
    deriving (Show, Read, Eq, Ord)
derivePersistField "ChatMessageStatus"


instance ToJSON ChatMessageStatus where
    toJSON :: ChatMessageStatus -> Value
    toJSON ChatMessageStatusRead = "Read"
    toJSON ChatMessageStatusUnread = "Unread"

instance FromJSON ChatMessageStatus where
    parseJSON :: Value -> Parser ChatMessageStatus
    parseJSON (String "Read") = pure ChatMessageStatusRead
    parseJSON (String "Unread") = pure ChatMessageStatusUnread
    parseJSON invalid = prependFailure "parsing ChatMessageStatus failed" (typeMismatch "String" invalid)


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


instance FromJSON PushSubscription where
    parseJSON :: Value -> Parser PushSubscription
    parseJSON = withObject "PushSubscription" $ \v -> do
        subscriber <- v .: "subscriber"
        publisher <- v .: "publisher"
        endpoint <- v .: "endpoint"
        keys <- v .: "keys"
        keyP256dh <- keys .: "p256dh"
        keyAuth <- keys .: "auth"
        return $ PushSubscription subscriber publisher endpoint keyP256dh keyAuth


instance Eq User where
    (==) :: User -> User -> Bool
    a == b = userEmail a == userEmail b


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

secretVolumeVapid :: String
secretVolumeVapid = "/vapid/vapid_min_details"

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
