{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoRo (fillDemoRo) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.FileEmbed (embedFile)

import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( apiInfoVapid, apiInfoGoogle, AuthenticationType (UserAuthTypeEmail)
    , User
      ( User, userEmail, userAuthType, userPassword, userVerkey, userVerified
      , userName, userSuperuser, userAdmin
      )
    , UserPhoto
      ( UserPhoto, userPhotoMime, userPhotoPhoto, userPhotoUser
      , userPhotoAttribution
      )
    , Token (Token, tokenApi, tokenStore)
    , Store (Store, storeToken, storeKey, storeVal)
    , StoreType (StoreTypeDatabase, StoreTypeGoogleSecretManager)
    , Ringtone (ringtoneName, Ringtone, ringtoneMime, ringtoneAudio)
    , DefaultRingtone (DefaultRingtone, defaultRingtoneRingtone, defaultRingtoneType)
    , RingtoneType
      ( RingtoneTypeCallOutgoing, RingtoneTypeCallIncoming, RingtoneTypeChatOutgoing
      , RingtoneTypeChatIncoming
      )
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)
import Yesod.Persist(PersistStoreWrite (insert, insert_))
import Settings (AppSettings (appDevelopment))


fillDemoRo :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRo appSettings = do

    if True -- appDevelopment appSettings
        then do    
        tid <- insert Token { tokenApi = apiInfoVapid
                            , tokenStore = StoreTypeDatabase
                            }
        insert_ Store { storeToken = tid
                      , storeKey = "VAPID triple"
                      , storeVal = "(8677994634232137212423785113544593598794450523536086396106826224669259901966,73724105402471006992357097408426893033736283540148048266080568591403695025869,107806506999142580483555956834506212618380772926302841504042472637602767614975)"
                      }
        else do
        insert_ Token { tokenApi = apiInfoGoogle
                      , tokenStore = StoreTypeGoogleSecretManager
                      }
    
        insert_ Token { tokenApi = apiInfoVapid
                      , tokenStore = StoreTypeGoogleSecretManager
                      }


    ringtone1 <- insert Ringtone { ringtoneName = "Ieșire cool"
                                 , ringtoneMime = "audio/mpeg"
                                 , ringtoneAudio = $(embedFile "demo/outgoing_call_galaxy_ringtones_1.mp3")
                                 }

    ringtone2 <- insert Ringtone { ringtoneName = "Vine cool"
                                 , ringtoneMime = "audio/mpeg"
                                 , ringtoneAudio = $(embedFile "demo/incoming_call_samsung_ringtones_1.mp3")
                                 }

    ringtone3 <- insert Ringtone { ringtoneName = "Chat ieșire"
                                 , ringtoneMime = "audio/mpeg"
                                 , ringtoneAudio = $(embedFile "demo/outgoing_message_ringtone_1.mp3")
                                 }

    ringtone4 <- insert Ringtone { ringtoneName = "Chat primit"
                                 , ringtoneMime = "audio/mpeg"
                                 , ringtoneAudio = $(embedFile "demo/incoming_message_ringtone_1.mp3")
                                 }

    insert_ Ringtone { ringtoneName = "Peste Orizont"
                     , ringtoneMime = "audio/mpeg"
                     , ringtoneAudio = $(embedFile "demo/over_the_horizon_ringtone.mp3")
                     }

    insert_ Ringtone { ringtoneName = "Flaut Roja"
                     , ringtoneMime = "audio/mpeg"
                     , ringtoneAudio = $(embedFile "demo/roja_flute_ringtone.mp3")
                     }

    insert_ DefaultRingtone { defaultRingtoneRingtone = ringtone1
                            , defaultRingtoneType = RingtoneTypeCallOutgoing
                            }

    insert_ DefaultRingtone { defaultRingtoneRingtone = ringtone2
                            , defaultRingtoneType = RingtoneTypeCallIncoming
                            }

    insert_ DefaultRingtone { defaultRingtoneRingtone = ringtone3
                            , defaultRingtoneType = RingtoneTypeChatOutgoing
                            }

    insert_ DefaultRingtone { defaultRingtoneRingtone = ringtone4
                            , defaultRingtoneType = RingtoneTypeChatIncoming
                            }
    
    pass1 <- liftIO $ saltPass "raduam"
    let user1 = User { userEmail = "raduam@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Radu Ana-Maria"
                     , userSuperuser = False
                     , userAdmin = True
                     }

    usr1 <- insert user1

    insert_ UserPhoto { userPhotoUser = usr1
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/happy-woman-gray-polo-shirt-with-pink-pin-button_53876-102858.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]
                      }

    pass2 <- liftIO $ saltPass "ionescuav"
    let user2 = User { userEmail = "ionescuav@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Ionescu Alexandru Victor"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr2 <- insert user2

    insert_ UserPhoto { userPhotoUser = usr2
                      , userPhotoMime = "image/avif"
                      , userPhotoPhoto = $(embedFile "demo/smiley-handsome-man-posing_23-2148911841.avif")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    pass3 <- liftIO $ saltPass "rususa"
    let user3 = User { userEmail = "rususa@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Rusu Ştefan Alexandru"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr3 <- insert user3

    insert_ UserPhoto { userPhotoUser = usr3
                      , userPhotoMime = "image/jpeg"
                      , userPhotoPhoto = $(embedFile "demo/portrait-handsome-man-blue-shirt-isolated-white_186202-8932.jpg")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }

    pass4 <- liftIO $ saltPass "mateiaa"
    let user4 = User { userEmail = "mateiaa@mail.ro"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Matei Andreea Alexandra"
                     , userSuperuser = False
                     , userAdmin = False
                     }

    usr4 <- insert user4

    insert_ UserPhoto { userPhotoUser = usr4
                      , userPhotoMime = "image/jpeg"
                      , userPhotoPhoto = $(embedFile "demo/portrait-smiling-blonde-woman_23-2148316635.jpg")
                      , userPhotoAttribution = Just [shamlet|
                                                            Designed by #
                                                            <a href="https://www.freepik.com/" target=_blank>
                                                              Freepik
                                                            |]

                      }
    return ()
