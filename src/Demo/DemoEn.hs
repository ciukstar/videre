{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.FileEmbed (embedFile)
import Data.Time
    ( utcToLocalTime, addLocalTime, nominalDay, getCurrentTimeZone, addUTCTime
    )
import Data.Time.Calendar (addGregorianYearsClip)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.LocalTime (localDay, localTimeOfDay)

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
    )

import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Auth.Email (saltPass)
import Yesod.Form.Fields (Textarea(Textarea))
import Yesod.Persist(PersistStoreWrite (insert, insert_))
import Settings (AppSettings (appDevelopment))


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn appSettings = do

    tz <- liftIO getCurrentTimeZone
    now <- liftIO getCurrentTime
    let today = utctDay now
    let localNow = utcToLocalTime tz now

    if appDevelopment appSettings
        then do    
        tid <- insert Token { tokenApi = apiInfoVapid
                            , tokenStore = StoreTypeDatabase
                            }
        insert_ Store { storeToken = tid
                      , storeKey = "VAPID triple"
                      , storeVal = "(22879504107471320671126810649052900428463951865620953018730582802067053764751,106963013057532433758118925351970093795784080248924999494142080730165234056952,106312979860921952110239664650581548265214157776781431141487974369128419274671)"
                      }
        else do
        insert_ Token { tokenApi = apiInfoGoogle
                      , tokenStore = StoreTypeGoogleSecretManager
                      }
    
        insert_ Token { tokenApi = apiInfoVapid
                      , tokenStore = StoreTypeGoogleSecretManager
                      }
    
    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass1
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Mary Lopez"
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

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass2
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "John Johnson"
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

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass3
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Julian Maulsby"
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

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userAuthType = UserAuthTypeEmail
                     , userPassword = Just pass4
                     , userVerkey = Just "xxxYYYzzz"
                     , userVerified = True
                     , userName = Just "Valentina Schoen"
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
