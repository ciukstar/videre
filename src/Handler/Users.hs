{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Users
  ( getUsersR
  , getUserR
  , postUserDeleR
  , getUserEditR
  , getUserPhotoR
  , postUserR
  ) where

import Control.Monad (void, join)

import Data.Bifunctor (Bifunctor(second))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, Entity (entityVal), selectOne
    , (^.), (==.), (=.), (:&)((:&)), (?.)
    , where_, val, update, set, Value (unValue), leftJoin, on, just
    )
import Database.Persist
    ( Entity (Entity), PersistStoreWrite (delete), PersistUniqueWrite (upsert) )
import qualified Database.Persist as P ((=.))

import Foundation
    ( Handler, Form
    , widgetMainMenu, widgetSnackbar, widgetAccount
    , Route (DataR, StaticR)
    , DataR (UserR, UsersR, UserDeleR, UserEditR, UserPhotoR)
    , AppMessage
      ( MsgUsers, MsgNoUsersYet
      , MsgPhoto, MsgUser, MsgSave, MsgBack, MsgCancel, MsgEmailAddress, MsgYes
      , MsgAuthentication, MsgPassword, MsgVerificationKey, MsgVerified, MsgNo
      , MsgFullName, MsgGoogle, MsgEmail, MsgEdit, MsgDele, MsgConfirmPlease
      , MsgDeleteAreYouSure, MsgInvalidFormData, MsgRecordDeleted, MsgNotSpecified
      , MsgRecordEdited, MsgSuperuser, MsgAdministrator, MsgSuperuserCannotBeDeleted
      , MsgAttribution, MsgTakePhoto, MsgUploadPhoto, MsgClose
      )
    )

import Model
    ( statusError, statusSuccess
    , UserId
    , User
      ( User, userName, userAdmin )
    , EntityField
      ( UserId, UserPhotoUser, UserName, UserPhotoMime, UserPhotoPhoto
      , UserAdmin, UserSuperuser, UserPhotoAttribution
      )
    , AuthenticationType
      ( UserAuthTypeGoogle, UserAuthTypeEmail, UserAuthTypePassword)
    , UserPhoto (UserPhoto)
    )
    
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    )
    
import Text.Hamlet (Html)

import Yesod.Core
    ( defaultLayout, newIdent, getMessages, setUltDestCurrent
    , SomeMessage (SomeMessage), getMessageRender, FileInfo (fileContentType)
    , addMessageI, redirect, TypedContent (TypedContent), ToContent (toContent)
    , fileSourceByteString, MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields (fileField, textField, checkBoxField, htmlField)
import Yesod.Form.Functions (generateFormPost, runFormPost, mopt, mreq)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , FieldView (fvInput, fvId, fvErrors, fvLabel, fvRequired)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Maybe (isJust)


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = do
    superuser <- maybe False unValue <$> runDB ( selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        where_ $ x ^. UserSuperuser ==. val True
        return $ x ^. UserSuperuser )
        
    ((fr,fw0),et0) <- runFormPost formDelete
    case (fr,superuser) of
      (FormSuccess (),False) -> do
          runDB $ delete uid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR UsersR
          
      (FormSuccess (),True) -> do
          addMessageI statusError MsgSuperuserCannotBeDeleted
          redirect $ DataR $ UserR uid
          
      _otherwise -> do
          user <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
              x :& h <- from $ table @User
                  `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
              where_ $ x ^. UserId ==. val uid
              return (x, h ?. UserPhotoAttribution) )
              
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgUser
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/users/user")
          


getUserEditR :: UserId -> Handler Html
getUserEditR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
        
    (fw,et) <- generateFormPost $ formUser user
    
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "data/users/edit")


postUserR :: UserId -> Handler Html
postUserR uid = do
    ((fr,fw),et) <- runFormPost $ formUser Nothing
    case fr of
      FormSuccess (UserData name admin mfi attrib) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val name, UserAdmin =. val admin ]
              where_ $ x ^. UserId ==. val uid

          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs attrib)
                    [ UserPhotoMime P.=. fileContentType fi
                    , UserPhotoPhoto P.=. bs
                    , UserPhotoAttribution P.=. attrib
                    ]
            Nothing -> runDB $ update $ \x -> do
                set x [ UserPhotoAttribution =. val attrib ]
                where_ $ x ^. UserPhotoUser ==. val uid
              
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ UserR uid
          
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgUser
              $(widgetFile "data/users/edit") 


getUserR :: UserId -> Handler Html
getUserR uid = do

    user <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. UserId ==. val uid
        return (x, h ?. UserPhotoAttribution) )
    
    (fw0,et0) <- generateFormPost formDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/users/user")


formDelete :: Form ()
formDelete extra = return (FormSuccess (),[whamlet|#{extra}|])


data UserData = UserData
    !(Maybe Text) -- ^ userDataName
    !Bool -- ^ userDataAdmin
    !(Maybe FileInfo) -- ^ userDataPhoto
    !(Maybe Html) -- ^ userDataPhotoAttribution


formUser :: Maybe (Entity User) -> Form UserData
formUser user extra = do

    rndr <- getMessageRender
    
    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgFullName)]
        } (userName . entityVal <$> user)
        
    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = []
        } (userAdmin . entityVal <$> user)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case user of
      Just (Entity uid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @UserPhoto
          where_ $ x ^. UserPhotoUser ==. val uid
          return $ x ^. UserPhotoAttribution
      Nothing -> return Nothing
    
    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = UserData <$> nameR <*> adminR <*> photoR <*> attribR

    idOverlay <- newIdent
    idLabelPhoto <- newIdent
    idFigurePhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idDialogSnapshot <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCapture <- newIdent
    
    let w = $(widgetFile "data/users/form") 

    return (r,w)


getUsersR :: Handler Html
getUsersR = do

    users <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& h <- from $ table @User
            `leftJoin` table @UserPhoto `on` (\(x :& h) -> just (x ^. UserId) ==. h ?. UserPhotoUser)
        orderBy [desc (x ^. UserId)]
        return (x, h ?. UserPhotoAttribution) )

    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgUsers
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/users/users")


getUserPhotoR :: UserId -> Handler TypedContent
getUserPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> do
          superuser <- maybe False unValue <$> runDB ( selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserId ==. val uid
              return $ x ^. UserSuperuser )
          redirect $ if superuser
              then StaticR img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
              else StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
