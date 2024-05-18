{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Accounts
  ( getAccountPhotoR
  , getAccountEditR
  , getAccountsR
  , getAccountR
  , postAccountR
  , getAccountInfoR
  , getAccountInfoEditR
  , postAccountInfoR
  ) where

import Control.Monad (void)

import Database.Persist
    ( Entity(Entity, entityVal), PersistUniqueWrite (upsert))
import qualified Database.Persist as P ((=.))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, update, set
    , (^.), (==.), (=.), Value (unValue)
    )

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Material3 ( md3textField, md3mopt, md3dayField )

import Model
    ( UserId, UserPhoto (UserPhoto), statusSuccess
    , EntityField
      ( UserPhotoUser, UserPhotoPhoto, UserPhotoMime, UserName, UserId
      , UserInfoUser, UserInfoBirthDate, UserSuperuser
      )
    , User (User, userName)
    , UserInfo (UserInfo, userInfoBirthDate)
    )

import Foundation ()
import Foundation.Data
    ( Handler, Widget
    , Route
      ( HomeR, StaticR, AuthR, AccountPhotoR, AccountEditR, AccountR
      , AccountInfoR, AccountInfoEditR
      )
    , AppMessage
      ( MsgUserAccount, MsgBack, MsgCancel, MsgFullName, MsgSignOut, MsgPhoto
      , MsgSave, MsgRecordEdited, MsgPersonalInfo, MsgAccount, MsgEdit
      , MsgBirthday, MsgSuperuser, MsgAdministrator, MsgNotIndicated
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import Widgets (widgetBanner, widgetSnackbar)

import Yesod.Auth (Route (LogoutR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessageRender
    , MonadHandler (liftHandler), redirect, FileInfo (fileContentType)
    , newIdent, fileSourceByteString, addMessageI, whamlet, getMessages
    )
import Yesod.Core.Content
    (TypedContent (TypedContent), ToContent (toContent))
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (fileField)
import Yesod.Form.Functions (generateFormPost, mopt, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))


postAccountInfoR :: UserId -> Handler Html
postAccountInfoR uid = do
    ((fr,fw),et) <- runFormPost $ formUserInfo uid Nothing
    case fr of
      FormSuccess r@(UserInfo _ bday) -> do
          void $ runDB $ upsert r [ UserInfoBirthDate P.=. bday ]
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AccountInfoR uid
      _otherwise -> do
          defaultLayout $ do
              setTitleI MsgPersonalInfo
              $(widgetFile "accounts/info/edit")


getAccountInfoEditR :: UserId -> Handler Html
getAccountInfoEditR uid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @UserInfo
        where_ $ x ^. UserInfoUser ==. val uid
        return x
    (fw,et) <- generateFormPost $ formUserInfo uid info
    defaultLayout $ do
        setTitleI MsgPersonalInfo
        $(widgetFile "accounts/info/edit")


formUserInfo :: UserId -> Maybe (Entity UserInfo)
             -> Html -> MForm Handler (FormResult UserInfo, Widget)
formUserInfo uid info extra = do
    rndr <- getMessageRender
    (bdayR,bdayV) <- md3mopt md3dayField FieldSettings
        { fsLabel = SomeMessage MsgBirthday
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgBirthday)]
        } (userInfoBirthDate . entityVal <$> info)

    let r = UserInfo uid <$> bdayR
    let w = [whamlet|
                    #{extra}
                    ^{fvInput bdayV}
                    |]
    return (r,w)


getAccountInfoR :: UserId -> Handler Html
getAccountInfoR uid = do

    info <- runDB $ selectOne $ do
        x <- from $ table @UserInfo
        where_ $ x ^. UserInfoUser ==. val uid
        return x

    (fw,et) <- generateFormPost $ formUserInfo uid info

    msgs <- getMessages
    
    defaultLayout $ do
        setTitleI MsgPersonalInfo
        idPanelInfo <- newIdent
        $(widgetFile "accounts/info/info")


postAccountR :: UserId -> Handler Html
postAccountR uid = do
    ((fr,fw),et) <- runFormPost $ formAccount Nothing
    case fr of
      FormSuccess (mname,mfi) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val mname ]
              where_ $ x ^. UserId ==. val uid
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs Nothing)
                    [UserPhotoMime P.=. fileContentType fi, UserPhotoPhoto P.=. bs]
            Nothing -> return ()
          addMessageI statusSuccess MsgRecordEdited
          redirect $ AccountR uid
      _otherwise -> defaultLayout $ do
          setTitleI MsgUserAccount
          $(widgetFile "accounts/edit")


getAccountR :: UserId -> Handler Html
getAccountR uid = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgUserAccount
        idPanelAccount <- newIdent
        $(widgetFile "accounts/account")


getAccountsR :: Handler Html
getAccountsR = undefined


getAccountEditR :: UserId -> Handler Html
getAccountEditR uid = do
    user <- maybeAuth
    (fw,et) <- generateFormPost $ formAccount user
    defaultLayout $ do
        setTitleI MsgUserAccount
        $(widgetFile "accounts/edit")


formAccount :: Maybe (Entity User)
            -> Html -> MForm Handler (FormResult (Maybe Text,Maybe FileInfo), Widget)
formAccount user extra = do
    rndr <- liftHandler getMessageRender
    (nameR,nameV) <- mopt md3textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", rndr MsgFullName)]
        } (userName . entityVal <$> user)
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    idUserIdent <- newIdent
    idLabelPhotoUser <- newIdent
    idFigurePhotoUser <- newIdent
    idImgPhotoUser <- newIdent

    return ( (,) <$> nameR <*> photoR
           , $(widgetFile "accounts/form")
           )


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
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
                     then
                       StaticR img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
                     else
                       StaticR img_person_FILL0_wght400_GRAD0_opsz24_svg
