{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Control.Lens (folded, filtered, (^?), _2, to, (?~))
import qualified Control.Lens as L ((^.))
import Control.Monad.Logger (LogSource)

import Data.Aeson.Lens ( key, AsValue(_String) )
import Data.Kind (Type)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Base64.Lazy as B64L (encode)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Function ((&))
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)

import Import.NoFoundation

import Database.Esqueleto.Experimental as E
    ( select, selectOne, from, table, val, where_, unValue, asc
    , (==.), (^.)
    , orderBy, valList, in_, not_
    )
import Database.Persist.Sql (runSqlPool)

import Foundation.Data

import Material3 (md3passwordField, md3emailField)

import Network.Mail.Mime
    ( Part (partDisposition, partEncoding, partType, partContent, partHeaders, Part)
    , PartContent (PartContent), Disposition (DefaultDisposition)
    , Mail (mailParts, mailHeaders, mailTo), renderMail', Encoding (None)
    , Address (Address), emptyMail
    )
import qualified Network.Wreq as W (get, responseHeader, responseBody)
import Network.Wreq (defaults, auth, oauth2Bearer, postWith, post, FormParam ((:=)))
import qualified Network.Wreq.Lens as WL

import System.Directory (doesFileExist)
import System.IO (readFile')

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet ( hamletFile )
import Text.Jasmine ( minifym )
import Text.Shakespeare.Text (stext)
import Text.Printf (printf)

import VideoRoom.Data (Route(IncomingR, PushMessageR))

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Auth.Email
    ( authEmail, Email, Identifier, SaltedPass, VerKey, VerUrl
    , forgotPasswordR, registerR, loginR, setpassR
    , EmailCreds
      ( emailCredsId, emailCredsAuthId, emailCredsStatus, emailCredsVerkey
      , emailCredsEmail, EmailCreds
      )
    , YesodAuthEmail
      ( AuthEmailId, getEmail, getEmailCreds, setPassword, getPassword
      , verifyAccount, needOldPassword, setVerifyKey, getVerifyKey
      , sendVerifyEmail, addUnverified, afterPasswordRoute, emailLoginHandler
      , registerHandler, confirmationEmailSentResponse, setPasswordHandler
      , forgotPasswordHandler
      )
    )
import qualified Yesod.Auth.Email as AE (Email)
import Yesod.Auth.OAuth2.Google (oauth2GoogleScopedWidget)
import Yesod.Auth.Message
    ( AuthMessage
      ( ConfirmPass, NewPass, CurrentPassword, SetPassTitle, PasswordResetTitle
      , SendPasswordResetEmail, PasswordResetPrompt, Register, EnterEmail
      , RegisterLong, ConfirmationEmailSentTitle, NewPass, SetPass, InvalidLogin
      , LoginTitle
      )
    )

import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Romanian (romanianFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)


-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where

    errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    errorHandler NotFound = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPageNotFound
            idHeader <- newIdent
            idHeaderStart <- newIdent
            $(widgetFile "error/not-found")
        provideRep $ return $ object ["message" .= ("Page not found." :: Text)]
        provideRep $ return ("Page not found." :: Text)

    errorHandler (PermissionDenied msg) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPermissionDenied
            $(widgetFile "error/permission-denied")
        provideRep $ return $ object ["message" .= ("Permission Denied. " <> msg)]
        provideRep $ return $ "Permission Denied. " <> msg

    errorHandler (InvalidArgs msgs) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgInvalidArguments
            idHeader <- newIdent
            $(widgetFile "error/invalid-args")
        provideRep $ return $ object ["message" .= msgs]
        provideRep $ return $ T.intercalate ", " msgs

    errorHandler x = defaultErrorHandler x

    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        msgr <- getMessageRender

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do

            addStylesheet $ StaticR css_m3_material_tokens_css_baseline_css
            addScript $ StaticR js_md3_min_js

            idDialogChatNotification <- newIdent
            idFigureSenderPhoto <- newIdent
            idImgSenderPhoto <- newIdent
            idFigcaptionSenderInfo <- newIdent
            idNotificationBody <- newIdent
            idButtonIgnoreNotification <- newIdent
            idButtonReplyNotification <- newIdent

            idDialogIncomingVideoCall <- newIdent
            idFigureVideoCallerPhoto <- newIdent
            idImgVideoCallerPhoto <- newIdent
            idFigcaptionVideoCallerPhoto <- newIdent
            idButtonVideoCallDecline <- newIdent
            idButtonVideoCallAccept <- newIdent

            idDialogIncomingAudioCall <- newIdent
            idFigureAudioCallerPhoto <- newIdent
            idImgAudioCallerPhoto <- newIdent
            idFigcaptionAudioCallerPhoto <- newIdent
            idButtonAudioCallDecline <- newIdent
            idButtonAudioCallAccept <- newIdent

            idDialogMissedCall <- newIdent
            idMissedCallCaller <- newIdent

            backlink <- fromMaybe HomeR <$> getCurrentRoute
            calleeName <- resolveName <$> maybeAuth

            $(widgetFile "default-layout")

        lang <- fromMaybe "en" . headMay <$> languages

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
      where
          resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized :: Route App -> Bool -> Handler AuthResult

    isAuthorized (VideoR _) _ = isAuthenticated

    isAuthorized (ChatR _) _ = isAuthenticated

    isAuthorized (PushSubscriptionsR sid _) _ = isAuthenticatedSelf sid
    isAuthorized (ContactRemoveR uid _ _) _ = isAuthenticatedSelf uid
    isAuthorized (ContactR uid _ _) _ = isAuthenticatedSelf uid
    isAuthorized (MyContactsR uid) _ = isAuthenticatedSelf uid
    isAuthorized (ContactsR uid) _ = isAuthenticatedSelf uid

    isAuthorized (AccountInfoEditR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountInfoR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountR uid) _ = isAuthenticatedSelf uid
    isAuthorized AccountsR _ = return Authorized
    isAuthorized (AccountEditR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountPhotoR _) _ = return Authorized

    isAuthorized (DataR TokensVapidClearR) _ = isAdmin
    isAuthorized (DataR TokensVapidR) _ = isAdmin
    isAuthorized (DataR TokensGoogleapisClearR) _ = isAdmin
    isAuthorized (DataR TokensGoogleapisHookR) _ = isAdmin
    isAuthorized r@(DataR TokensR) _ = setUltDest r >> isAdmin

    isAuthorized (DataR (UserPhotoR _)) _ = isAdmin
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized r@(DataR UsersR) _ = setUltDest r >> isAdmin

    -- Routes not requiring authentication.
    isAuthorized HomeR _ = return Authorized
    isAuthorized DocsR _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized

    isAuthorized WebAppManifestR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


isAuthenticatedSelf :: UserId -> Handler AuthResult
isAuthenticatedSelf uid = do
    muid <- maybeAuthId
    case muid of
        Just uid' | uid == uid' -> return Authorized
                  | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
        Nothing -> unauthorizedI MsgLoginPlease


isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ _ _ True _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ _ _ _ True)) -> return Authorized
        Just (Entity _ (User _ _ _ _ _ _ _ False)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgLoginPlease


-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool


instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR

    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR

    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    loginHandler :: AuthHandler App Html
    loginHandler = do
        app <- getYesod
        tp <- getRouteToParent
        rndr <- getUrlRender
        ult <- fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
        authLayout $ do
            setTitleI LoginTitle
            $(widgetFile "auth/login")


    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout w = liftHandler $ do
        defaultLayout $ do
            setTitleI MsgSignIn
            $(widgetFile "auth/layout")

    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate (Creds plugin ident extra) = liftHandler $ case plugin of
      "google" -> do
          let atoken :: Maybe Text
              atoken = extra ^? folded . filtered ((== "accessToken") . fst) . _2
          let name :: Maybe Text
              name = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "name" . _String
          let subject :: Maybe Text
              subject = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "sub" . _String
          let picture :: Maybe Text
              picture = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "picture" . _String
          let email :: Maybe Text
              email = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "email" . _String

          case (atoken,email) of
              (Just at,Just em) -> do
                  Entity uid _ <- runDB $ upsert User { userEmail = em
                                                      , userAuthType = UserAuthTypeGoogle
                                                      , userPassword = Nothing
                                                      , userVerkey = Nothing
                                                      , userVerified = True
                                                      , userName = name
                                                      , userSuperuser = False
                                                      , userAdmin = False
                                                      }
                                  [UserEmail =. em]
                  _ <- runDB $ upsert UserCred { userCredUser = uid
                                               , userCredName = "google_access_token"
                                               , userCredIdent = subject
                                               , userCredVal = at
                                               }
                       [UserCredVal =. at]

                  case picture of
                    Just src -> do
                        r <- liftIO $ W.get (unpack src)
                        case (r ^? W.responseHeader "Content-Type" . to decodeUtf8, BSL.toStrict <$> r ^? W.responseBody) of
                            (Just mime, Just bs) -> do
                                liftIO $ print mime
                                liftIO $ print bs
                                _ <- runDB $ upsert UserPhoto { userPhotoUser = uid
                                                              , userPhotoMime = mime
                                                              , userPhotoPhoto = bs
                                                              , userPhotoAttribution = Nothing
                                                              }
                                     [UserPhotoMime =. mime, UserPhotoPhoto =. bs]
                                return ()
                            _otherwise -> return ()
                        return ()
                    Nothing -> return ()
                  setUltDest $ MyContactsR uid
                  return $ Authenticated uid
              _otherwise -> return $ UserError InvalidLogin

      _ -> do
          user <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail E.==. val ident
              return x
          case user of
            Just (Entity uid _) -> do
                setUltDest $ MyContactsR uid
                return $ Authenticated uid
            Nothing -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [ oauth2GoogleScopedWidget $(widgetFile "auth/google") ["email","openid","profile"]
                        (googleApiConfClientId . appGoogleApiConf . appSettings $ app)
                        (googleApiConfClientSecret . appGoogleApiConf . appSettings $ app)
                      , authEmail
                      ]


instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    forgotPasswordHandler :: AuthHandler App Html
    forgotPasswordHandler = do
        (fw,et) <- liftHandler $ generateFormPost formForgotPassword
        parent <- getRouteToParent
        msgs <- getMessages
        authLayout $ do
            setTitleI PasswordResetTitle
            idFormForgotPassword <- newIdent
            $(widgetFile "auth/forgot")
      where
          formForgotPassword :: Form Text
          formForgotPassword extra = do
              rndr <- getMessageRender
              (r,v) <- mreq md3emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "forgotPassword", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("label", rndr MsgEmailAddress)]
                  } Nothing
              return (r,[whamlet|#{extra}^{fvInput v}|])


    setPasswordHandler :: Bool -> AuthHandler App TypedContent
    setPasswordHandler old = do
        parent <- getRouteToParent
        msgs <- getMessages
        selectRep $ provideRep $ authLayout $ do
            setTitleI SetPassTitle
            idFormSetPassWrapper <- newIdent
            idFormSetPass <- newIdent
            (fw,et) <- liftHandler $ generateFormPost formSetPassword
            $(widgetFile "auth/password")
      where
          formSetPassword :: Form (Text,Text,Text)
          formSetPassword extra = do
              rndr <- getMessageRender
              (currR,currV) <- mreq md3passwordField FieldSettings
                  { fsLabel = SomeMessage CurrentPassword
                  , fsTooltip = Nothing
                  , fsId = Just "currentPassword"
                  , fsName = Just "current"
                  , fsAttrs = [("label", rndr CurrentPassword)]
                  } Nothing
              (newR,newV) <- mreq md3passwordField FieldSettings
                  { fsLabel = SomeMessage NewPass
                  , fsTooltip = Nothing
                  , fsId = Just "newPassword"
                  , fsName = Just "new"
                  , fsAttrs = [("label", rndr NewPass)]
                  } Nothing
              (confR,confV) <- mreq md3passwordField FieldSettings
                  { fsLabel = SomeMessage ConfirmPass
                  , fsTooltip = Nothing
                  , fsId = Just "confirmPassword"
                  , fsName = Just "confirm"
                  , fsAttrs = [("label", rndr ConfirmPass)]
                  } Nothing

              let r = (,,) <$> currR <*> newR <*> confR
              let w = do
                      toWidget [cassius|
                                       ##{fvId currV}, ##{fvId newV}, ##{fvId confV}
                                         align-self: stretch
                                       |]
                      [whamlet|
                              #{extra}
                              $if old
                                ^{fvInput currV}
                              ^{fvInput newV}
                              ^{fvInput confV}
                              |]
              return (r,w)


    confirmationEmailSentResponse :: AE.Email -> AuthHandler App TypedContent
    confirmationEmailSentResponse email = do
        parent <- getRouteToParent
        msgs <- getMessages
        selectRep $ provideRep $ authLayout $ do
            setTitleI ConfirmationEmailSentTitle
            $(widgetFile "auth/confirmation")

    registerHandler :: AuthHandler App Html
    registerHandler = do
        (fw,et) <- liftHandler $ generateFormPost formRegEmailForm
        parent <- getRouteToParent
        msgs <- getMessages
        authLayout $ do
            setTitleI RegisterLong
            formRegisterWrapper <- newIdent
            formRegister <- newIdent
            $(widgetFile "auth/register")
      where
          formRegEmailForm :: Form Text
          formRegEmailForm extra = do
              renderMsg <- getMessageRender
              (emailR,emailV) <- mreq md3emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "email", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("label", renderMsg MsgEmailAddress)]
                  } Nothing
              let w = [whamlet|
                          #{extra}
                          ^{fvInput emailV}
                      |]
              return (emailR,w)


    emailLoginHandler :: (Route Auth -> Route App) -> Widget
    emailLoginHandler parent = do

        (fw,et) <- liftHandler $ generateFormPost formEmailLogin
        idFormEmailLoginWarpper <- newIdent
        idFormEmailLogin <- newIdent
        msgs <- getMessages
        $(widgetFile "auth/email")
      where
          formEmailLogin :: Form (Text,Text)
          formEmailLogin extra = do
              msgRender <- liftHandler getMessageRender
              (emailR,emailV) <- mreq md3emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsTooltip = Nothing, fsId = Just "email", fsName = Just "email"
                  , fsAttrs = [("label", msgRender MsgEmailAddress)]
                  } Nothing
              (passR,passV) <- mreq md3passwordField FieldSettings
                  { fsLabel = SomeMessage MsgPassword
                  , fsTooltip = Nothing, fsId = Just "password", fsName = Just "password"
                  , fsAttrs = [("label", msgRender MsgPassword)]
                  } Nothing
              let r = (,) <$> emailR <*> passR
                  w = do

                      users <- liftHandler $ runDB ( select $ do
                          x <- from $ table @User
                          where_ $ x ^. UserAuthType `in_` valList [UserAuthTypeEmail,UserAuthTypePassword]
                          where_ $ not_ $ x ^. UserSuperuser
                          orderBy [asc (x ^. UserId)]
                          return x )

                      supers <- liftHandler $ runDB ( select $ do
                          x <- from $ table @User
                          where_ $ x ^. UserAuthType `in_` valList [UserAuthTypeEmail,UserAuthTypePassword]
                          where_ $ x ^. UserSuperuser
                          orderBy [asc (x ^. UserId)]
                          return x )

                      let accounts = users <> supers

                      toWidget [cassius|
                          ##{fvId emailV}, ##{fvId passV}
                            align-self: stretch
                      |]
                      [whamlet|
<span style="position:relative;align-self:flex-end">
  <md-text-button.body-small type=button #anchorDemoAccounts trailing-icon
    onclick="document.getElementById('menuDemoAccounts').open = !document.getElementById('menuDemoAccounts').open">
    _{MsgDemoUserAccounts}
    <md-icon slot=icon>arrow_drop_down
  <md-menu #menuDemoAccounts anchor=anchorDemoAccounts>
    $with n <- length accounts
      $forall (i,Entity uid (User email _ _ _ _ name super admin)) <- zip (irange 1) accounts
        $with pass <- maybe "" (TE.decodeUtf8 . localPart) (emailAddress $ TE.encodeUtf8 email)
          <md-menu-item onclick="document.getElementById('#{fvId emailV}').value = '#{email}';document.getElementById('#{fvId passV}').value = '#{pass}'">
            <md-icon slot=start>
              <img src=@{AccountPhotoR uid} loading=lazy alt=_{MsgPhoto} style="clip-path:circle(50%)">
            <div slot=headline>
              #{email}
            <div slot=supporting-text>
              $maybe name <- name
                #{name}
            <div slot=supporting-text style="text-transform:uppercase">
              $with roles <- snd <$> filter fst [(super,MsgSuperuser),(admin,MsgAdministrator)]
                $if not (null roles)
                  $forall role <- roles
                    _{role} #
          $if i /= n
            <md-divider role=separator tabindex=-1>

#{extra}

^{fvInput emailV}
^{fvInput passV}
                      |]
              return (r,w)
            where
                irange x = [x :: Int ..]


    afterPasswordRoute :: App -> Route App
    afterPasswordRoute _ = HomeR


    addUnverified :: AE.Email -> VerKey -> AuthHandler App (AuthEmailId App)
    addUnverified email vk = liftHandler $ runDB $ insert
        (User email UserAuthTypeEmail Nothing (Just vk) False Nothing False False)


    sendVerifyEmail :: AE.Email -> VerKey -> VerUrl -> AuthHandler App ()
    sendVerifyEmail email _ verurl = do

        renderMsg <- getMessageRender

        tokenInfo <- liftHandler $ runDB $ selectOne $ do
            x <- from $ table @Token
            where_ $ x ^. TokenApi E.==. val apiInfoGoogle
            return x

        secretExists <- liftIO $ doesFileExist secretVolumeGmail

        (rtoken,sender) <- case (tokenInfo,secretExists) of
          (Just (Entity tid (Token _ StoreTypeDatabase)),_) -> do
              refresh <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val gmailRefreshToken
                  return $ x ^. StoreVal )
              sender <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val gmailSender
                  return $ x ^. StoreVal )
              return (refresh,sender)

          (Just (Entity _ (Token _ StoreTypeSession)),_) -> do
                refresh <- lookupSession gmailRefreshToken
                sender <- lookupSession gmailSender
                return (refresh,sender)

          (Just (Entity tid (Token _ StoreTypeGoogleSecretManager)),True) -> do

              refresh <- liftIO $ readFile' secretVolumeGmail

              sender <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val gmailSender
                  return $ x ^. StoreVal )

              return (Just (pack refresh),sender)

          (_,True) -> do
              refresh <- liftIO $ readFile' secretVolumeGmail
              return (Just (pack refresh),Just "me")

          _otherwise -> return (Nothing,Nothing)

        atoken <- case rtoken of
          Just refresh -> do
              settings <- appSettings <$> getYesod

              r <- liftIO $ post "https://oauth2.googleapis.com/token"
                  [ "refresh_token" := refresh
                  , "client_id" := (googleApiConfClientId . appGoogleApiConf $ settings)
                  , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ settings)
                  , "grant_type" := ("refresh_token" :: Text)
                  ]

              return $ r ^? WL.responseBody . key "access_token" . _String
          Nothing -> return Nothing

        case (atoken,sender) of
          (Just at,Just sendby) -> do

              let mail = (emptyMail $ Address Nothing "noreply")
                      { mailTo = [Address Nothing email]
                      , mailHeaders = [("Subject", renderMsg MsgVerifyYourEmailAddress)]
                      , mailParts = [[textPart, htmlPart]]
                      }
                    where
                      textPart = Part
                          { partType = "text/plain; charset=utf-8"
                          , partEncoding = None
                          , partDisposition = DefaultDisposition
                          , partContent = PartContent $ TLE.encodeUtf8 [stext|
                              _{MsgConfirmEmailPlease}.

                              #{verurl}

                              _{MsgThankYou}.
                              |]
                          , partHeaders = []
                          }
                      htmlPart = Part
                          { partType = "text/html; charset=utf-8"
                          , partEncoding = None
                          , partDisposition = DefaultDisposition
                          , partContent = PartContent $ renderHtml [shamlet|
                              <p>
                                #{renderMsg MsgConfirmEmailPlease}.
                              <p>
                                <a href=#{verurl}>#{verurl}
                              <p>
                                #{renderMsg MsgThankYou}.
                              |]
                          , partHeaders = []
                          }

              raw <- liftIO $ TE.decodeUtf8 . toStrict . B64L.encode <$> renderMail' mail

              let opts = defaults & auth ?~ oauth2Bearer (TE.encodeUtf8 at)
              response <- liftIO $ tryAny $ postWith
                  opts (gmailApi $ unpack sendby) (object ["raw" .= raw])

              case response of
                Left e@(SomeException _) -> case fromException e of
                  Just (HttpExceptionRequest _ (StatusCodeException r' bs)) -> do
                      case r' L.^. WL.responseStatus . WL.statusCode of
                        401 -> do
                            liftIO $ print response
                        403 -> do
                            liftIO $ print response
                        _   -> do
                            liftIO $ print response
                  _other -> do
                      liftIO $ print response
                Right _ok -> return ()
          _otherwise -> do
              curr <- getCurrentRoute
              addMessageI statusError MsgGmailAccountNotSet
              redirect $ fromMaybe HomeR curr


    getVerifyKey :: AuthEmailId App -> AuthHandler App (Maybe VerKey)
    getVerifyKey = liftHandler . runDB . fmap (userVerkey =<<) . get

    setVerifyKey :: AuthEmailId App -> VerKey -> AuthHandler App ()
    setVerifyKey uid k = liftHandler $ runDB $ update uid [UserVerkey =. Just k]

    needOldPassword :: AuthId App -> AuthHandler App Bool
    needOldPassword _ = return False

    verifyAccount :: AuthEmailId App -> AuthHandler App (Maybe (AuthId App))
    verifyAccount uid = liftHandler $ runDB $ do
        mu <- get uid
        case mu of
          Nothing -> return Nothing
          Just _ -> do
              update uid [UserVerified =. True, UserVerkey =. Nothing]
              return $ Just uid

    getPassword :: AuthId App -> AuthHandler App (Maybe SaltedPass)
    getPassword = liftHandler . runDB . fmap (userPassword =<<) . get

    setPassword :: AuthId App -> SaltedPass -> AuthHandler App ()
    setPassword uid pass = liftHandler $ runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds :: Identifier -> AuthHandler App (Maybe (EmailCreds App))
    getEmailCreds email = liftHandler $ runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
          Nothing -> return Nothing
          Just (Entity uid u) -> return $ Just EmailCreds
              { emailCredsId = uid
              , emailCredsAuthId = Just uid
              , emailCredsStatus = isJust $ userPassword u
              , emailCredsVerkey = userVerkey u
              , emailCredsEmail = email
              }

    getEmail :: AuthEmailId App -> AuthHandler App (Maybe Yesod.Auth.Email.Email)
    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get


gmailApi :: String -> String
gmailApi = printf "https://gmail.googleapis.com/gmail/v1/users/%s/messages/send"


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage _ ("fr":_) = frenchFormMessage
    renderMessage _ ("ro":_) = romanianFormMessage
    renderMessage _ ("ru":_) = russianFormMessage
    renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
