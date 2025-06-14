{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Foundation where

import ChatRoom.Data (ChatRoom)

import Control.Lens (folded, filtered, (^?), _2, to, (?~))
import qualified Control.Lens as L ((^.))
import Control.Monad.Logger (LogSource)

import Data.Aeson ((.:?))
import qualified Data.Aeson as A (Value (Bool))
import Data.Aeson.Lens ( key, AsValue(_String) )
import qualified Data.Aeson.Text as A (encodeToLazyText)
import Data.Aeson.Types (Parser, withObject)
import qualified Data.ByteString.Base64.Lazy as B64L (encode)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Kind (Type)
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import qualified Data.Map as M (alter)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)

import Import.NoFoundation

import Database.Esqueleto.Experimental as E
    ( select, selectOne, from, table, val, where_, unValue, asc
    , (==.), (^.), (:&) ((:&))
    , orderBy, valList, in_, not_, innerJoin, on
    )
import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Material3 (md3widget)

import Network.Mail.Mime
    ( Part (partDisposition, partEncoding, partType, partContent, partHeaders, Part)
    , PartContent (PartContent), Disposition (DefaultDisposition)
    , Mail (mailParts, mailHeaders, mailTo), renderMail', Encoding (None)
    , Address (Address), emptyMail
    )
import Network.Wai.EventSource (ServerEvent (ServerEvent), eventSourceAppChan)
import qualified Network.Wreq as W (get, responseHeader, responseBody)
import Network.Wreq (defaults, auth, oauth2Bearer, postWith, post, FormParam ((:=)))
import qualified Network.Wreq.Lens as WL

import System.Directory (doesFileExist)
import System.IO (readFile')

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius (cassiusFile)
import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet ( hamletFile )
import Text.Jasmine ( minifym )
import Text.Julius (juliusFile, rawJS)
import Text.Printf (printf)
import Text.Shakespeare.Text (stext)
import Text.Read (readMaybe)

import VideoRoom.Data (VideoRoom)

import Web.WebPush
    ( VAPIDKeys, VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    , readVAPIDKeys, vapidPublicKeyBytes
    )

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
      , LoginTitle, NowLoggedIn
      )
    , defaultMessage, englishMessage, frenchMessage, russianMessage, romanianMessage
    )

import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Romanian (romanianFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Yesod.WebSockets (WebSocketsT, webSockets, sendTextData, sourceWS)



-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings           :: AppSettings
    , appStatic             :: Static -- ^ Settings for static file serving.
    , appConnPool           :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager        :: Manager
    , appLogger             :: Logger
    , getChatRoom           :: ChatRoom
    , getVideoRoom          :: VideoRoom
    , getServerEventChannel :: Chan ServerEvent
    , getOnlineChannel      :: TVar (TChan Text, Map UserId (Maybe UTCTime))
    }

mkMessage "App" "messages" "en"


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")


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
            
            idOverlayDialogChatNotification <- newIdent
            idDialogChatNotification <- newIdent
            idFigureSenderPhoto <- newIdent
            idImgSenderPhoto <- newIdent
            idFigcaptionSenderInfo <- newIdent
            idNotificationBody <- newIdent
            idAudioIncomingChatRingtone <- newIdent
            idButtonChatNotificationIgnore <- newIdent
            idButtonReplyNotification <- newIdent

            idOverlayDialogIncomingVideoCall <- newIdent
            idDialogIncomingVideoCall <- newIdent
            idFigureVideoCallerPhoto <- newIdent
            idImgVideoCallerPhoto <- newIdent
            idFigcaptionVideoCallerPhoto <- newIdent
            idAudioIncomingVideoCallRingtone <- newIdent
            idButtonVideoCallDecline <- newIdent
            idButtonVideoCallAcceptAudio <- newIdent
            idButtonVideoCallAccept <- newIdent

            idOverlayDialogIncomingAudioCall <- newIdent
            idDialogIncomingAudioCall <- newIdent
            idFigureAudioCallerPhoto <- newIdent
            idImgAudioCallerPhoto <- newIdent
            idFigcaptionAudioCallerPhoto <- newIdent
            idAudioIncomingAudioCallRingtone <- newIdent
            idButtonAudioCallDecline <- newIdent
            idButtonAudioCallAcceptVideo <- newIdent
            idButtonAudioCallAccept <- newIdent

            idOverlayDialogMissedCall <- newIdent
            idDialogMissedCall <- newIdent
            idMissedCallCaller <- newIdent

            backlink <- fromMaybe HomeR <$> getCurrentRoute
            calleeName <- resolveName <$> maybeAuth

            mVAPIDKeys <- liftHandler getVAPIDKeys

            incomingChatRingtone <- do
                user <- maybeAuth
                case user of
                  Just (Entity uid _) -> liftHandler $ do
                      userIcomingCallRingtone <- runDB $ selectOne $ do
                          x :& t <- from $ table @Ringtone `E.innerJoin` table @UserRingtone
                              `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. UserRingtoneRingtone)
                          where_ $ t ^. UserRingtoneUser E.==. val uid
                          where_ $ t ^. UserRingtoneType E.==. val RingtoneTypeChatIncoming
                          return x
                      case userIcomingCallRingtone of
                        Just (Entity rid (Ringtone _ mime _)) -> return (UserRingtoneAudioR uid rid, mime)
                        Nothing -> do
                            defaultIcomingCallRingtone <- liftHandler $ runDB $ selectOne $ do
                                x :& t <- from $ table @Ringtone `E.innerJoin` table @DefaultRingtone
                                    `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. DefaultRingtoneRingtone)
                                where_ $ t ^. DefaultRingtoneType E.==. val RingtoneTypeChatIncoming
                                return x
                            case defaultIcomingCallRingtone of
                              Just (Entity rid (Ringtone _ mime _)) -> return (DefaultRingtoneAudioR rid, mime)
                              Nothing -> return (StaticR ringtones_incoming_message_ringtone_1_mp3, "audio/mpeg")
                            
                  Nothing -> do
                      defaultIcomingCallRingtone <- liftHandler $ runDB $ selectOne $ do
                          x :& t <- from $ table @Ringtone `E.innerJoin` table @DefaultRingtone
                              `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. DefaultRingtoneRingtone)
                          where_ $ t ^. DefaultRingtoneType E.==. val RingtoneTypeChatIncoming
                          return x
                      case defaultIcomingCallRingtone of
                        Just (Entity rid (Ringtone _ mime _)) -> return (DefaultRingtoneAudioR rid, mime)
                        Nothing -> return (StaticR ringtones_incoming_message_ringtone_1_mp3, "audio/mpeg")


            incomingCallRingtone <- do
                user <- maybeAuth
                case user of
                  Just (Entity uid _) -> liftHandler $ do
                      userIcomingCallRingtone <- runDB $ selectOne $ do
                          x :& t <- from $ table @Ringtone `E.innerJoin` table @UserRingtone
                              `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. UserRingtoneRingtone)
                          where_ $ t ^. UserRingtoneUser E.==. val uid
                          where_ $ t ^. UserRingtoneType E.==. val RingtoneTypeCallIncoming
                          return x
                      case userIcomingCallRingtone of
                        Just (Entity rid (Ringtone _ mime _)) -> return (UserRingtoneAudioR uid rid, mime)
                        Nothing -> do
                            defaultIcomingCallRingtone <- liftHandler $ runDB $ selectOne $ do
                                x :& t <- from $ table @Ringtone `E.innerJoin` table @DefaultRingtone
                                    `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. DefaultRingtoneRingtone)
                                where_ $ t ^. DefaultRingtoneType E.==. val RingtoneTypeCallIncoming
                                return x
                            case defaultIcomingCallRingtone of
                              Just (Entity rid (Ringtone _ mime _)) -> return (DefaultRingtoneAudioR rid, mime)
                              Nothing -> return (StaticR ringtones_incoming_call_samsung_ringtones_1_mp3, "audio/mpeg")
                            
                  Nothing -> do
                      defaultIcomingCallRingtone <- liftHandler $ runDB $ selectOne $ do
                          x :& t <- from $ table @Ringtone `E.innerJoin` table @DefaultRingtone
                              `E.on` (\(x :& t) -> x ^. RingtoneId E.==. t ^. DefaultRingtoneRingtone)
                          where_ $ t ^. DefaultRingtoneType E.==. val RingtoneTypeCallIncoming
                          return x
                      case defaultIcomingCallRingtone of
                        Just (Entity rid (Ringtone _ mime _)) -> return (DefaultRingtoneAudioR rid, mime)
                        Nothing -> return (StaticR ringtones_incoming_call_samsung_ringtones_1_mp3, "audio/mpeg")
            

            case mVAPIDKeys of
              Just vapidKeys -> do
                  user <- maybeAuth
                  let authenicated = A.Bool . isJust $ user
                  let applicationServerKey = vapidPublicKeyBytes vapidKeys
                  
                  $(widgetFile "default-layout")
                  
                  case user of
                    Just (Entity uid _) -> toWidget $(juliusFile "templates/channel.julius")
                    Nothing -> return ()
                  
              Nothing -> invalidArgsI [MsgNotGeneratedVAPID]

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


    isAuthorized (CalleesR uid) _ = isAuthenticatedSelf uid
    isAuthorized (CallsR uid) _ = isAuthenticatedSelf uid


    isAuthorized PushSubscriptionEndpointR _ = isAuthenticated

    isAuthorized (PushSubscriptionsDeleR _ _ pid) _ = isAuthenticatedSelf pid
    isAuthorized (PushSubscriptionsR sid _) _ = isAuthenticatedSelf sid
    isAuthorized (ContactRemoveR uid _ _) _ = isAuthenticatedSelf uid
    isAuthorized (ContactR uid _ _) _ = isAuthenticatedSelf uid
    isAuthorized (MyContactsR uid) _ = isAuthenticatedSelf uid
    isAuthorized (ContactsR uid) _ = isAuthenticatedSelf uid

    
    isAuthorized (DefaultRingtoneAudioR _) _ = return Authorized

    isAuthorized (AccountPreferencesR uid) _ = isAuthenticatedSelf uid
    isAuthorized (UserRingtoneAudioR uid _) _ = isAuthenticatedSelf uid
    isAuthorized (AccountNotificationsR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountRingtonesR uid _) _ = isAuthenticatedSelf uid
    isAuthorized (AccountSubscriptionDeleR uid _) _ = isAuthenticatedSelf uid
    isAuthorized (AccountSubscriptionR uid _) _ = isAuthenticatedSelf uid
    isAuthorized (AccountSubscriptionsR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountInfoEditR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountInfoR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountR uid) _ = isAuthenticatedSelf uid
    isAuthorized AccountsR _ = return Authorized
    isAuthorized (AccountEditR uid) _ = isAuthenticatedSelf uid
    isAuthorized (AccountPhotoR _) _ = return Authorized

    
    isAuthorized (DataR (UserSubscriptionDeleR _ _)) _ = isAdmin
    isAuthorized (DataR (UserSubscriptionR _ _)) _ = isAdmin
    isAuthorized (DataR (UserSubscriptionsR _)) _ = isAdmin
    isAuthorized r@(DataR SubscriptionsR) _ = setUltDest r >> isAdmin

    isAuthorized (DataR TokensVapidClearR) _ = isAdmin
    isAuthorized (DataR TokensVapidR) _ = isAdmin
    isAuthorized (DataR TokensGoogleapisClearR) _ = isAdmin
    isAuthorized (DataR TokensGoogleapisHookR) _ = isAdmin
    isAuthorized r@(DataR TokensR) _ = setUltDest r >> isAdmin


    isAuthorized (DataR (RingtoneSettingDeleR _)) _ = isAdmin
    isAuthorized (DataR (RingtoneSettingR _)) _ = isAdmin
    isAuthorized (DataR RingtoneSettingNewR) _ = isAdmin
    isAuthorized (DataR RingtoneSettingsR) _ = isAdmin
    
    isAuthorized (DataR (RingtoneAudioR _)) _ = isAdmin
    isAuthorized (DataR (RingtoneR _)) _ = isAdmin
    isAuthorized (DataR (RingtoneDeleR _)) _ = isAdmin
    isAuthorized (DataR (RingtoneEditR _)) _ = isAdmin
    isAuthorized (DataR RingtoneNewR) _ = isAdmin
    isAuthorized r@(DataR RingtonesR) _ = setUltDest r >> isAdmin
    
    isAuthorized (DataR (UserPhotoR _)) _ = isAdmin
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized r@(DataR UsersR) _ = setUltDest r >> isAdmin

    -- Routes not requiring authentication.
    isAuthorized r@HomeR _ = setUltDest r >> return Authorized
    isAuthorized DocsR _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized ServerEventListenerR _ = return Authorized
    isAuthorized (OnlineChannelR uid) _ = isAuthenticatedSelf uid
    

    isAuthorized ServiceWorkerR _ = return Authorized
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


getDefaultRingtoneAudioR :: RingtoneId -> Handler TypedContent
getDefaultRingtoneAudioR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId E.==. val rid
        return x
    
    return $ case ringtone of
      Just (Entity _ (Ringtone _ mime bs)) -> TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> TypedContent "audio/mpeg" $ toContent emptyContent


postServerEventListenerR :: Handler ()
postServerEventListenerR = do
    chan <- getServerEventChannel <$> getYesod
    writeChan chan $ ServerEvent Nothing Nothing $ return "Important Message"


getServerEventListenerR :: Handler ()
getServerEventListenerR = do
    chan <- getServerEventChannel <$> getYesod 
    sendWaiApplication $ eventSourceAppChan chan
        

getOnlineChannelR :: UserId -> Handler ()
getOnlineChannelR = webSockets . onlineApp


data UserMessage = UserMessage
    { umType     :: !WsMessageType
    , umUser     :: !UserId
    , umLastSeen :: !(Maybe UTCTime)
    } deriving (Show, Read)

instance FromJSON UserMessage where
    parseJSON :: Value -> Parser UserMessage
    parseJSON = withObject "UserMessage" $ \o -> do
        umType <- o .: "type"
        umUser <- o .: "user"
        umLastSeen <- o .:? "lastSeen"
        return UserMessage {..}


instance ToJSON UserMessage where
    toJSON :: UserMessage -> Value
    toJSON (UserMessage {..}) = object [ "type" .= umType
                                       , "user" .= umUser
                                       , "lastSeen" .= umLastSeen
                                       ]


onlineApp :: UserId -> WebSocketsT Handler ()
onlineApp uid = do
    tvar <- getOnlineChannel <$> getYesod
    (wChan, online) <- readTVarIO tvar
    rChan <- atomically $ do        
        writeTVar tvar (wChan, M.alter userJoinedChannel uid online)
        writeTChan wChan $ TL.toStrict $ A.encodeToLazyText $ UserMessage WsMessageTypeOnline uid Nothing
        dupTChan wChan

    (e :: Either SomeException ()) <- try $ race_
        (forever $ atomically (readTChan rChan) >>= sendTextData)
        (runConduit $ (sourceWS .|) $ mapM_C $ \json -> do
              atomically $ writeTChan wChan $ TL.toStrict json
        )
    case e of
      Left _ -> do
          
          now <- liftIO getCurrentTime
          tvar' <- getOnlineChannel <$> getYesod
          (wrChan, session) <- readTVarIO tvar
          let newOnline = M.alter (userLeftChannel now) uid session
          atomically $ writeTVar tvar' (wrChan, newOnline)
          atomically $ writeTChan wrChan $ TL.toStrict $ A.encodeToLazyText
              $ UserMessage WsMessageTypeOffline uid (Just now)
                  
      Right () -> return ()


userJoinedChannel :: Maybe (Maybe UTCTime) -> Maybe (Maybe UTCTime)
userJoinedChannel _ = Just Nothing


userLeftChannel :: UTCTime -> Maybe (Maybe UTCTime) -> Maybe (Maybe UTCTime)
userLeftChannel _ Nothing = Just Nothing
userLeftChannel lastSeen (Just _) = Just (Just lastSeen)


getServiceWorkerR :: Handler TypedContent
getServiceWorkerR = do

    rndr <- getUrlRenderParams
    msgr <- getMessageRender
    mVAPIDKeys <- getVAPIDKeys

    calleeName <- resolveName <$> maybeAuth

    case mVAPIDKeys of
      Just vapidKeys -> do
          let applicationServerKey = vapidPublicKeyBytes vapidKeys
          declineCall <- newIdent
          acceptCall <- newIdent
          ignoreChat <- newIdent
          replyChat <- newIdent
          return $ TypedContent typeJavascript $ toContent $ $(juliusFile "static/js/sw.julius") rndr
      Nothing -> invalidArgsI [MsgNotGeneratedVAPID]
  where
      resolveName = fromMaybe "" . ((\(Entity _ (User email _ _ _ _ name _ _)) -> name <|> Just email) =<<)


getVAPIDKeys :: Handler (Maybe VAPIDKeys)
getVAPIDKeys = do

    storeType <- (bimap unValue unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi E.==. val apiInfoVapid
        return (x ^. TokenId, x ^. TokenStore) )

    let readTriple (s,x,y) = VAPIDKeysMinDetails s x y

    details <- case storeType of
      Just (_, StoreTypeGoogleSecretManager) -> do
          liftIO $ (readTriple <$>) . readMaybe <$> readFile' secretVolumeVapid

      Just (tid, StoreTypeDatabase) -> do
          ((readTriple <$>) . readMaybe . unpack . unValue =<<) <$> runDB ( selectOne $ do
              x <-from $ table @Store
              where_ $ x ^. StoreToken E.==. val tid
              return $ x ^. StoreVal )

      Just (_,StoreTypeSession) -> return Nothing
      Nothing -> return Nothing

    return $ readVAPIDKeys <$> details


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


widgetSnackbar :: [(Text,Html)] -> Widget
widgetSnackbar msgs = $(widgetFile "widgets/snackbar")


widgetAccount :: Widget
widgetAccount = do
    user <- maybeAuth
    idMenu <- newIdent
    $(widgetFile "widgets/account") 


widgetMainMenu :: Text -> Text -> Widget
widgetMainMenu idOverlay idDialogMainMenu = do
    curr <- getCurrentRoute
    user <- maybeAuth
    idButtonMainMenuClose <- newIdent
    $(widgetFile "widgets/menu")


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

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("fr":_) = frenchMessage
    renderAuthMessage _ ("ro":_) = romanianMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs

    onLogin :: (MonadHandler m, HandlerSite m ~ App) => m ()
    onLogin = liftHandler $ do
        chan <- getServerEventChannel <$> getYesod
        uid <- maybeAuthId

        case uid of
          Just _uid' -> writeChan chan $ ServerEvent Nothing Nothing $ return "Logged IN"
          Nothing   -> return ()
        
        addMessageI msgSuccess NowLoggedIn


    onLogout :: (MonadHandler m, HandlerSite m ~ App) => m ()
    onLogout = liftHandler $ do
        chan <- getServerEventChannel <$> getYesod
        uid <- maybeAuthId
        setUltDest HomeR
        case uid of
          Just _uid' -> writeChan chan $ ServerEvent Nothing Nothing $ return "Logged OUT"
          Nothing -> return ()
        


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
              (r,v) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "forgotPassword", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("autocomplete", "email")]
                  } Nothing
              return (r,[whamlet|#{extra} ^{md3widget v}|])


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
              (currR,currV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage CurrentPassword
                  , fsTooltip = Nothing
                  , fsId = Just "currentPassword"
                  , fsName = Just "current"
                  , fsAttrs = [("autocomplete", "off")]
                  } Nothing
              (newR,newV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage NewPass
                  , fsTooltip = Nothing
                  , fsId = Just "newPassword"
                  , fsName = Just "new"
                  , fsAttrs = [("autocomplete", "off")]
                  } Nothing
              (confR,confV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage ConfirmPass
                  , fsTooltip = Nothing
                  , fsId = Just "confirmPassword"
                  , fsName = Just "confirm"
                  , fsAttrs = [("autocomplete", "off")]
                  } Nothing

              let r = (,,) <$> currR <*> newR <*> confR
              let w = [whamlet|
                              #{extra}
                              $if old
                                ^{md3widget currV}
                              ^{md3widget newV}
                              ^{md3widget confV}
                              |]
              return (r,w)


    confirmationEmailSentResponse :: AE.Email -> AuthHandler App TypedContent
    confirmationEmailSentResponse email = do
        parent <- getRouteToParent
        msgs <- getMessages
        selectRep $ provideRep $ authLayout $ do
            setTitleI ConfirmationEmailSentTitle
            toWidget $(cassiusFile "templates/widgets/snackbar.cassius")
            toWidget $(juliusFile "templates/widgets/snackbar.julius")
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
              (emailR,emailV) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "email", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("autocomplete","email")]
                  } Nothing
              let w = [whamlet|
                          #{extra}
                          ^{md3widget emailV}
                      |]
              return (emailR,w)


    emailLoginHandler :: (Route Auth -> Route App) -> Widget
    emailLoginHandler parent = do

        (fw,et) <- liftHandler $ generateFormPost formEmailLogin
        msgs <- getMessages

        idFormEmailLoginWarpper <- newIdent
        idFormEmailLogin <- newIdent
        $(widgetFile "auth/email")

      where
          formEmailLogin :: Form (Text,Text)
          formEmailLogin extra = do
              (emailR,emailV) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsTooltip = Nothing, fsId = Just "email", fsName = Just "email"
                  , fsAttrs = [("autocomplete", "email")]
                  } Nothing
                  
              (passR,passV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage MsgPassword
                  , fsTooltip = Nothing, fsId = Just "password", fsName = Just "password"
                  , fsAttrs = [("autocomplete", "off")]
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

                      idButtonDemoAccounts <- newIdent
                      idMenuDemoAccounts <- newIdent
                      
                      toWidget [julius|
                          Array.from(
                            document.getElementById(#{idMenuDemoAccounts}).querySelectorAll('li[data-email][data-pwd]')
                          ).forEach(x => {
                            x.addEventListener('click', e => {
                              document.getElementById(#{fvId emailV}).value = x.dataset.email;
                              document.getElementById(#{fvId passV}).value = x.dataset.pwd;
                              document.getElementById(#{idMenuDemoAccounts}).classList.remove('active');
                            });
                          });
                      |]
                      [whamlet|
                          <button.border.transparent type=button ##{idButtonDemoAccounts} data-ui=##{idMenuDemoAccounts}>
                            <span>_{MsgDemoUserAccounts}
                            <i>arrow_drop_down
                            <menu.border.no-wrap ##{idMenuDemoAccounts}>
                              $forall Entity uid (User email _ _ _ _ name super admin) <- accounts
                                $with pass <- maybe "" (TE.decodeUtf8 . localPart) (emailAddress $ TE.encodeUtf8 email)
                                  <li data-email=#{email} data-pwd=#{pass} data-ui=##{idMenuDemoAccounts}>
                                    <i.circle>
                                      <img src=@{AccountPhotoR uid} loading=lazy alt=_{MsgPhoto}>

                                    <div.max>
                                      <h6.small>
                                        #{email}
                                      <div.large-line>
                                        $maybe name <- name
                                          #{name}
                                      <div.upper>
                                        $with roles <- snd <$> filter fst [(super,MsgSuperuser),(admin,MsgAdministrator)]
                                          $if not (null roles)
                                            $forall role <- roles
                                              _{role} #

                          #{extra}

                          ^{md3widget emailV}
                          ^{md3widget passV}
                      |]
              return (r,w)


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
