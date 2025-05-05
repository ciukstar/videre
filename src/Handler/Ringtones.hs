{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Ringtones
  ( getRingtonesR
  , getRingtoneNewR
  , postRingtonesR
  , getRingtoneR
  , getRingtoneAudioR
  , getRingtoneEditR
  , postRingtoneR
  , postRingtoneDeleR
  , getRingtoneSettingsR
  , getRingtoneSettingNewR
  , postRingtoneSettingsR
  , getRingtoneSettingR
  , postRingtoneSettingDeleR
  ) where


import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, where_, val
    , (^.), (==.), (=.), (:&) ((:&))
    , update, set, Value (unValue), asc, innerJoin, on
    )
import Database.Persist
    ( Entity (Entity), entityVal
    , PersistStoreWrite (insert_, replace, delete)
    )    

import Foundation
    ( Handler, Form, widgetMainMenu, widgetSnackbar, widgetAccount
    , Route (DataR)
    , DataR
      ( RingtoneNewR, RingtonesR, RingtonesR, RingtoneR, RingtoneAudioR
      , RingtoneEditR, RingtoneDeleR, RingtoneSettingsR, RingtoneSettingNewR
      , RingtoneSettingR, RingtoneSettingDeleR
      )
    , AppMessage
      ( MsgRingtones, MsgNoRingtonesYet, MsgAdd, MsgRingtone, MsgBack
      , MsgSave, MsgCancel, MsgTheName, MsgAudio, MsgSelectRingtone
      , MsgRecordAdded, MsgType, MsgRingtoneNotFound, MsgDele, MsgEdit
      , MsgSelectRingtoneFile, MsgRecordEdited, MsgRecordDeleted
      , MsgInvalidFormData, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgAlreadyExists, MsgDefaultSettings, MsgOutgoingCall
      , MsgIncomingCall, MsgOutgoingChatMessage, MsgIncomingChatMessage
      , MsgDefaultRingtone, MsgNoDefaultRingtonesSetYet
      )
    )
    
import Material3 (md3mreq, md3textField, md3selectField, md3widget, md3widgetFile, md3widgetSelect)

import Model
    ( statusSuccess, statusError, RingtoneId, Ringtone (Ringtone, ringtoneName)
    , DefaultRingtoneId, DefaultRingtone (DefaultRingtone)
    , RingtoneType
      ( RingtoneTypeCallOutgoing, RingtoneTypeCallIncoming
      , RingtoneTypeChatOutgoing, RingtoneTypeChatIncoming
      )
    , EntityField
      ( RingtoneId, RingtoneName, DefaultRingtoneRingtone, DefaultRingtoneType
      , DefaultRingtoneId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), getMessageRender, addMessageI, redirect
    , fileSourceByteString, TypedContent (TypedContent), invalidArgsI
    , ToContent (toContent), whamlet, MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( Field, FieldView (fvInput, fvId, fvErrors), checkM, optionsPairs
    , FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs), textField, selectField
    )
import Yesod.Form.Fields (fileField)
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, mopt)
import Yesod.Persist.Core (YesodPersist(runDB))


postRingtoneSettingDeleR :: DefaultRingtoneId -> Handler Html
postRingtoneSettingDeleR did = do

    ((fr,_),_) <- runFormPost formRingtoneSettingDelete

    case fr of
      FormSuccess () -> do
          runDB $ delete did
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR RingtoneSettingsR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR $ RingtoneSettingR did
    


getRingtoneSettingR :: DefaultRingtoneId -> Handler Html
getRingtoneSettingR did = do

    setting <- runDB $ selectOne $ do
        x :& r <- from $ table @DefaultRingtone
            `innerJoin` table @Ringtone `on` (\(x :& r) -> x ^. DefaultRingtoneRingtone ==. r ^. RingtoneId) 
        where_ $ x ^. DefaultRingtoneId ==. val did
        return (x,r)

    (fw0,et0) <- generateFormPost formRingtoneSettingDelete
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDefaultRingtone
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/ringtones/settings/setting")


formRingtoneSettingDelete :: Form ()
formRingtoneSettingDelete extra = return (pure (), [whamlet|#{extra}|])


postRingtoneSettingsR :: Handler Html
postRingtoneSettingsR = do

    ((fr,fw),et) <- runFormPost $ formRingtoneDefault Nothing

    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR RingtoneSettingsR
      _otherwise -> do
          msgs <- getMessages  
          defaultLayout $ do
              setTitleI MsgDefaultSettings
              idFormRingtoneDefault <- newIdent
              $(widgetFile "data/ringtones/settings/new")


getRingtoneSettingNewR :: Handler Html
getRingtoneSettingNewR = do

    (fw,et) <- generateFormPost $ formRingtoneDefault Nothing
    
    msgs <- getMessages  
    defaultLayout $ do
        setTitleI MsgDefaultSettings
        idFormRingtoneDefault <- newIdent
        $(widgetFile "data/ringtones/settings/new")


formRingtoneDefault :: Maybe (Entity DefaultRingtone) -> Form DefaultRingtone
formRingtoneDefault setting extra = do

    msgr <- getMessageRender
    
    ringtones <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Ringtone
        orderBy [asc (x ^. RingtoneName)]
        return (x ^. RingtoneName, x ^. RingtoneId) )
        
    (ringtoneR,ringtoneV) <- mreq (selectField (optionsPairs ringtones)) FieldSettings
        { fsLabel = SomeMessage MsgRingtone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } Nothing

    (typeR,typeV) <- mreq (uniqueTypeSelectField types) FieldSettings
        { fsLabel = SomeMessage MsgType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label",msgr MsgType)]
        } Nothing

    return ( DefaultRingtone <$> ringtoneR <*> typeR
           , [whamlet|#{extra} ^{md3widgetSelect ringtoneV} ^{md3widgetSelect typeV}|]
           )
  where
      types = [ (MsgOutgoingCall, RingtoneTypeCallOutgoing)
              , (MsgIncomingCall, RingtoneTypeCallIncoming)
              , (MsgOutgoingChatMessage, RingtoneTypeChatOutgoing)
              , (MsgIncomingChatMessage, RingtoneTypeChatIncoming)
              ]

      uniqueTypeSelectField :: [(AppMessage,RingtoneType)] -> Field Handler RingtoneType
      uniqueTypeSelectField xs = checkM uniqueType (selectField (optionsPairs xs))

      uniqueType :: RingtoneType -> Handler (Either AppMessage RingtoneType)
      uniqueType typ = do
          x <- runDB $ selectOne $ do
              x <- from $ table @DefaultRingtone
              where_ $ x ^. DefaultRingtoneType ==. val typ
              return x
          return $ case x of
            Nothing -> Right typ
            Just (Entity sid _) -> case setting of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _) | sid == sid' -> Right typ
                                   | otherwise -> Left MsgAlreadyExists
      

getRingtoneSettingsR :: Handler Html
getRingtoneSettingsR = do

    defaults <- runDB $ select $ do
        x :& r <- from $ table @DefaultRingtone
            `innerJoin` table @Ringtone `on` (\(x :& r) -> x ^. DefaultRingtoneRingtone ==. r ^. RingtoneId)
        return (x, r) 
    
    msgs <- getMessages  
    defaultLayout $ do
        setTitleI MsgDefaultSettings
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/ringtones/settings/settings")


postRingtoneDeleR :: RingtoneId -> Handler Html
postRingtoneDeleR rid = do
    
    ((fr,_),_) <- runFormPost formRingtoneDelete
    
    case fr of
      FormSuccess () -> do
          runDB $ delete rid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ DataR RingtonesR
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ DataR RingtonesR


formRingtoneDelete :: Form ()
formRingtoneDelete extra = return (pure (), [whamlet|#{extra}|]) 


postRingtoneR :: RingtoneId -> Handler Html
postRingtoneR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId ==. val rid
        return x
    
    ((fr,fw),et) <- runFormPost $ formRingtoneEdit ringtone

    case fr of
      FormSuccess (RingtoneEditForm name (Just fi)) -> do
          bs <- fileSourceByteString fi
          runDB $ replace rid $ Ringtone name (fileContentType fi) bs
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ RingtoneR rid
      FormSuccess (RingtoneEditForm name Nothing) -> do
          runDB $ update $ \x -> do
              set x [ RingtoneName =. val name]
              where_ $ x ^. RingtoneId ==. val rid
          addMessageI statusSuccess MsgRecordEdited
          redirect $ DataR $ RingtoneR rid
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRingtone
              $(widgetFile "data/ringtones/edit")


getRingtoneEditR :: RingtoneId -> Handler Html
getRingtoneEditR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId ==. val rid
        return x
    
    (fw,et) <- generateFormPost $ formRingtoneEdit ringtone

    msgs <- getMessages  
    defaultLayout $ do
        setTitleI MsgRingtone
        $(widgetFile "data/ringtones/edit")
    

data RingtoneEditForm = RingtoneEditForm !Text !(Maybe FileInfo)


formRingtoneEdit :: Maybe (Entity Ringtone) -> Form RingtoneEditForm
formRingtoneEdit ringtone extra = do
    
    (nameR, nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (ringtoneName . entityVal <$> ringtone)
        
    (audioR, audioV) <- mopt fileField FieldSettings
        { fsLabel = maybe (SomeMessage MsgSelectRingtone) (SomeMessage . ringtoneName . entityVal) ringtone
        , fsId = Nothing, fsName = Nothing, fsTooltip = Just (SomeMessage MsgAudio)
        , fsAttrs = []
        } Nothing

            
    return ( RingtoneEditForm <$> nameR <*> audioR
           , $(widgetFile "data/ringtones/form")
           )

  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Ringtone
              where_ $ x ^. RingtoneName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case ringtone of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getRingtoneR :: RingtoneId -> Handler Html
getRingtoneR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId ==. val rid
        return x

    (fw0,et0) <- generateFormPost formRingtoneDelete

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtone
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/ringtones/ringtone")


postRingtonesR :: Handler Html
postRingtonesR = do
    
    ((fr,fw),et) <- runFormPost $ formRingtone Nothing

    case fr of
      FormSuccess (RingtoneForm name fi) -> do
          bs <- fileSourceByteString fi
          runDB $ insert_ $ Ringtone name (fileContentType fi) bs
          addMessageI statusSuccess MsgRecordAdded
          redirect $ DataR RingtonesR
      _otherwise -> do
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRingtone
              idFormRingtone <- newIdent
              $(widgetFile "data/ringtones/new")


getRingtoneNewR :: Handler Html
getRingtoneNewR = do
    
    (fw,et) <- generateFormPost $ formRingtone Nothing

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtone
        idFormRingtone <- newIdent
        $(widgetFile "data/ringtones/new")
    

data RingtoneForm = RingtoneForm !Text !FileInfo


formRingtone :: Maybe (Entity Ringtone) -> Form RingtoneForm
formRingtone ringtone extra = do
    
    (nameR, nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (ringtoneName . entityVal <$> ringtone)
        
    (audioR, audioV) <- mreq fileField FieldSettings
        { fsLabel = maybe (SomeMessage MsgSelectRingtone) (SomeMessage . ringtoneName . entityVal) ringtone
        , fsId = Nothing, fsName = Nothing, fsTooltip = Just (SomeMessage MsgAudio)
        , fsAttrs = []
        } Nothing

    
    let r = RingtoneForm <$> nameR <*> audioR
    let w = $(widgetFile "data/ringtones/form") 
            
    return (r,w)

  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Ringtone
              where_ $ x ^. RingtoneName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case ringtone of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


getRingtonesR :: Handler Html
getRingtonesR = do
    
    ringtones <- runDB $ select $ do
        x <- from $ table @Ringtone
        orderBy [desc (x ^. RingtoneId)]
        return x

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtones
        idOverlay <- newIdent
        idDialogMainMenu <- newIdent
        $(widgetFile "data/ringtones/ringtones")


getRingtoneAudioR :: RingtoneId -> Handler TypedContent
getRingtoneAudioR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId ==. val rid
        return x
    
    case ringtone of
      Just (Entity _ (Ringtone _ mime bs)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> invalidArgsI [MsgRingtoneNotFound]
