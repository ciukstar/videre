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
  ) where

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, where_, val
    , (^.), (==.)
    )
import Database.Persist
    ( Entity (Entity), entityVal, PersistStoreWrite (insert_))

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR (RingtoneNewR, RingtonesR, RingtonesR, RingtoneR, RingtoneAudioR)
    , AppMessage
      ( MsgRingtones, MsgNoRingtonesYet, MsgAdd, MsgRingtone, MsgBack
      , MsgSave, MsgCancel, MsgTheName, MsgAudio, MsgSelectRingtone
      , MsgNoFileChosen, MsgRecordAdded, MsgType, MsgRingtoneNotFound
      , MsgDele, MsgEdit
      )
    )
    
import Material3 (md3mreq, md3textField)

import Model
    ( statusSuccess, RingtoneId, Ringtone (Ringtone, ringtoneName)
    , EntityField (RingtoneId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetBanner, widgetMenu, widgetSnackbar, widgetUser)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), getMessageRender, addMessageI, redirect
    , fileSourceByteString, TypedContent (TypedContent), invalidArgsI
    , ToContent (toContent)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldView (fvInput, fvId)
    , FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (fileField)
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq)
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Text.Encoding (encodeUtf8)


getRingtoneR :: RingtoneId -> Handler Html
getRingtoneR rid = do
    
    ringtone <- runDB $ selectOne $ do
        x <- from $ table @Ringtone
        where_ $ x ^. RingtoneId ==. val rid
        return x

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtone
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
              $(widgetFile "data/ringtones/new")


getRingtoneNewR :: Handler Html
getRingtoneNewR = do
    
    (fw,et) <- generateFormPost $ formRingtone Nothing

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtone
        $(widgetFile "data/ringtones/new")
    

data RingtoneForm = RingtoneForm Text FileInfo


formRingtone :: Maybe (Entity Ringtone) -> Form RingtoneForm
formRingtone ringtone extra = do

    msgr <- getMessageRender
    
    (nameR, nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (ringtoneName . entityVal <$> ringtone)
        
    (audioR, audioV) <- mreq fileField FieldSettings
        { fsLabel = SomeMessage MsgAudio
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    let r = RingtoneForm <$> nameR <*> audioR
    let w = $(widgetFile "data/ringtones/form")
            
    return (r,w)


getRingtonesR :: Handler Html
getRingtonesR = do
    
    ringtones <- runDB $ select $ do
        x <- from $ table @Ringtone
        orderBy [desc (x ^. RingtoneId)]
        return x

    msgs <- getMessages

    idFabAdd <- newIdent
    
    defaultLayout $ do
        setTitleI MsgRingtones
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
