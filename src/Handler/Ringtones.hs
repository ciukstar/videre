{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Ringtones
  ( getRingtonesR
  , getRingtoneNewR
  ) where

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc
    , (^.)
    )
import Database.Persist (Entity (Entity), entityVal)

import Foundation
    ( Handler, Form
    , Route (DataR)
    , DataR (RingtoneNewR, RingtonesR)
    , AppMessage
      ( MsgRingtones, MsgNoRingtonesYet, MsgAdd, MsgRingtone, MsgBack
      , MsgSave, MsgCancel, MsgTheName, MsgDefault, MsgAudio, MsgSelectRingtone
      , MsgNoFileChosen
      )
    )
    
import Material3 (md3mreq, md3textField, md3switchField)

import Model
    ( Ringtone (Ringtone, ringtoneName, ringtoneDefault)
    , EntityField (RingtoneId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Widgets (widgetBanner, widgetMenu, widgetSnackbar, widgetUser)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, newIdent, FileInfo
    , SomeMessage (SomeMessage), getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form
    ( FieldView (fvInput, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (fileField)
import Yesod.Form.Functions (generateFormPost, mreq)
import Yesod.Persist.Core (YesodPersist(runDB))


getRingtoneNewR :: Handler Html
getRingtoneNewR = do
    
    (fw,et) <- generateFormPost $ formRingtone Nothing

    msgs <- getMessages    
    defaultLayout $ do
        setTitleI MsgRingtone
        $(widgetFile "data/ringtones/new")
    

data RingtoneForm = RingtoneForm Text Bool FileInfo


formRingtone :: Maybe (Entity Ringtone) -> Form RingtoneForm
formRingtone ringtone extra = do

    msgr <- getMessageRender
    
    (nameR, nameV) <- md3mreq md3textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgTheName)]
        } (ringtoneName . entityVal <$> ringtone)
        
    (defaultR, defaultV) <- md3mreq md3switchField FieldSettings
        { fsLabel = SomeMessage MsgDefault
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("label", msgr MsgDefault)]
        } (ringtoneDefault . entityVal <$> ringtone)
        
    (audioR, audioV) <- mreq fileField FieldSettings
        { fsLabel = SomeMessage MsgAudio
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
    
    let r = RingtoneForm <$> nameR <*> defaultR <*> audioR
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
