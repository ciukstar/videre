{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Contacts
  ( getContactsR
  , postContactsR
  , getMyContactsR
  , getContactR
  , postContactRemoveR
  ) where

import ChatRoom (YesodChat (getBacklink, getAccountPhotoRoute, getContactRoute))
import ChatRoom.Data (Route (ChatRoomR))

import Control.Monad (join, forM_)

import Data.Bifunctor (Bifunctor(second, bimap))

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, leftJoin, on, just
    , (^.), (?.), (==.), (:&) ((:&))
    , Value (unValue), innerJoin, val, where_, selectOne
    )
import Database.Persist
    ( Entity (Entity), entityVal, entityKey, PersistStoreWrite (insert_, delete)
    )

import Foundation (Form)
import Foundation.Data
    ( Handler, App
    , Route
      ( AccountPhotoR, ChatR, ContactsR, MyContactsR, ContactR, ContactRemoveR)
    , AppMessage
      ( MsgNoContactsYet, MsgAppName, MsgContacts, MsgNoRegisteredUsersYet
      , MsgAdd, MsgInvalidFormData, MsgNewContactsAdded, MsgViewContact
      , MsgContact, MsgPhoto, MsgDele, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgCancel, MsgRecordDeleted
      )
    )

import Model
    ( statusError, statusSuccess
    , UserId, User (User, userName), UserPhoto
    , EntityField
      ( UserId, UserPhotoUser, UserPhotoAttribution, ContactOwner, ContactEntry
      , ContactId
      )
    , Contact (Contact), ContactId
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (RenderMessage)

import Widgets (widgetMenu, widgetUser)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages, handlerToWidget, addMessageI
    , addMessage, toHtml
    )
import Yesod.Core.Handler (setUltDestCurrent, newIdent, redirect)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( OptionList(olOptions), optionsPairs, multiSelectField
    , Option (optionInternalValue, optionExternalValue, optionDisplay)
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( Field (fieldView), FieldView (fvInput)
    , FormResult (FormSuccess, FormFailure, FormMissing)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postContactRemoveR :: UserId -> UserId -> ContactId -> Handler Html
postContactRemoveR uid rid cid = do
    ((fr,_),_) <- runFormPost formContactRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete cid
          addMessageI statusSuccess MsgRecordDeleted
          redirect $ MyContactsR uid
      _otherwise -> do
          addMessageI statusError MsgInvalidFormData
          redirect $ ContactR uid rid cid


getContactR :: UserId -> UserId -> ContactId -> Handler Html
getContactR uid rid cid = do
    
    contact <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& e :& h <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. ContactId ==. val cid
        return (e, h ?. UserPhotoAttribution) )

    (fw,et) <- generateFormPost formContactRemove
    
    defaultLayout $ do
        setTitleI MsgViewContact
        
        $(widgetFile "my/contacts/contact")


formContactRemove :: Form ()
formContactRemove extra = return (FormSuccess (),[whamlet|#{extra}|])


getMyContactsR :: UserId -> Handler Html
getMyContactsR uid = do
    
    entries <- (bimap unValue (second (join . unValue)) <$>) <$> runDB ( select $ do
        x :& e :& h <- from $ table @Contact
            `innerJoin` table @User `on` (\(x :& e) -> x ^. ContactEntry ==. e ^. UserId)
            `leftJoin` table @UserPhoto `on` (\(_ :& e :& h) -> just (e ^. UserId) ==. h ?. UserPhotoUser)
        where_ $ x ^. ContactOwner ==. val uid
        orderBy [desc (e ^. UserId)]
        return (x ^. ContactId, (e, h ?. UserPhotoAttribution)) )

    msgs <- getMessages
    
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAppName
        idFabAdd <- newIdent
        $(widgetFile "my/contacts/contacts")


postContactsR :: UserId -> Handler Html
postContactsR uid = do
    
    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x
        
    ((fr,fw),et) <- runFormPost $ formContacts users
    case fr of
      FormSuccess r -> do
          forM_ r $ \(Entity eid _) -> runDB $ insert_ (Contact uid eid)
          addMessageI statusSuccess MsgNewContactsAdded
          redirect $ MyContactsR uid
      FormFailure errs -> defaultLayout $ do
          setTitleI MsgContacts
          forM_ errs $ \err -> addMessage statusError (toHtml err)
          msgs <- getMessages
          idFabAdd <- newIdent
          idFormPostContacts <- newIdent
          $(widgetFile "contacts/contacts")
      FormMissing -> defaultLayout $ do
          setTitleI MsgContacts
          addMessageI statusError MsgInvalidFormData
          msgs <- getMessages
          idFabAdd <- newIdent
          idFormPostContacts <- newIdent
          $(widgetFile "contacts/contacts")


getContactsR :: UserId -> Handler Html
getContactsR uid = do
    
    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x
    
    (fw,et) <- generateFormPost $ formContacts users

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContacts
        idFabAdd <- newIdent
        idFormPostContacts <- newIdent
        $(widgetFile "contacts/contacts")


formContacts :: [Entity User] -> Form [Entity User]
formContacts options extra = do

    (usersR,usersV) <- mreq (usersFieldList (option <$> options)) "" Nothing

    return ( usersR
           , $(widgetFile "contacts/form")
           )
  where
      option e@(Entity _ (User email _ _ _ _ _ _ _)) = (email,e)

      usersFieldList :: RenderMessage App msg => [(msg, Entity User)] -> Field Handler [Entity User]
      usersFieldList = usersField . optionsPairs

      usersField :: Handler (OptionList (Entity User)) -> Field Handler [Entity User]
      usersField ioptlist = (multiSelectField ioptlist)
          { fieldView = \theId name attrs eval _idReq -> do
              opts <- olOptions <$> handlerToWidget ioptlist
              let optselected (Left _) _ = False
                  optselected (Right vals) opt = optionInternalValue opt `elem` vals
              [whamlet|
                <md-list ##{theId}>
                  $forall opt <- opts
                    <md-list-item type=button onclick="this.querySelector('md-checkbox').click()">
                      <img slot=start src=@{AccountPhotoR (entityKey $ optionInternalValue opt)}
                        width=56 height=56 loading=lazy style="clip-path:circle(50%)">

                      <div slot=headline>
                        $maybe name <- userName $ entityVal $ optionInternalValue opt
                          #{name}
                      <div slot=supporting-text>
                        #{optionDisplay opt}

                      <md-checkbox touch-target=wrapper slot=end onclick="event.stopPropagation()"
                        name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected eval opt:checked>
              |]
          }
    


instance YesodChat App where
    getBacklink :: UserId -> UserId -> Handler (Route App)
    getBacklink sid _ = return $ MyContactsR sid

    getAccountPhotoRoute :: UserId -> Handler (Route App)
    getAccountPhotoRoute uid = return $ AccountPhotoR uid
    
    getContactRoute :: UserId -> UserId -> ContactId -> Handler (Route App)
    getContactRoute uid rid cid = return $ ContactR uid rid cid
