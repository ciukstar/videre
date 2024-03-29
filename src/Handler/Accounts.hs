{-# LANGUAGE TypeApplications #-}

module Handler.Accounts (getAccountPhotoR) where

import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, unValue
    , (^.), (==.)
    )
import Database.Persist (Entity(Entity))

import Foundation (Handler, Route (StaticR))

import Model
    ( UserId, UserPhoto (UserPhoto), User (User)
    , EntityField (UserPhotoUser, UserId, UserSuperuser)
    )

import Settings.StaticFiles
    ( img_shield_person_FILL0_wght400_GRAD0_opsz24_svg
    , img_person_FILL0_wght400_GRAD0_opsz24_svg
    )

import Yesod.Core.Content
    ( TypedContent (TypedContent), ToContent (toContent) )
import Yesod.Core.Handler (redirect)
import Yesod.Persist.Core (runDB)


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
