{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Handler.Common
  ( getFaviconR, getRobotsR, getSitemapR, getWebAppManifestR
  ) where

import Control.Monad ( Monad(return) )

import Data.Aeson (object, (.=), Value (String))
import Data.Conduit (yield)
import Data.FileEmbed (embedFile)
import Data.Function (($))
import Data.Maybe (Maybe (Nothing, Just))

import Prelude ((*))

import Foundation
    ( Handler
    , Route (HomeR, StaticR, DocsR)
    , AppMessage
      ( MsgAppName, MsgMetaDescription, MsgTextChat, MsgContactList
      , MsgFeatures, MsgVideoCall
      )
    )
    
import Settings.StaticFiles
    ( img_video_chat_FILL0_wght400_GRAD0_opsz144_svg
    , img_screenshot_1_narrow_png, img_screenshot_1_wide_png
    , img_screenshot_2_narrow_png, img_screenshot_2_wide_png
    , img_screenshot_3_narrow_png, img_screenshot_3_wide_png
    , img_screenshot_4_narrow_png, img_screenshot_4_wide_png
    )

import Yesod.Core.Content
    ( TypedContent (TypedContent), typePlain, ToContent (toContent)
    )
import Yesod.Core.Handler
    ( cacheSeconds, selectRep, getUrlRender, getMessageRender
    )
import Yesod.Core.Json (array, provideJson)
import Yesod.Sitemap
    (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly)
    )


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    urlr <- getUrlRender
    msgr <- getMessageRender
    selectRep $ provideJson $ object
        [ "name" .= msgr MsgAppName
        , "short_name" .= msgr MsgAppName
        , "description" .= msgr MsgMetaDescription
        , "categories" .= array [String "social"]
        , "start_url" .= urlr HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array [ object [ "src" .= urlr (StaticR img_video_chat_FILL0_wght400_GRAD0_opsz144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "any"
                                    ]
                           , object [ "src" .= urlr (StaticR img_video_chat_FILL0_wght400_GRAD0_opsz144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "maskable"
                                    ]
                           ]
        , "screenshots" .= array [ object [ "src" .= urlr (StaticR img_screenshot_1_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgFeatures
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_1_wide_png)
                                          , "sizes" .= String "1918x926"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgContactList
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_2_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgContactList
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_2_wide_png)
                                          , "sizes" .= String "1918x926"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgContactList
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_3_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgVideoCall
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_3_wide_png)
                                          , "sizes" .= String "1918x926"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgVideoCall
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_4_narrow_png)
                                          , "sizes" .= String "440x783"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgTextChat
                                          ]
                                 , object [ "src" .= urlr (StaticR img_screenshot_4_wide_png)
                                          , "sizes" .= String "1918x926"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgTextChat
                                          ]
                                 ]
        ]


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl DocsR Nothing (Just Monthly) (Just 1.0)


getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
