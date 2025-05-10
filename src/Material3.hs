{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Material3
  ( md3widget
  , md3widgetSelect
  , md3widgetSwitch
  , md3widgetFile
  , md3radioField
  ) where

import Data.Maybe (isJust)
import Text.Julius (julius)

import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core (newIdent, WidgetFor, ToWidget (toWidget))
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( OptionList (olOptions), radioField'
    , Option (optionExternalValue, optionDisplay, optionInternalValue)
    , FormMessage
    )
import Yesod.Form.Types
    ( Field (fieldView)
    , FieldView (fvErrors, fvInput, fvId, fvLabel, fvRequired)
    )


md3widgetSelect :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widgetSelect v = [whamlet|
  <div.field.label.suffix.border.round :isJust (fvErrors v):.invalid>
    ^{fvInput v}
    <label for=#{fvId v}>
      #{fvLabel v}
      $if fvRequired v
        <sup>*
    <i>arrow_drop_down
    $maybe err <- fvErrors v
      <span.error>#{err}
|]

    
md3widgetSwitch :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widgetSwitch v = [whamlet|
  <div.field.no-margin.middle-align.small :isJust (fvErrors v):.invalid>
    <nav.no-padding>          
      <label.switch>
        ^{fvInput v}
        <span style="padding-left:1rem">
          #{fvLabel v}

      $maybe err <- fvErrors v
        <span.error>#{err}
|]


md3widgetFile :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widgetFile v = do
    idButtonUploadLabel <- newIdent
    toWidget [julius|
        document.getElementById(#{fvId v}).addEventListener('change', (e) => {
          const file = e.target.files[0];
          const label = document.getElementById(#{idButtonUploadLabel});
          if (file) {
            label.textContent = file.name;
          }
        }, false);
    |]
    [whamlet|
        <button.transparent.border>
          <i>upload_file
          <span ##{idButtonUploadLabel}>
            #{fvLabel v}
            
          ^{fvInput v}

        $maybe err <- fvErrors v
          <span.error-text>#{err}
    |]


md3widget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widget v = [whamlet|
  <div.field.label.border.round :isJust (fvErrors v):.invalid>

    ^{fvInput v}
    <label for=#{fvId v}>
      #{fvLabel v}
      $if fvRequired v
        <sup>*

    $maybe err <- fvErrors v
      <span.error>#{err}
|]


md3radioField :: (RenderMessage m FormMessage, Eq a) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3radioField options = (radioField' options)
    { fieldView = \theId name attrs x isReq -> do
          opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget options
          let sel (Left _) _ = False
              sel (Right y) opt = optionInternalValue opt == y
          [whamlet|
<div ##{theId} *{attrs}>
  $forall (i,opt) <- opts
    <label.radio for=#{theId}-#{i}>
      <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt} :sel x opt:checked>
      <span style="white-space:normal">
        #{optionDisplay opt}

|] }
