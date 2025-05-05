{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Material3
  ( md3emailField
  , md3passwordField
  , md3radioField
  , md3telField
  , md3textareaField
  , md3selectField
  , md3doubleField
  , md3dayField
  , md3timeField
  , md3datetimeLocalField
  , md3checkboxesField
  , md3checkboxesFieldList
  , tsep

  , md3widget
  , md3widgetSelect
  , md3widgetSwitch
  , md3widgetFile
  ) where


import Data.Foldable (find)
import qualified Data.List.Safe as LS (head, tail)
import Data.Maybe (isJust)
import Data.Text (Text, pack, splitOn)
import Text.Julius (julius)
import Data.Time.Calendar (Day)
import Data.Time (TimeOfDay, LocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core (newIdent, WidgetFor, ToWidget (toWidget))
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, handlerToWidget)
import Yesod.Form.Fields
    ( emailField, passwordField, textField, OptionList (olOptions), radioField'
    , Option (optionExternalValue, optionDisplay, optionInternalValue)
    , textareaField, Textarea (Textarea), selectField
    , FormMessage, doubleField, dayField, timeField, datetimeLocalField
    , optionsPairs, multiSelectField
    )
import Yesod.Form.Types
    ( Field (fieldView)
    , FieldView (fvErrors, fvInput, fvId, fvLabel, fvRequired)
    )


md3checkboxesFieldList :: (Eq a, RenderMessage m msg) => [(msg,a)] -> Field (HandlerFor m) [a]
md3checkboxesFieldList = md3checkboxesField . optionsPairs


md3checkboxesField :: Eq a => HandlerFor m (OptionList a) -> Field (HandlerFor m) [a]
md3checkboxesField ioptlist = (multiSelectField ioptlist)
    { fieldView = \theId name attrs val _idReq -> do
          opts <- olOptions <$> handlerToWidget ioptlist
          let optselected (Left _) _ = False
              optselected (Right vals) opt = optionInternalValue opt `elem` vals
          [whamlet|
            <span ##{theId}>
              $forall opt <- opts
                <label>
                  <md-checkbox touch-target=wrapper
                    name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                  #{optionDisplay opt}
          |]
    }


md3selectField :: (Eq a, RenderMessage m FormMessage) => HandlerFor m (OptionList a) -> Field (HandlerFor m) a
md3selectField options = (selectField options)
    { fieldView = \theId name attrs x req -> do
          opts <- olOptions <$> handlerToWidget options
              
          [whamlet|
<md-filled-select ##{theId} *{attrs} :req:required name=#{name} value=#{either id (get opts) x}>
  $forall opt <- opts
    $with parts <- splitOn tsep $ optionDisplay opt
      <md-select-option value=#{optionExternalValue opt} :sel x opt:selected>
        $maybe h <- LS.head parts
          <div slot=headline>#{h}
        $maybe ts <- LS.tail parts
          $forall t <- ts
            <div slot=supporting-text>#{t}
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }

  where    
    sel (Left _) _ = False
    sel (Right y) opt = optionInternalValue opt == y

    get :: (Eq a) => [Option a] -> a -> Text
    get os a = maybe "undefied" optionExternalValue $ find (\o -> optionInternalValue o == a) os          


md3telField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3telField = textField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=tel name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3passwordField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3passwordField = passwordField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=password name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3emailField :: RenderMessage m FormMessage => Field (HandlerFor m) Text
md3emailField = emailField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=email name=#{name} :req:required value=#{either id id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3dayField :: RenderMessage m FormMessage => Field (HandlerFor m) Day
md3dayField = dayField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=date name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
      <md-icon>today
|] }


md3timeField :: RenderMessage m FormMessage => Field (HandlerFor m) TimeOfDay
md3timeField = timeField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=time name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button slot=trailing-icon
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
        <md-icon>schedule
|] }


md3datetimeLocalField :: RenderMessage m FormMessage => Field (HandlerFor m) LocalTime
md3datetimeLocalField = datetimeLocalField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=datetime-local name=#{name} :req:required value=#{either id showVal ex} *{attrs}>
  <span slot=trailing-icon style="display:flex;flex-direction:row;align-items:center">
    $if elem "error" (fst <$> attrs)
      <md-icon>error
    <md-icon-button type=button slot=trailing-icon
      onclick="document.getElementById('#{theId}').shadowRoot.querySelector('input').showPicker()">
        <md-icon>schedule
|] }
  where
    showVal = pack . iso8601Show


md3textareaField :: RenderMessage m FormMessage => Field (HandlerFor m) Textarea
md3textareaField = textareaField { fieldView = \theId name attrs x req -> [whamlet|
<md-filled-text-field ##{theId} type=textarea name=#{name} :req:required value=#{either Textarea id x} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


md3doubleField :: RenderMessage m FormMessage => Field (HandlerFor m) Double
md3doubleField = doubleField { fieldView = \theId name attrs ex req -> [whamlet|
<md-filled-text-field ##{theId} type=number name=#{name} :req:required value=#{either id (pack . show) ex} *{attrs}>
  $if elem "error" (fst <$> attrs)
    <md-icon slot=trailing-icon>error
|] }


tsep :: Text
tsep = "<>"


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
