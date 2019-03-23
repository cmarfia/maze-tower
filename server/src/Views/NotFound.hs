{-# LANGUAGE OverloadedStrings #-}
module Views.NotFound (view) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

view = do
  html $ do
    body $ do
      h1 "NotFound"
