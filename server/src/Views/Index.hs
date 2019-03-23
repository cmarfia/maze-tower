{-# LANGUAGE OverloadedStrings #-}
module Views.Index (view) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified  Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

view = do
  html $ do
    body $ do
      H.div ! A.id "maze-tower__game" $ ""
      script ! type_ "text/javascript" ! src "/index.js" $ ""
