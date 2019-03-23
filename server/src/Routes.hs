{-# LANGUAGE OverloadedStrings #-}
module Routes (routes) where

import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text
import qualified Views.NotFound
import qualified Views.Index
import qualified Maze

viewRoute = html . renderHtml

routes :: ScottyM ()
routes = do
    middleware $ staticPolicy (noDots >-> addBase "assets")

    get "/" $ do
        viewRoute Views.Index.view

    get "/mazes/:width/:height" $ do
        width <- param "width"
        height <- param "height"
        json $ Maze.generate height width

    notFound $ viewRoute Views.NotFound.view

