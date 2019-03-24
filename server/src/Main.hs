{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index 
import qualified Maze

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "assets")

    get "/" $ do
        html $ renderHtml $ Views.Index.view

    get "/mazes/:rows/:columns" $ do
        rows <- param "rows"
        columns <- param "columns"
        json $ Maze.generate rows columns

    notFound $ do
       redirect "/"

