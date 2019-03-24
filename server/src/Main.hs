{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vault.Lazy as V
import Control.Monad.Trans (liftIO)
import System.Random
import Web.Scotty
import Network.Wai
import Data.Maybe
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index 
import qualified Maze

main :: IO ()
main = do
    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "assets")

        get "/" $ do
            html $ renderHtml $ Views.Index.view

        get "/mazes/:rows/:columns" $ do
            rows <- param "rows"
            columns <- param "columns"
            seed <- liftIO newStdGen
            json $ Maze.generate rows columns seed

        notFound $ do
            redirect "/"

