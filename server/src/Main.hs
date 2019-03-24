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
import Text.Blaze.Html.Renderer.Text
import qualified Views.Index 
import qualified Maze

-- setVault :: Request -> V.Key a -> a -> Request
-- setVault req key value = req { vault = V.insert key value (vault req)}

-- getVault :: Request -> V.Key a -> a
-- getVault req key = fromJust $ V.lookup key (vault req)

-- setRandomSeed :: V.Key StdGen -> Middleware
-- setRandomSeed key app req respond = do
--     seed <- newStdGen
--     let vault' = V.insert key seed (vault req)
--         req' = req { vault = vault' }
--     app req' respond

app :: ScottyM ()
app = do
    middleware $ staticPolicy (noDots >-> addBase "assets")

    get "/" $ do
        html $ renderHtml $ Views.Index.view

    get "/mazes/:rows/:columns" $ do
        -- Get Params
        rows <- param "rows"
        columns <- param "columns"
        
        -- Put random generator seed on the request
        -- seedKey <- liftIO V.newKey
        -- middleware $ setRandomSeed seedKey

        -- Get random generator seed
        seed <- liftIO newStdGen
        
        json $ Maze.generate rows columns seed

    notFound $ do
        redirect "/"

main :: IO ()
main = do
    scotty 3000 app

