{-# LANGUAGE OverloadedStrings #-}
module Maze (Maze, generate) where

import qualified Data.Matrix as Matrix
import Data.Aeson
import Data.Text

data Maze = Maze (Matrix.Matrix Int)
    deriving (Show)

instance ToJSON Maze where
     toJSON (Maze matrix) =
        String $ pack $ Matrix.prettyMatrix matrix

generate :: Int -> Int -> Maze
generate rows columns =
    Maze $ Matrix.zero rows columns

