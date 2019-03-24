{-# LANGUAGE OverloadedStrings #-}
module Maze (Maze, generate) where

import qualified Data.Matrix as Matrix
import Data.Aeson
import GHC.Exts
import Data.Text

data Point = Point Int Int
    deriving (Show)

instance ToJSON Point where
    toJSON (Point x y) =
        object [ "x" .= x, "y" .= y]

data Cell = Cell (Maybe Point) (Maybe Point) (Maybe Point) (Maybe Point)
    deriving (Show)

instance ToJSON Cell where
    toJSON (Cell top right bottom left) =
        object [
            "top" .= top,
            "right" .= right,
            "bottom" .= bottom,
            "left" .= left
        ]

data Maze = Maze (Matrix.Matrix Cell) Point Point
    deriving (Show)

instance ToJSON Maze where
     toJSON (Maze matrix entrance exit) =
        object [
            ("maze",  Array $ fromList $ Prelude.map encodeRow $ Matrix.toLists matrix),
            "entrance" .= entrance,
            "exit" .= exit
        ]

encodeRow row =
    Array $ fromList $ Prelude.map toJSON row

generate :: Int -> Int -> Maze
generate rows columns =
    let
        maze = Matrix.matrix rows columns $ const $ Cell Nothing Nothing Nothing $ Just $ Point 2 3
    in
    Maze maze (Point 0 0) (Point 2 2)

