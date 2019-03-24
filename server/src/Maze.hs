{-# LANGUAGE OverloadedStrings #-}
module Maze (Maze, generate) where

import qualified Data.Matrix as Matrix
import Data.Aeson 
import GHC.Exts
import Data.Text 

data Point = Point Int Int
    deriving (Show)

instance ToJSON Point where
    toJSON (Point row col) =
        object [ "row" .= row, "col" .= col]

data Cell = Cell (Maybe Point) (Maybe Point) (Maybe Point) (Maybe Point)
    deriving (Show)

instance ToJSON Cell where
    toJSON (Cell up right down left) =
        object [
            "up" .= up,
            "right" .= right,
            "down" .= down,
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
        -- maze = Matrix.matrix rows columns $ const $ Cell Nothing Nothing Nothing $ Just $ Point 2 3
        maze = Matrix.fromLists
            [ [ Cell Nothing (Just $ Point 1 0) Nothing Nothing
              , Cell Nothing (Just $ Point 2 0) Nothing (Just $ Point 0 0)
              , Cell Nothing (Just $ Point 3 0) Nothing (Just $ Point 1 0)
              , Cell Nothing Nothing (Just $ Point 3 1) (Just $ Point 2 0)
              ]
            , [ Cell Nothing Nothing (Just $ Point 0 2) Nothing
              , Cell Nothing (Just $ Point 2 1) (Just $ Point 1 2) Nothing
              , Cell Nothing (Just $ Point 3 1) Nothing (Just $ Point 1 1)
              , Cell (Just $ Point 3 0) Nothing Nothing (Just $ Point 2 1)
              ]
            , [ Cell (Just $ Point 0 1) Nothing (Just $ Point 0 3) Nothing
              , Cell (Just $ Point 1 1) (Just $ Point 2 2) Nothing Nothing
              , Cell Nothing (Just $ Point 3 2) Nothing (Just $ Point 1 2)
              , Cell Nothing Nothing (Just $ Point 3 3) (Just $ Point 2 2)
              ]
            , [ Cell (Just $ Point 0 2) (Just $ Point 1 3) Nothing Nothing
              , Cell Nothing (Just $ Point 2 3) Nothing (Just $ Point 0 3)
              , Cell Nothing (Just $ Point 3 3) Nothing (Just $ Point 1 3)
              , Cell (Just $ Point 3 2) Nothing Nothing (Just $ Point 2 3)
              ]
            ]
        
        entrance = Point 0 0

        exit = Point 1 0 
    in
    Maze maze entrance exit

