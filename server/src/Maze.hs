{-# LANGUAGE OverloadedStrings #-}
module Maze (Maze, generate) where

import System.Random
import qualified Data.Matrix as Matrix
import Data.Aeson 
import GHC.Exts
import Data.Text 
import Data.Maybe

data Point = Point Int Int

instance ToJSON Point where
    toJSON (Point row col) =
        object [ "row" .= row, "col" .= col]

data Cell = Cell (Maybe Point) (Maybe Point) (Maybe Point) (Maybe Point)

instance ToJSON Cell where
    toJSON (Cell up right down left) =
        object [
            "up" .= up,
            "right" .= right,
            "down" .= down,
            "left" .= left
        ]

nullCell :: Cell        
nullCell = Cell Nothing Nothing Nothing Nothing

data Maze = Maze (Matrix.Matrix Cell) Point Point

instance ToJSON Maze where
    toJSON (Maze matrix entrance exit) =
        object [
            ("maze",  encodeMatrix matrix),
            "entrance" .= entrance,
            "exit" .= exit
        ]

encodeMatrix :: Matrix.Matrix Cell -> Value    
encodeMatrix matrix =
    Array $ fromList $ Prelude.map encodeRow $ Matrix.toLists matrix

encodeRow :: [Cell] -> Value    
encodeRow row =
    Array $ fromList $ Prelude.map toJSON row

randomZeroTo :: Int -> StdGen -> (Int, StdGen)
randomZeroTo max seed =
    randomR (0, max) seed

startingPoint :: Int -> Int -> StdGen -> (Point, StdGen)
startingPoint rows cols s0 =
    let
        (row, s1) = randomZeroTo rows s0
        (col, s2) = randomZeroTo cols s1
    in
    (Point row col, s2)

generate :: Int -> Int -> StdGen -> Maze
generate rows cols s0 = 
    let
        (start, s1) = startingPoint rows cols s0
        (matrix, _, s2) = generate' (Matrix.matrix rows cols $ const nullCell) [start] s1 
        entrance = Point 0 0
        exit = Point 1 0 
    in
    Maze matrix start exit


generate' :: Matrix.Matrix Cell -> [Point] -> StdGen -> (Matrix.Matrix Cell, [Point], StdGen)
generate' matrix [] s0 = (matrix, [], s0)
generate' matrix (Point row col : stack) s0 =
    case unvisitedNeighbors matrix row col of
        [] -> if Prelude.length stack == 0 then (matrix, [], s0) else generate' matrix stack s0
        neighbors ->
            let
                ((Point nRow nCol), s1) = pick neighbors s0
            in
            (matrix, [], s0)

pick :: [Point] -> StdGen -> (Point, StdGen)
pick points s0 =
    let (i, s1) = randomR (0, Prelude.length points - 1) s0
    in (points !! i, s1)

posOfUnvisitedCell :: Int -> Int -> Maybe Cell -> Maybe Point
posOfUnvisitedCell _ _ Nothing = Nothing
posOfUnvisitedCell _ _ (Just (Cell Nothing Nothing Nothing Nothing)) = Nothing
posOfUnvisitedCell row col (Just _) = Just $ Point row col


unvisitedNeighbors :: Matrix.Matrix Cell -> Int -> Int -> [Point]
unvisitedNeighbors matrix row col =
    catMaybes
        [ posOfUnvisitedCell row (col -1) $ Matrix.safeGet row (col - 1) matrix
        , posOfUnvisitedCell row (col + 1) $ Matrix.safeGet row (col + 1) matrix
        , posOfUnvisitedCell (row - 1) col $ Matrix.safeGet (row - 1) col matrix
        , posOfUnvisitedCell (row + 1) col $ Matrix.safeGet (row + 1) col matrix
        ]