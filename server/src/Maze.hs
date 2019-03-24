{-# LANGUAGE OverloadedStrings #-}
module Maze (Maze, generate) where

import System.Random
import Data.List.Index (deleteAt)
import qualified Data.Matrix as Matrix
import Data.Aeson 
import GHC.Exts
import Data.Text 
import Data.Maybe

data Point = Point Int Int
    deriving (Show)

instance ToJSON Point where
    toJSON (Point row col) =
        -- subtract 1 for going to a zero-based index in JavaScript
        object [ "row" .= (row - 1 :: Int), "col" .= (col - 1 :: Int)]

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

nullCell :: Cell        
nullCell = Cell Nothing Nothing Nothing Nothing

data Maze = Maze (Matrix.Matrix Cell) Point Point
    deriving (Show)

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
    randomR (0, max - 1) seed

startingPoint :: Int -> Int -> StdGen -> (Point, StdGen)
startingPoint rows cols s0 =
    let
        (row, s1) = randomR (1, rows) s0
        (col, s2) = randomR (1, cols)  s1
    in
    (Point row col, s2)

generate :: Int -> Int -> StdGen-> Maze
generate rows cols s0 = 
    let
        (start, s1) = startingPoint rows cols s0
        (matrix, _, s2) = generate' (Matrix.matrix rows cols $ const nullCell) [start] s1 
        deadEnds = getDeadEnds matrix
        (entrance, remainingDeadEnds, s3) = randomizeEntrance deadEnds s2
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
                current = linkCells (Matrix.getElem row col matrix) (row, col) (nRow, nCol)
                next = linkCells (Matrix.getElem nRow nCol matrix) (nRow, nCol) (row, col)
                newMatrix = Matrix.setElem current (row, col) $ Matrix.setElem next (nRow, nCol) matrix
            in
            generate' newMatrix (Point nRow nCol : Point row col : stack) s1

linkCells :: Cell -> (Int, Int) -> (Int, Int) -> Cell
linkCells (Cell up right down left) (row, col) (nRow, nCol)
    | row > nRow && col == nCol = Cell (Just $ Point nRow nCol) right down left
    | row < nRow && col == nCol = Cell up right (Just $ Point nRow nCol) left
    | row == nRow && col > nCol = Cell up right down (Just $ Point nRow nCol)
    | row == nRow && col < nCol = Cell up (Just $ Point nRow nCol) down left
    | otherwise = error "Could not link cells"

pick :: [Point] -> StdGen -> (Point, StdGen)
pick points s0 =
    let (i, s1) = randomR (0, Prelude.length points - 1) s0
    in (points !! i, s1)

posOfUnvisitedCell :: Int -> Int -> Maybe Cell -> Maybe Point
posOfUnvisitedCell row col (Just (Cell Nothing Nothing Nothing Nothing)) = Just $ Point row col
posOfUnvisitedCell _ _ _ = Nothing

unvisitedNeighbors :: Matrix.Matrix Cell -> Int -> Int -> [Point]
unvisitedNeighbors matrix row col =
    catMaybes
        [ posOfUnvisitedCell row (col -1) $ Matrix.safeGet row (col - 1) matrix
        , posOfUnvisitedCell row (col + 1) $ Matrix.safeGet row (col + 1) matrix
        , posOfUnvisitedCell (row - 1) col $ Matrix.safeGet (row - 1) col matrix
        , posOfUnvisitedCell (row + 1) col $ Matrix.safeGet (row + 1) col matrix
        ]

getDeadEnds :: Matrix.Matrix Cell -> [Point]
getDeadEnds matrix =
    catMaybes $ Matrix.toList $ Matrix.mapPos getPosOfDeadEnd matrix

getPosOfDeadEnd :: (Int, Int) -> Cell -> Maybe Point
getPosOfDeadEnd (row, col) cell
    | isDeadEnd cell = Just $ Point row col
    | otherwise = Nothing

isDeadEnd :: Cell -> Bool
isDeadEnd (Cell (Just _) Nothing Nothing Nothing) = True
isDeadEnd (Cell Nothing (Just _) Nothing Nothing) = True
isDeadEnd (Cell Nothing Nothing (Just _) Nothing) = True
isDeadEnd (Cell Nothing Nothing Nothing (Just _)) = True
isDeadEnd (Cell _ _ _ _) = False

randomizeEntrance :: [Point] -> StdGen -> (Point, [Point], StdGen)
randomizeEntrance points s0 =
    let (i, s1) = randomR (0, Prelude.length points - 1) s0
    in (points !! i, deleteAt i points, s1)
