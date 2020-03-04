module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import qualified ChessLogic as CL
import qualified AI as AI

window :: Display
window = InWindow "PKD Chess" (1080, 760) (10, 10)


{- blackOrWhite w b
DESCRIPTON: Translates the output from CL.White and CL.Black to the function color so a Picture can be made
RETURNS: A funktion that decides color of a piece -}
blackOrWhite :: CL.Color -> (Picture -> Picture)
blackOrWhite CL.White = color white
blackOrWhite CL.Black = color black

{- chessPieceToPicture p (x,y)
DESCRIPTION: Takes a chesspeice, its cordinates and draws it to a picture on its correct position
RETURNS: A picture with the drawn pieces
-}
chessPieceToPicture :: CL.ChessPiece -> (Float,Float) ->Picture
chessPieceToPicture (CL.King c)  (x,y)      = translate (squareSize * x) (squareSize * (7 - y)) $ (blackOrWhite c $ rectangleSolid 10 10)
chessPieceToPicture (CL.Queen c)   (x,y)    = translate (squareSize * x) (squareSize * (7 - y)) $  ( blackOrWhite c $ thickCircle 15 5)
chessPieceToPicture (CL.Knight c)  (x,y)    = translate (squareSize * x) (squareSize * (7 - y)) $ ( blackOrWhite c $ rotate 45 $ rectangleSolid 10 10)
chessPieceToPicture (CL.Bishop c)  (x,y)     = translate (squareSize * x) (squareSize * (7 - y)) $ ( blackOrWhite c $ rectangleSolid 15 15)
chessPieceToPicture (CL.Rook  c)  (x,y)      = translate (squareSize * x) (squareSize * (7 - y)) $ ( blackOrWhite c $ thickCircle 15 15)
chessPieceToPicture (CL.Pawn c ) (x,y)      = translate (squareSize * x) (squareSize * (7 - y)) $ ( blackOrWhite c $ thickCircle 15 10)
chessPieceToPicture CL.Void     _ = rectangleSolid 0 0

{--}
updateBoard :: CL.BoardState -> IO Picture
updateBoard b = return $ chessBoard b

updateBoardAux :: CL.BoardState -> Picture
updateBoardAux board = Pictures $ loopY 0 board
    wherkonstrukt
        loopY :: Float -> CL.BoardState -> [Picture]
        loopY 8 _ = []
        loopY y board = (loopX y 0 board)  ++ loopY (y+1) board

        loopX :: Float -> Float -> CL.BoardState -> [Picture]
        loopX _ 8 _ = []
        loopX y x board = chessPieceToPicture (CL.getPiece (round x,round y) board) (x,y) : loopX y (x + 1) board 


squares :: Picture
squares = Pictures [color red $ square (x *40) (y*40) | x <- [0..7], y <- [0..7], mod (round ( x + y)) 2 == 0]


square :: Float-> Float -> Picture
square x y = translate (x) (y) $ rectangleSolid 40 40

outline :: Picture
outline = translate 140 140 $ rectangleWire 320 320

squareSize :: Float
squareSize = 40

chessBoard :: CL.BoardState -> Picture
chessBoard b = translate (-4 * squareSize) (-4 * squareSize) $ Pictures [boardBack, squares, outlines, (updateBoardAux b)]

background :: Color
background = white

boardBack :: Picture
boardBack = translate 140 140 $ color yellow $ rectangleSolid (squareSize*8) (squareSize*8)

--playIO :: Display -> Color -> Int -> CL.BoardState -> (CL.BoardState -> IO Picture) -> (Event -> CL.BoardState -> IO CL.BoardState) -> (Float -> CL.BoardState -> IO CL.BoardState) -> IO ()

step :: Float -> CL.BoardState -> IO CL.BoardState
step _ b = return b

input :: Event -> CL.BoardState -> IO CL.BoardState
input (EventKey (MouseButton LeftButton) Down _ (_, _)) b = return $ CL.move (1,1) (3,3) b 
input _ b = return b

main :: IO ()
main =  playIO (window) (white) (5) (CL.createBoardState) (updateBoard) (input) (step)
--Commited?
--Nu då?