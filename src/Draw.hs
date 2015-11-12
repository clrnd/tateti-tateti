{-# LANGUAGE LambdaCase #-}
module Draw where

import Control.Monad
import Data.Array
import Lens.Simple
import UI.NCurses

import Types
import Util


drawCross :: (Integer, Integer) -> Integer -> Update ()
drawCross (y, x) cellsize = do
    moveCursor (cellsize + y) x
    drawLineH (Just glyphLineH) (cellsize * 3 + 2)
    moveCursor (cellsize + y + cellsize + 1) x
    drawLineH (Just glyphLineH) (cellsize * 3 + 2)

    moveCursor y (cellsize + x)
    drawLineV (Just glyphLineV) (cellsize * 3 + 2)
    moveCursor y (cellsize + x + cellsize + 1)
    drawLineV (Just glyphLineV) (cellsize * 3 + 2)


drawCrosses :: Update ()
drawCrosses = do
    -- main cross
    drawCross (0, 0) 7

    -- top row crosses
    drawCross (1, 1) 1
    drawCross (1, 1 + 8) 1
    drawCross (1, 1 + 8 + 8) 1

    -- middle row crosses
    drawCross (1 + 8, 1) 1
    drawCross (1 + 8, 1 + 8) 1
    drawCross (1 + 8, 1 + 8 + 8) 1

    -- bottom row crosses
    drawCross (1 + 8 + 8, 1) 1
    drawCross (1 + 8 + 8, 1 + 8) 1
    drawCross (1 + 8 + 8, 1 + 8 + 8) 1


drawMessages :: GameState -> Colors -> Update ()
drawMessages gs colors = do

    setColor defaultColorID
    moveCursor 0 2
    clearLine
    drawString "Mode: "
    drawString (show $ gs ^. gMode)

    let player = gs ^. gPlayer
    moveCursor 1 2
    drawString "Player: "
    setColor $ colors Nothing (Just player)
    drawString (show player)


drawCursor :: GameState -> Update ()
drawCursor gs =
    let p = gs ^. gBoardState . bsPosition
        p' = gs ^. gBoardState . bsCells . ax p . bsPosition
    in
    uncurry moveCursor $ positionToCoordinates p p'


drawBoard :: GameState -> Colors -> Update ()
drawBoard gs colors = do
    let poss = range (Position T L, Position B R)
    forM_ poss $ \p -> do
        let poss' = range (Position T L, Position B R)
            m_winner = gs ^. gBoardState . bsCells . ax p . bsWinner
        forM_ poss' $ \p' -> do
            uncurry moveCursor $ positionToCoordinates p p'
            let m_p = gs ^. gBoardState . bsAx p . bsAx p'
            setColor $ colors m_winner m_p
            case m_p of
                Nothing -> drawString " "
                Just player -> drawString $ show player


positionToCoordinates :: Position -> Position -> (Integer, Integer)
positionToCoordinates outer_p inner_p =
    (getPos 8 outer_p) `plusTuple`
    (1, 1) `plusTuple`
    (getPos 2 inner_p)
  where
    getPos _ (Position T L) = (0, 0)
    getPos n (Position T C) = (0, n)
    getPos n (Position T R) = (0, n + n)
    getPos n (Position M L) = (n, 0)
    getPos n (Position M C) = (n, n)
    getPos n (Position M R) = (n, n + n)
    getPos n (Position B L) = (n + n, 0)
    getPos n (Position B C) = (n + n, n)
    getPos n (Position B R) = (n + n, n + n)
