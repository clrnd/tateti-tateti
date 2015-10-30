{-# LANGUAGE LambdaCase #-}
module Draw where

import Lens.Simple
import UI.NCurses

import Types


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


movePlayer :: Input -> Game ()
movePlayer input = do
    use gMode >>= \case
        Free -> do
            current <- use $ gBoardState . bsPosition
            --let new = movePlayer' input current
            return ()
        Fixed -> return ()

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
