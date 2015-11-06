{-# LANGUAGE LambdaCase #-}
module Draw where

import Data.Monoid
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

drawPlayer :: GameState -> Update ()
drawPlayer gs =
    let outer_s = gs ^. gBoardState
        inner_l = case outer_s ^. bsPosition of
                    TL -> bsTL
                    TM -> bsTM
                    TR -> bsTR
                    ML -> bsML
                    MM -> bsMM
                    MR -> bsMR
                    BL -> bsBL
                    BM -> bsBM
                    BR -> bsBR
        inner_p = getPos 1 $ outer_s ^. inner_l . bsPosition
        outer_p = getPos 8 $ outer_s ^. bsPosition
    in
    uncurry moveCursor $ outer_p `plusTuple` inner_p
  where
    getPos _ TL = (0, 0)
    getPos n TM = (0, n)
    getPos n TR = (0, n + n)
    getPos n ML = (n, 0)
    getPos n MM = (n, n)
    getPos n MR = (n, n + n)
    getPos n BL = (n + n, 0)
    getPos n BM = (n + n, n)
    getPos n BR = (n + n, n + n)

plusTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
plusTuple (a, b) (a', b') = (a + a', b + b')
