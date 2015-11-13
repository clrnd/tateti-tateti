{-# LANGUAGE LambdaCase #-}
module Draw where

import Control.Monad
import Data.Array
import Lens.Simple
import UI.NCurses

import Types
import Util


drawCrosses :: GameState -> Colors -> Update ()
drawCrosses gs colors = do
    -- main cross
    drawCross 7 Nothing (0, 0)

    -- top row crosses
    let offsets = [1, 1 + 8, 1 + 8 + 8]
        coords = (,) <$> offsets <*> offsets
        poss = range (Position T L, Position B R)
        winner p = gs ^. gBoardState . bsAx p . bsWinner
        color_ids = map (winner >=> return . colors . color) poss

    mapM_ (uncurry $ drawCross 1) $ zip color_ids coords


drawCross :: Integer -> Maybe ColorID -> (Integer, Integer) -> Update ()
drawCross cellsize m_cid (y, x) = do
    case m_cid of
        Just cid -> setColor cid
        Nothing -> setColor defaultColorID

    moveCursor (cellsize + y) x
    drawLineH (Just glyphLineH) (cellsize * 3 + 2)
    moveCursor (cellsize + y + cellsize + 1) x
    drawLineH (Just glyphLineH) (cellsize * 3 + 2)

    moveCursor y (cellsize + x)
    drawLineV (Just glyphLineV) (cellsize * 3 + 2)
    moveCursor y (cellsize + x + cellsize + 1)
    drawLineV (Just glyphLineV) (cellsize * 3 + 2)

    setColor defaultColorID


drawMessages :: GameState -> Colors -> Update ()
drawMessages gs colors = do

    let mode = gs ^. gMode
    moveCursor 0 2
    clearLine
    drawString "Mode: "
    setColor . colors . color $ mode
    drawString (show $ mode)
    setColor defaultColorID

    let player = gs ^. gPlayer
    moveCursor 1 2
    drawString "Player: "
    setColor . colors . color $ player
    drawString $ show player
    setColor defaultColorID


drawCursor :: GameState -> Update ()
drawCursor gs =
    let p = gs ^. gBoardState . bsPosition
        p' = gs ^. gBoardState . bsCells . ax p . bsPosition
    in
    uncurry moveCursor $ positionToCoordinates p p'


drawMarks :: GameState -> Colors -> Update ()
drawMarks gs colors = do
    let poss = range (Position T L, Position B R)
    forM_ poss $ \p -> do
        let poss' = range (Position T L, Position B R)
        forM_ poss' $ \p' -> do
            let m_p = gs ^. gBoardState . bsAx p . bsAx p'
            case m_p of
                Nothing -> return ()
                Just player -> do
                    uncurry moveCursor $ positionToCoordinates p p'
                    setColor . colors . color $ player
                    drawString $ show player
