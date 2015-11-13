{-# LANGUAGE RankNTypes #-}
module Util where

import Control.Monad.Trans
import Data.Array
import Lens.Simple
import UI.NCurses

import Types


defaultBoard :: a -> BoardState a
defaultBoard a = BoardState
    { _bsCells=listArray (Position T L, Position B R) (repeat a)
    , _bsPosition=Position M C
    , _bsWinner=Nothing }


-- | Lens into an array
ax :: Ix i => i -> Lens (Array i a) (Array i a) a a
ax i = lens getter setter
  where
    getter = (! i)
    setter = (\arr v -> arr // [(i, v)])


bsAx :: Position -> Lens (BoardState t) (BoardState t) t t
bsAx p = bsCells . ax p


plusTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
plusTuple (a, b) (a', b') = (a + a', b + b')


isDiagonal :: Position -> Bool
isDiagonal (Position T L) = True
isDiagonal (Position T R) = True
isDiagonal (Position B L) = True
isDiagonal (Position B R) = True
isDiagonal (Position M C) = True
isDiagonal _ = False


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


parseInput :: Window -> Game Input
parseInput w = do
    ev <- lift $ getEvent w Nothing
    case ev of
        Just (EventCharacter 'q') -> return Quit
        Just (EventCharacter 'Q') -> return Quit
        Just (EventCharacter ' ') -> return Select
        Just (EventSpecialKey k) ->
            case k of
                KeyUpArrow -> return $ Movement KUp
                KeyRightArrow -> return $ Movement KRight
                KeyDownArrow -> return $ Movement KDown
                KeyLeftArrow -> return $ Movement KLeft
                _ -> parseInput w
        _ -> parseInput w
