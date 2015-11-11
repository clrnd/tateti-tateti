{-# LANGUAGE RankNTypes #-}
module Util where

import Data.Array
import Lens.Simple

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
