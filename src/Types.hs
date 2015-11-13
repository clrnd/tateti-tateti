{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Types where

import Control.Monad.State.Strict
import Data.Array
import Lens.Simple
import UI.NCurses


-- | Main monad
type Game a = StateT GameState Curses a

data GameState = GameState
    { _gPlayer :: Player
    , _gBoardState :: BoardState (BoardState (Maybe Player))
    , _gMode :: Mode
    , _gQuit :: Bool
    } deriving Show

data Mode = Free | Fixed deriving Show

-- | State for a 3x3 board with an inner type for each cell
data BoardState t = BoardState
    { _bsCells :: Array Position t
    , _bsPosition :: Position
    , _bsWinner :: Maybe Winner
    } deriving Show


-- | Player and Winners
data Player = X
            | O
            deriving (Show, Eq, Ord)

data Winner = Player Player
            | Draw
            deriving (Show, Eq, Ord)


-- | Movement and Input simplified from NCurses
data Movement = KUp | KRight | KDown | KLeft
              deriving Show

data Input = Movement Movement
           | Select | Quit
           deriving Show


-- | Position types
data Position = Position Vertical Horizontal
              deriving (Show, Eq, Ord, Ix)

data Vertical = T | M | B deriving (Show, Enum, Eq, Ord, Ix)

data Horizontal = L | C | R deriving (Show, Enum, Eq, Ord, Ix)


-- | Color abstraction
type Colors = GameColor -> ColorID

data GameColor = Red | Blue | Yellow | Green

class Colorable a where
    color :: a -> GameColor

instance Colorable Player where
    color X = Red
    color O = Blue

instance Colorable Winner where
    color (Player pl) = color pl
    color Draw = Yellow

instance Colorable Mode where
    color Fixed = Yellow
    color Free = Green

$(makeLenses ''GameState)
$(makeLenses ''BoardState)
