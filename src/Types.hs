{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Types where

import Control.Monad.State.Strict
import Data.Array
import Lens.Simple
import UI.NCurses


type Game a = StateT GameState Curses a

type Colors = Array Player ColorID

data Player = X
            | O
            deriving (Show, Eq, Ord, Ix)
data Winner = Player Player
            | Draw
            deriving Show

data Movement = KUp | KRight | KDown | KLeft
              deriving Show
data Input = Movement Movement
           | Select | Quit
           deriving Show

data Mode = Free | Fixed deriving Show

data Position = Position Vertical Horizontal
              deriving (Show, Eq, Ord, Ix)
data Vertical = T | M | B deriving (Show, Enum, Eq, Ord, Ix)
data Horizontal = L | C | R deriving (Show, Enum, Eq, Ord, Ix)

data BoardState t = BoardState
    { _bsCells :: Array Position t
    , _bsPosition :: Position
    , _bsWinner :: Maybe Winner
    } deriving Show

data GameState = GameState
    { _gPlayer :: Player
    , _gBoardState :: BoardState (BoardState (Maybe Player))
    , _gMode :: Mode
    , _gQuit :: Bool
    } deriving Show

$(makeLenses ''GameState)
$(makeLenses ''BoardState)
