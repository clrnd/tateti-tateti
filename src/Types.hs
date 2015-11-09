{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Types where

import UI.NCurses
import Lens.Simple
import Control.Monad.State.Strict


type Game a = StateT GameState Curses a

data Player = X
            | O
            deriving (Show, Eq)
data Winner = Player Player
            | Draw
            deriving Show

data Movement = KUp | KRight | KDown | KLeft
              deriving Show
data Input = Movement Movement
           | Select | Quit
           deriving Show

data Mode = Free | Fixed deriving Show

type BoardPosition = Position Vertical Horizontal

data Position a b = Position a b deriving Show
data Vertical = T | M | B deriving (Show, Enum)
data Horizontal = L | C | R deriving (Show, Enum)

data BoardState t = BoardState
    { _bsTL :: t , _bsTC :: t , _bsTR :: t
    , _bsML :: t , _bsMC :: t , _bsMR :: t
    , _bsBL :: t , _bsBC :: t , _bsBR :: t
    , _bsPosition :: BoardPosition
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


defaultBoard :: a -> BoardState a
defaultBoard a = BoardState
    { _bsTL=a , _bsTC=a , _bsTR=a
    , _bsML=a , _bsMC=a , _bsMR=a
    , _bsBL=a , _bsBC=a , _bsBR=a
    , _bsPosition=Position M C
    , _bsWinner=Nothing }


positionToLens p = case p of
    Position T L -> bsTL
    Position T C -> bsTC
    Position T R -> bsTR
    Position M L -> bsML
    Position M C -> bsMC
    Position M R -> bsMR
    Position B L -> bsBL
    Position B C -> bsBC
    Position B R -> bsBR
