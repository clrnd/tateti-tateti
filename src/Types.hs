{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Types where

import UI.NCurses
import Lens.Simple
import Control.Monad.State.Lazy


type Game a = StateT GameState Curses a

data Player = X
            | O
            deriving Show

data Input = U | R | D | L
           | Select | Quit
           deriving Show

data Mode = Free | Fixed deriving Show

data BoardPosition = TL | TM | TR
                   | ML | MM | MR
                   | BL | BM | BR
                   deriving Show

data BoardState t = BoardState
    { _bsTL :: t , _bsTM :: t , _bsTR :: t
    , _bsML :: t , _bsMM :: t , _bsMR :: t
    , _bsBL :: t , _bsBM :: t , _bsBR :: t
    , _bsPosition :: BoardPosition
    , _bsWinner :: Maybe Player
    } deriving Show

data GameState = GameState
    { _gPlayer :: Player
    , _gBoardState :: BoardState (BoardState (Maybe Player))
    , _gMode :: Mode
    } deriving Show

defaultBoard :: a -> BoardState a
defaultBoard a = BoardState
    { _bsTL=a , _bsTM=a , _bsTR=a
    , _bsML=a , _bsMM=a , _bsMR=a
    , _bsBL=a , _bsBM=a , _bsBR=a
    , _bsPosition=MM
    , _bsWinner=Nothing }

$(makeLenses ''GameState)
$(makeLenses ''BoardState)
