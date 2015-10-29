module Types where

data Player = X
            | O
            deriving Show

data BoardPosition = TL | TM | TR
                   | ML | MM | MR
                   | BL | BM | BR
                   deriving Show

data BoardState t = BoardState
    { bsTL :: t , bsTM :: t , bsTR :: t
    , bsML :: t , bsMM :: t , bsMR :: t
    , bsBL :: t , bsBM :: t , bsBR :: t
    , bsPosition :: BoardPosition
    , bsWinner :: Maybe Player
    } deriving Show

data Mode = Free | Fixed deriving Show

data Game = Game
    { gPlayer :: Player
    , gBoardState :: BoardState (BoardState (Maybe Player))
    , gMode :: Mode
    } deriving Show

defaultBoard :: a -> BoardState a
defaultBoard a = BoardState
    { bsTL=a , bsTM=a , bsTR=a
    , bsML=a , bsMM=a , bsMR=a
    , bsBL=a , bsBM=a , bsBR=a
    , bsPosition=MM
    , bsWinner=Nothing }
