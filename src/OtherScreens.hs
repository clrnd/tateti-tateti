{-# LANGUAGE LambdaCase #-}
module OtherScreens where

import Control.Monad.Trans
import UI.NCurses

import Types
import Util


-- | Select screen types
data Choice = CPlayer Player
            | CRandom
            deriving Show


whoPlaysLoop :: Window -> Colors -> Choice -> Game (Maybe Choice)
whoPlaysLoop w colors who = do
    lift . updateWindow w $ do
        moveCursor 10 4
        drawString "Who should play?"

        uncurry moveCursor . getPos $ CPlayer X
        setColor . colors . color $ X
        drawString "X"

        uncurry moveCursor . getPos $ CPlayer O
        setColor . colors . color $ O
        drawString "O"

        uncurry moveCursor . getPos $ CRandom
        setColor . colors . color $ Draw
        drawString "Random"

        setColor defaultColorID

        uncurry moveCursor $ getPos who

    lift render

    parseInput w >>= \case
        Movement KLeft -> whoPlaysLoop w colors $ toLeft who
        Movement KRight -> whoPlaysLoop w colors $ toRight who
        Select -> return $ Just who
        Quit -> return Nothing
        _ -> whoPlaysLoop w colors who
  where
    toLeft (CPlayer X) = CPlayer X
    toLeft (CPlayer O) = CPlayer X
    toLeft CRandom = CPlayer O

    toRight (CPlayer X) = CPlayer O
    toRight (CPlayer O) = CRandom
    toRight CRandom = CRandom

    getPos (CPlayer X) = (12, 6)
    getPos (CPlayer O) = (12, 8)
    getPos CRandom = (12, 10)


endGameLoop :: Winner -> Window -> Colors -> Game ()
endGameLoop winner w colors = do
    lift . updateWindow w $ do

        moveCursor 0 2
        setColor . colors . color $ winner
        drawString $ case winner of
            Player pl -> show pl ++ " wins!"
            Draw -> "Draw lolz..."

    lift render

    parseInput w >>= \case
        Quit -> return ()
        _ -> endGameLoop winner w colors
