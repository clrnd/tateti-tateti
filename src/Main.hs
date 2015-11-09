{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans
import Control.Monad.State.Strict
import Lens.Simple
import UI.NCurses

import Types
import Draw


main :: IO ()
main =
    let game = GameState { _gPlayer=X
                         , _gBoardState=defaultBoard (
                                         defaultBoard Nothing)
                         , _gMode = Free
                         , _gQuit = False }
    in
    void . runCurses . flip runStateT game $ do
        lift $ setEcho False
        -- main window
        w1 <- lift $ newWindow 23 23 1 1
        -- message window
        w2 <- lift $ newWindow 3 15 (24 - 2) 24

        lift $ updateWindow w1 $ drawCrosses
        lift $ render

        mainLoop w1 w2

        -- cleaning up
        lift $ closeWindow w1
        lift $ closeWindow w2


mainLoop :: Window -> Window -> Game ()
mainLoop w1 w2 = do
    gs <- get
    lift $ updateWindow w2 $ drawMessages gs
    lift $ updateWindow w1 $ drawCursor gs

    lift $ render

    parseInput w1 >>= \case
        Movement m -> movePlayer m
        Select -> actionPlayer
        Quit -> gQuit .= True

    if gs ^. gQuit
        then return ()
        else mainLoop w1 w2


actionPlayer :: Game ()
actionPlayer = do
    use gMode >>= \case
        Free -> gMode .= Fixed
        Fixed -> do
            pl <- use gPlayer

            -- check empty space
            pos <- use $ gBoardState . bsPosition

            pos' <- use $ gBoardState . (positionToLens pos) . bsPosition

            use (gBoardState . (positionToLens pos) . (positionToLens pos')) >>= \case
                Nothing -> gBoardState . (positionToLens pos) . (positionToLens pos') .= Just pl
                Just _ -> return ()


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


movePlayer :: Movement -> Game ()
movePlayer input = do
    use gMode >>= \case
        Free -> do
            current <- use $ gBoardState . bsPosition
            let newPos = movePlayer' input current
            gBoardState . bsPosition .= newPos
        Fixed -> return ()
  where
    movePlayer' KUp (Position T h) = Position T h
    movePlayer' KUp (Position v h) = Position (pred v) h

    movePlayer' KRight (Position v R) = Position v R
    movePlayer' KRight (Position v h) = Position v (succ h)

    movePlayer' KDown (Position B h) = Position B h
    movePlayer' KDown (Position v h) = Position (succ v) h

    movePlayer' KLeft (Position v L) = Position v L
    movePlayer' KLeft (Position v h) = Position v (pred h)
