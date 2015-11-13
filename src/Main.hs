{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Array
import Data.Maybe
import Lens.Simple
import UI.NCurses
import System.Random

import SelectScreen
import Types
import Draw
import Util

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

        colors <- getColors

        whoPlaysLoop w1 colors (CPlayer X) >>= \case
            -- quited
            Nothing -> return ()
            -- chose something
            Just m_choice -> do
                pl <- case m_choice of
                    CPlayer pl -> return pl
                    CRandom -> do
                        n <- liftIO randomIO :: Game Int
                        if n < 0
                            then return X
                            else return O
                gPlayer .= pl
                lift $ updateWindow w1 clear
                mainLoop w1 w2 colors

        -- cleaning up
        lift $ closeWindow w1
        lift $ closeWindow w2


mainLoop :: Window -> Window -> Colors -> Game ()
mainLoop w1 w2 colors = do
    gs <- get
    lift $ updateWindow w1 $ drawCrosses gs colors
    lift $ updateWindow w2 $ drawMessages gs colors
    lift $ updateWindow w1 $ drawMarks gs colors
    lift $ updateWindow w1 $ drawCursor gs

    lift render

    parseInput w1 >>= \case
        Movement m -> movePlayer m
        Select -> use gMode >>= \case
            Free -> do
                p <- use (gBoardState . bsPosition)
                use (gBoardState . bsAx p . bsWinner) >>= \case
                    -- board is already closed, do nothing
                    Just _ -> return ()
                    -- board is open, enter
                    Nothing -> gMode .= Fixed
            Fixed -> actionPlayer >>= \case
                -- illegal action, do noting
                Nothing -> return ()

                -- legal action, `played_p` is where they played
                Just played_p -> do

                    -- calculate winners
                    p <- use (gBoardState . bsPosition)
                    gBoardState . bsAx p . bsWinner <~ calcWinners played_p

                    -- switch players
                    gPlayer %= \x -> if x == X then O else X

                    -- move to next board
                    gBoardState . bsPosition .= played_p
                    p' <- use (gBoardState . bsPosition)

                    -- enter free mode if closed
                    use (gBoardState . bsAx p' . bsWinner) >>= \case
                        Nothing -> return ()
                        Just _ -> gMode .= Free
        Quit -> gQuit .= True

    if gs ^. gQuit
        then return ()
        else mainLoop w1 w2 colors


-- | Acts on a user marking a cell, on success returns which position.
actionPlayer :: Game (Maybe Position)
actionPlayer = do
    pl <- use gPlayer

    -- check empty space
    pos <- use (gBoardState . bsPosition)

    zoom (gBoardState . bsAx pos) $ do

        pos' <- use bsPosition

        use (bsAx pos') >>= \case
            -- the spot is already occupied
            Just _ -> return Nothing

            -- the spot is free
            Nothing -> do
                bsAx pos' .= Just pl
                return $ Just pos'


movePlayer :: Movement -> Game ()
movePlayer input = do
    use gMode >>= \case
        Free -> do
            p <- use (gBoardState . bsPosition)
            let new_p = movePlayer' input p
            gBoardState . bsPosition .= new_p
        Fixed -> do
            p <- use (gBoardState . bsPosition)
            p' <- use (gBoardState . bsAx p . bsPosition)
            let new_p = movePlayer' input p'
            gBoardState . bsAx p . bsPosition .= new_p
  where
    movePlayer' KUp (Position T h) = Position T h
    movePlayer' KUp (Position v h) = Position (pred v) h

    movePlayer' KRight (Position v R) = Position v R
    movePlayer' KRight (Position v h) = Position v (succ h)

    movePlayer' KDown (Position B h) = Position B h
    movePlayer' KDown (Position v h) = Position (succ v) h

    movePlayer' KLeft (Position v L) = Position v L
    movePlayer' KLeft (Position v h) = Position v (pred h)


calcWinners :: Position -> Game (Maybe Winner)
calcWinners played_p = do
    pl <- use gPlayer
    p <- use (gBoardState . bsPosition)
    cells <- use (gBoardState . bsAx p . bsCells)
    return $ calcWinners' pl cells
  where
    calcWinners' pl cells =
        let Position v h = played_p
            draw = checkDraw
            w_v = checkVertical v
            w_h = checkHorizontal h
            w_d = if isDiagonal played_p
                    then checkDiagonal
                    else False
        in
        if or [w_h, w_v, w_d]
            then Just (Player pl)
            else if draw
                then Just Draw
                else Nothing
      where
        check cond = all (\p -> cond (cells ^. ax p))
        checkDiagonal =
            let directions = [ [ Position T L
                               , Position M C
                               , Position B R ]
                             , [ Position T R
                               , Position M C
                               , Position B L ] ]
            in any (check (==Just pl)) directions
        checkVertical v = check (==Just pl) [ Position v x | x <- [L .. R] ]
        checkHorizontal h = check (==Just pl) [ Position y h | y <- [T .. B] ]
        checkDraw = check isJust $ range (Position T L, Position B R)


getColors :: Game Colors
getColors = do
    r <- lift $ newColorID ColorRed ColorDefault 1
    b <- lift $ newColorID ColorBlue ColorDefault 2
    y <- lift $ newColorID ColorYellow ColorDefault 3
    g <- lift $ newColorID ColorGreen ColorDefault 4
    return $  \case
        Red -> r
        Blue -> b
        Yellow -> y
        Green -> g
