module Main where

import Control.Monad.Trans
import Control.Monad.State.Lazy
import Lens.Simple
import UI.NCurses

import Types
import Draw


main :: IO ()
main = do
    let game = GameState { _gPlayer=X
                         , _gBoardState=defaultBoard (
                                         defaultBoard Nothing)
                         , _gMode = Free }
    _ <- runCurses . flip runStateT game $ do
        lift $ setEcho False
        w <- lift $ newWindow 23 23 1 1

        lift $ updateWindow w $ drawCrosses
        lift $ render

        mainLoop w
    return ()


mainLoop :: Window -> Game ()
mainLoop w = do

    input <- parseInput w
    --liftIO $ print input

    mainLoop w


parseInput :: Window -> Game Input
parseInput w = do
    ev <- lift $ getEvent w Nothing
    case ev of
        Just (EventCharacter 'q') -> return Quit
        Just (EventSpecialKey k) ->
            case k of
                KeyUpArrow -> return U
                KeyRightArrow -> return R
                KeyDownArrow -> return D
                KeyLeftArrow -> return L
                _ -> parseInput w
        _ -> parseInput w
