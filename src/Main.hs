{-# LANGUAGE LambdaCase #-}
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
    movePlayer input

    gs <- get
    lift $ updateWindow w $ drawPlayer gs
    lift $ render
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


movePlayer :: Input -> Game ()
movePlayer input = do
    use gMode >>= \case
        Free -> do
            current <- use $ gBoardState . bsPosition
            let newPos = movePlayer' input current
            gBoardState . bsPosition .= newPos
            return ()
        Fixed -> return ()
  where
    movePlayer' U TL = TL
    movePlayer' U TM = TM
    movePlayer' U TR = TR
    movePlayer' U ML = TL
    movePlayer' U MM = TM
    movePlayer' U MR = TR
    movePlayer' U BL = ML
    movePlayer' U BM = MM
    movePlayer' U BR = MR

    movePlayer' R TL = TM
    movePlayer' R TM = TL
    movePlayer' R TR = TR
    movePlayer' R ML = MM
    movePlayer' R MM = MR
    movePlayer' R MR = MR
    movePlayer' R BL = BM
    movePlayer' R BM = BR
    movePlayer' R BR = BR

    movePlayer' D TL = ML
    movePlayer' D TM = MM
    movePlayer' D TR = MR
    movePlayer' D ML = BL
    movePlayer' D MM = BM
    movePlayer' D MR = BR
    movePlayer' D BL = BL
    movePlayer' D BM = BM
    movePlayer' D BR = BR

    movePlayer' L TL = TL
    movePlayer' L TM = TL
    movePlayer' L TR = TM
    movePlayer' L ML = ML
    movePlayer' L MM = ML
    movePlayer' L MR = MM
    movePlayer' L BL = BL
    movePlayer' L BM = BL
    movePlayer' L BR = BM

    movePlayer' _ _ = undefined
