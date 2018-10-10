module Main where
import UI.NCurses
import System.Random

import HsWorm


main :: IO ()
main = do
    -- Random initial meal position
    omnomX <- getStdRandom (randomR (0, gameWidth - 1))
    omnomY <- getStdRandom (randomR (0, gameHeight - 1))
    let firstOmnom = Position { x = omnomX, y = omnomY }
    g <- getStdGen

    endState <- runCurses $ do
        setCursorMode CursorInvisible
        setEcho False

        -- resize window to make room for 1 char width border on each side
        w <- defaultWindow
        updateWindow w $ do
            resizeWindow (toInteger totalHeight) (toInteger totalWidth)

        -- Initial clear screen
        defaultWindow >>= flip updateWindow clear

        let startState = initialState { omnom = firstOmnom, randomGen = g }
        gameLoop startState >>= return

    putStrLn "Game over!"
    putStrLn $ "Final score: " ++ (show $ score endState)
