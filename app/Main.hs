module Main where

import UI.NCurses
import System.Random

import HsWorm

main :: IO ()
main = do
    -- Random initial meal position
    omnomX <- getStdRandom $ randomR (0, gameWidth - 1)
    omnomY <- getStdRandom $ randomR (0, gameHeight - 1)
    let firstOmnom = Position { x = omnomX, y = omnomY }
    g <- getStdGen
    res <- runGame initialState { omnom = firstOmnom, randomGen = g }
    case res of 
        Left err -> putStrLn err
        Right gameState -> putStrLn $ "Game over!\nFinal score: " ++ (show $ score gameState)


runGame :: GameState -> IO (Either String GameState)
runGame startState = do
    termBigEnough <- runCurses $ do
        (h, w) <- screenSize
        return $ h >= toInteger totalHeight && w >= toInteger totalWidth
    
    case termBigEnough of
        False -> return $ Left $ "You need at least " ++ show totalWidth ++ "x" ++
                                 show (totalHeight + 1) ++ " size terminal."
        True -> do
            endState <- runCurses $ do
                setCursorMode CursorInvisible
                setEcho False

                -- resize window to make room for 1 char width border on each side
                w <- defaultWindow
                updateWindow w $ do
                    resizeWindow (toInteger totalHeight) (toInteger totalWidth)

                -- Initial clear screen
                defaultWindow >>= flip updateWindow clear

                gameLoop startState >>= return
            return $ Right endState
    