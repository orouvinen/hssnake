module Main where
import           Data.Time.Clock.POSIX
import           System.Random
import           UI.NCurses

import           HsWorm

main :: IO ()
main = do
    -- Random initial meal position
    omnomX <- getStdRandom $ randomR (0, gameWidth - 1)
    omnomY <- getStdRandom $ randomR (0, gameHeight - 1)
    now <- (round . (* 1000)) <$> getPOSIXTime
    let firstOmnom = Omnom
            { position = Position
                         { x = omnomX
                         , y = omnomY
                         }
            , endTime = now + omnomTime
            }
    g <- getStdGen
    res <- runGame initialState { omnom = firstOmnom, randomGen = g }
    case res of
        Left err -> putStrLn err
        Right gameState ->
            putStrLn $ "Game over!\nFinal score: " ++ (show $ score gameState)


initEnv :: Curses GameEnv
initEnv = do
    headColorDef <- newColorID headColor ColorDefault 1
    bodyColorDef <- newColorID bodyColor ColorDefault 2
    omnomColorDef <- newColorID omnomColor ColorDefault 3
    return GameEnv
        { headColorID = headColorDef
        , bodyColorID = bodyColorDef
        , omnomColorID = omnomColorDef
        }


runGame :: GameState -> IO (Either String GameState)
runGame startState = do
    termBigEnough <- runCurses $ do
        (h, w) <- screenSize
        return $ h >= toInteger totalHeight && w >= toInteger totalWidth

    case termBigEnough of
        False ->
            return
                $ Left $ "You need at least "
                ++ show totalWidth
                ++ "x"
                ++ show (totalHeight + 1)
                ++ " size terminal."
        True -> do
            endState <- runCurses $ do
                _ <- setCursorMode CursorInvisible
                setEcho False

                -- resize window to make room for 1 char width border on each side
                w <- defaultWindow
                updateWindow w $ do
                    resizeWindow (toInteger totalHeight) (toInteger totalWidth)
                    clear

                gameEnv <- initEnv
                gameLoop gameEnv startState >>= return
            return $ Right endState

