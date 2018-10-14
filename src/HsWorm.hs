module HsWorm where

import Control.Monad.State
import Data.Char
import Data.Time.Clock.POSIX
import System.Random
import UI.NCurses

type Milliseconds = Int

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show

data Position = Position { x :: Int, y :: Int } deriving Show
instance Eq Position where
    (==) a b = x a == x b && y a == y b


initialWormLength = 3
gameWidth = 80 
gameHeight = 25
totalWidth = gameWidth + 2
totalHeight = gameHeight + 2

data GameState = GameState
    { worm :: [Position]
    , direction :: Direction
    , alive :: Bool
    , score :: Int
    , omnom :: Position
    , quit :: Bool
    , moveInterval :: Milliseconds
    , lastUpdate :: Milliseconds
    , randomGen :: StdGen
    , errorMsg :: Maybe String
    {- Using 'clear' on every update would cause unwanted flickering. As a remedy,
       keep track of screen positions that have cleared so they can be erased
       by drawing a space character
       (in reality, the only thing that needs clearing is the previous worm tail's
        last piece)
    -}
    , clearPositions :: [Position]
    } deriving Show

initialState :: GameState
initialState = GameState
    { worm = [Position { x = gameWidth `div` 2 + x, y = gameHeight `div` 2} | x <- [0.. initialWormLength - 1]]
    , direction = DirLeft
    , alive = True
    , score = 0
    , omnom = Position { x = 0, y = 0 }
    , quit = False
    , moveInterval = 50  -- ms
    , lastUpdate = 0
    , clearPositions = []
    , randomGen = mkStdGen 0
    }


gameLoop :: GameState -> Curses GameState
gameLoop fromState = do
    event <- defaultWindow >>= nonBlockingPeekEvent
    millisecsNow <- (round . (* 1000)) <$> liftIO getPOSIXTime

    let (updated, state) = runState (updateGame millisecsNow event) fromState

    if quit state || (not . alive) state
        then return state
        else let renderAction = if updated
                                then defaultWindow >>=
                                    flip updateWindow (drawGame state) >>
                                    render
                                else return ()
            in renderAction >> gameLoop state
    

updateGame :: Milliseconds -> Maybe Event -> State GameState Bool
updateGame now event = do
    handleInput event
    nextUpdate <- nextUpdateAt  

    if now > nextUpdate
        then do
            s <- get
            put s { clearPositions = [] }
            s <- moveWorm
            alive' <- isWormAlive
            put s { lastUpdate = now, alive = alive' }

            return True
        else return False


isWormAlive :: State GameState Bool
isWormAlive = do
    s <- get
    let wormX = x $ head $ worm s
    let wormY = y $ head $ worm s
    let isInsideGameArea = wormX >= 0 && wormX <= gameWidth &&
                           wormY >= 0 && wormY < gameHeight

    return $ isInsideGameArea && (not . selfCollision $ worm s)


selfCollision :: [Position] -> Bool
selfCollision (head:tail) =
    any (== head) tail


-- Yep, this is just lastUpdate + moveInterval.
-- Sorry.
nextUpdateAt :: State GameState Milliseconds
nextUpdateAt =
    let values fs = (<*>) fs . pure
    in get >>= return . sum . values [lastUpdate, moveInterval]


moveWorm :: State GameState GameState
moveWorm = do
    s <- get
    hadMeal <- omnomEaten
    omnom' <- if hadMeal then randomPosition else return $ omnom s

    let w = worm s
        newHeadPos =
            let (x', y') = ((x . head) w, (y . head) w) in
                case direction s of
                    DirDown     -> (head w) { y = y' + 1 }
                    DirUp       -> (head w) { y = y' - 1 }
                    DirLeft     -> (head w) { x = x' - 1 }
                    DirRight    -> (head w) { x = x' + 1 }
        worm' = if hadMeal
                then newHeadPos : head w : init w
                else newHeadPos : init w

        score' = if hadMeal then (score s) + 1 else score s
    
    s <- get
    put s { worm = worm'
          , clearPositions = last w : clearPositions s
          , score = score'
          , omnom = omnom'
          }
    get >>= return


omnomEaten :: State GameState Bool
omnomEaten = do
    s <- get
    return $ (head . worm) s == omnom s


handleInput :: Maybe Event -> State GameState ()
handleInput event = do
    s <- get
    case event of
        Just e -> do
            let isQuit = case e of
                    EventCharacter c -> toLower c == 'q'
                    _ -> False
                
                newDirection = case e of
                        EventSpecialKey KeyUpArrow -> DirUp
                        EventSpecialKey KeyDownArrow -> DirDown
                        EventSpecialKey KeyLeftArrow -> DirLeft
                        EventSpecialKey KeyRightArrow -> DirRight
                        _ -> direction s
            put s { direction = newDirection, quit = isQuit }
        
        Nothing -> return ()


nonBlockingPeekEvent :: Window -> Curses (Maybe Event)
nonBlockingPeekEvent w = getEvent w (Just 0)


drawGame :: GameState -> Update ()
drawGame s = do
    mapM_ (stringPos " ") (fmap screenOffset (clearPositions s))
    drawWorm $ worm s

    (cursorPos . screenOffset $ omnom s) >> drawString "+"

    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    moveCursor 0 1
    drawString $ "Score: " ++ (show $ score s)


drawWorm :: [Position] -> Update ()
drawWorm (head:tail) =
    stringPos "@" (screenOffset head) >> mapM_ (stringPos "#" . screenOffset) tail


randomPosition :: State GameState Position
randomPosition = do
    x' <- randomNum 0 gameWidth
    y' <- randomNum 0 gameHeight
    return $ Position { x = x', y = y' }


randomNum :: Int -> Int -> State GameState Int
randomNum lower upper = do
    s <- get
    let (x, g') = randomR (lower, upper - 1) (randomGen s)
    put $ s { randomGen = g' }
    return x


-- Map game position to screen
screenOffset :: Position -> Position
screenOffset p = Position { x = x p + 1, y = y p + 1 }


cursorPos :: Position -> Update()
cursorPos pos = moveCursor ((toInteger . y) pos) ((toInteger . x) pos)


stringPos :: String -> Position -> Update()
stringPos s p =
    moveCursor (toInteger $ y p) (toInteger $ x p) >> drawString s