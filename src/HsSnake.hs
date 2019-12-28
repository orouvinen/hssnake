module HsSnake where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Time.Clock.POSIX
import           System.Random
import           Text.Printf           (printf)
import           UI.NCurses

type Milliseconds = Int

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show

data Position = Position
    { x :: Int
    , y :: Int
    } deriving Show

instance Eq Position where
    (==) a b = x a == x b && y a == y b


initialSnakeLength :: Int
initialSnakeLength = 3

omnomTime :: Milliseconds
omnomTime = 10000

gameWidth :: Int
gameWidth = 80

gameHeight :: Int
gameHeight = 25

totalWidth :: Int
totalWidth = gameWidth + 2

totalHeight :: Int
totalHeight = gameHeight + 2

headColor :: Color
headColor = ColorWhite

bodyColor :: Color
bodyColor = ColorBlue

omnomColor :: Color
omnomColor = ColorYellow

-- For now, game env. only holds color information for different screen elements
data GameEnv = GameEnv
    { headColorID  :: ColorID
    , bodyColorID  :: ColorID
    , omnomColorID :: ColorID
    }

data Omnom = Omnom
    { position :: Position
    , endTime  :: Milliseconds
    } deriving Show

data GameState = GameState
    { snake          :: [Position]
    , direction      :: Direction
    , alive          :: Bool
    , score          :: Int
    , omnom          :: Omnom
    , quit           :: Bool
    , moveInterval   :: Milliseconds
    , currentTick    :: Milliseconds
    , lastUpdate     :: Milliseconds
    , randomGen      :: StdGen
    {- Using 'clear' on every update would cause unwanted flickering. As a remedy,
       keep track of screen positions that have cleared so they can be erased
       by drawing a space character
       (in reality, the only thing that needs clearing is the previous snake's tail's
       last piece)
    -}
    , clearPositions :: [Position]
    } deriving Show

initialState :: GameState
initialState = GameState
    { snake = [ Position
               { x = gameWidth `div` 2 + x'
               , y = gameHeight `div` 2
               } | x' <- [0.. initialSnakeLength - 1]
             ]
    , direction = DirLeft
    , alive = True
    , score = 0
    , omnom = Omnom { position = Position { x = 0, y = 0 }, endTime = 0 }
    , quit = False
    , moveInterval = 50  -- ms
    , currentTick = 0
    , lastUpdate = 0
    , clearPositions = []
    , randomGen = mkStdGen 0
    }

gameLoop :: GameEnv -> GameState -> Curses GameState
gameLoop env st = do
    event <- defaultWindow >>= nonBlockingPeekEvent
    millisecsNow <- (round . (* 1000)) <$> liftIO getPOSIXTime

    let (updated, s) = runState (updateGame millisecsNow event) st
    let drawUpdate = runReaderT (drawGame s) env

    if quit s || (not . alive) s
        then pure s
        else let renderGame =
                    if updated then do
                        w <- defaultWindow
                        updateWindow w drawUpdate
                        render
                    else pure ()
             in do
                renderGame
                gameLoop env s

updateGame :: Milliseconds -> Maybe Event -> State GameState Bool
updateGame now event = do
    handleInput event
    modify (\s -> s { currentTick = now, clearPositions = [] })
    nextUpdate <- nextUpdateAt

    if now > nextUpdate
        then do s <- moveSnake
                put s { lastUpdate = now }
                pure True
        else pure False

isSnakeAlive :: State GameState Bool
isSnakeAlive = do
    selfCollision' <- selfCollision
    outOfTime <- omnomTimeExpired
    snake' <- gets snake

    let snakeX = (x . head) snake'
        snakeY = (y . head) snake'
        isInsideGameArea = snakeX >= 0 && snakeX <= gameWidth &&
                           snakeY >= 0 && snakeY < gameHeight

    pure $ isInsideGameArea && not selfCollision' && not outOfTime

omnomTimeExpired :: State GameState Bool
omnomTimeExpired = do
    snakePos <- gets (head . snake)
    currentTick' <- gets currentTick
    omnomEnd <- gets (endTime . omnom)
    omnomPos <- gets (position . omnom)
    pure $ currentTick' >= omnomEnd && snakePos /= omnomPos

nextUpdateAt :: State GameState Milliseconds
nextUpdateAt = (+) <$> gets lastUpdate <*> gets moveInterval

willEat :: State GameState Bool
willEat =
    let snakePos = gets (head . snake)
        omnomPos = gets (position . omnom)
    in (==) <$> snakePos <*> omnomPos

tryToEat :: State GameState Bool
tryToEat = do
    hadMeal <- willEat
    currentTick' <- gets currentTick
    omnom' <- if hadMeal then newOmnom else gets omnom
    modify (\s -> s { omnom = omnom'
                    , score = score s + if hadMeal
                                        then 1
                                        else 0
                    })
    pure hadMeal

newOmnom :: State GameState Omnom
newOmnom = do
    now <- gets currentTick
    pos <- randomPosition
    pure $ Omnom { position = Position { x = x pos
                                       , y = y pos
                                       }
                 , endTime = now + omnomTime
                 }

moveSnake :: State GameState GameState
moveSnake = do
    hadMeal <- tryToEat
    snake <- gets snake
    direction <- gets direction
    let newHeadPos =
            let pos = head snake
            in
                case direction of
                    DirDown  -> pos { y = (+) (y $ pos) 1 }
                    DirUp    -> pos { y = (-) (y $ pos) 1 }
                    DirLeft  -> pos { x = (-) (x $ pos) 1 }
                    DirRight -> pos { x = (+) (x $ pos) 1 }

        newSnake = if hadMeal
                   then newHeadPos : head snake : init snake
                   else newHeadPos : init snake

    modify (\s -> s { snake = newSnake
                    , clearPositions = last newSnake : clearPositions s
                    , alive = evalState isSnakeAlive s { snake = newSnake }
                    })
    get

handleInput :: Maybe Event -> State GameState ()
handleInput event = do
    currentDirection <- gets direction
    case event of
        Just e -> do
            let isQuit = case e of
                    EventCharacter c -> toLower c == 'q'
                    _                -> False

                newDirection = case e of
                        EventSpecialKey KeyUpArrow    -> DirUp
                        EventSpecialKey KeyDownArrow  -> DirDown
                        EventSpecialKey KeyLeftArrow  -> DirLeft
                        EventSpecialKey KeyRightArrow -> DirRight
                        _                             -> currentDirection
            modify (\s -> s { direction = newDirection, quit = isQuit })

        Nothing -> pure ()

randomPosition :: State GameState Position
randomPosition = do
    x' <- randomNum 0 gameWidth
    y' <- randomNum 0 gameHeight
    pure $ Position { x = x', y = y' }

selfCollision :: State GameState Bool
selfCollision = do
    snake' <- gets snake
    omnomPos <- gets (position . omnom)
    pure $
        any (== head snake') (tail snake') &&
        (head snake') /= omnomPos -- allow eating omnom that is on top of the snake

randomNum :: Int -> Int -> State GameState Int
randomNum lower upper = do
    randomGen <- gets randomGen
    let (x, g) = randomR (lower, upper - 1) randomGen
    modify (\s -> s { randomGen = g })
    pure x

nonBlockingPeekEvent :: Window -> Curses (Maybe Event)
nonBlockingPeekEvent w = getEvent w (Just 0)

drawGame :: GameState -> ReaderT GameEnv Update ()
drawGame s = do
    env <- ask
    drawSnake $ snake s
    lift $ do
        mapM_ (stringPos " ") (fmap screenOffset (clearPositions s))

        setColor $ omnomColorID env
        (cursorPos . screenOffset $ (position . omnom) s) >> drawString "+"
        setColor defaultColorID

        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

        moveCursor 0 1
        drawString $ "Score: " ++ (show $ score s)

        moveCursor 0 67
        let timeLeft = (fromIntegral (endTime (omnom s) - currentTick s)) / 1000 :: Float
        drawString $ printf "Time left: %.1f" timeLeft

drawSnake :: [Position] -> ReaderT GameEnv Update ()
drawSnake [] = pure ()
drawSnake (head':tail') = do
    env <- ask
    lift $ do
        setColor $ headColorID env
        stringPos "@" (screenOffset head')
        setColor $ bodyColorID env
        mapM_ (stringPos "#" . screenOffset) tail'

cursorPos :: Position -> Update()
cursorPos pos = moveCursor ((toInteger . y) pos) ((toInteger . x) pos)

stringPos :: String -> Position -> Update()
stringPos s p =
    moveCursor (toInteger $ y p) (toInteger $ x p) >> drawString s

-- Map game position to screen
screenOffset :: Position -> Position
screenOffset p = Position { x = x p + 1, y = y p + 1 }

