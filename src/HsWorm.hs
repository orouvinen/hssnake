module HsWorm where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Time.Clock.POSIX
import           System.Random
import           UI.NCurses

type Milliseconds = Int

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show

data Position = Position
    { x :: Int
    , y :: Int
    } deriving Show

instance Eq Position where
    (==) a b = x a == x b && y a == y b


initialWormLength :: Int
initialWormLength = 3

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

data GameState = GameState
    { worm           :: [Position]
    , direction      :: Direction
    , alive          :: Bool
    , score          :: Int
    , omnom          :: Position
    , quit           :: Bool
    , moveInterval   :: Milliseconds
    , lastUpdate     :: Milliseconds
    , randomGen      :: StdGen
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
    { worm = [ Position
               { x = gameWidth `div` 2 + x'
               , y = gameHeight `div` 2
               } | x' <- [0.. initialWormLength - 1]
             ]
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

fromState :: (GameState -> a) -> State GameState a
fromState f = get >>= \s -> pure $ f s

gameLoop :: GameEnv -> GameState -> Curses GameState
gameLoop env st = do
    event <- defaultWindow >>= nonBlockingPeekEvent
    millisecsNow <- (round . (* 1000)) <$> liftIO getPOSIXTime

    let (updated, s) = runState (updateGame millisecsNow event) st
    let drawUpdate = runReaderT (drawGame s) env

    if quit s || (not . alive) s
        then return s
        else let renderAction =
                     if updated
                     then defaultWindow >>=
                          flip updateWindow drawUpdate >>
                          render
                     else return ()
             in renderAction >> gameLoop env s

updateGame :: Milliseconds -> Maybe Event -> State GameState Bool
updateGame now event = do
    handleInput event
    nextUpdate <- nextUpdateAt

    if now > nextUpdate
        then do
            get >>= \s -> put s { clearPositions = [] }
            s <- moveWorm
            alive' <- isWormAlive
            put s { lastUpdate = now, alive = alive' }
            return True
        else return False

isWormAlive :: State GameState Bool
isWormAlive = do
    s <- get
    let wormX = (x . head . worm) s
    let wormY = (y . head . worm) s
    let isInsideGameArea = wormX >= 0 && wormX <= gameWidth &&
                           wormY >= 0 && wormY < gameHeight

    return $ isInsideGameArea && (not . selfCollision $ worm s)


stateOp :: (a -> b -> c) -> (GameState -> a) -> (GameState -> b) -> State GameState c
stateOp f g h = get >>= \s -> pure $ f (g s) (h s)

nextUpdateAt :: State GameState Milliseconds
nextUpdateAt = stateOp (+) lastUpdate moveInterval

moveWorm :: State GameState GameState
moveWorm = do
    hadMeal <- omnomEaten
    omnom' <- if hadMeal then randomPosition else fromState omnom --omnom s

    w <- fromState worm
    d <- fromState direction
    score' <- fromState score

    let newHeadPos =
            let pos = head w
            in
                case d of
                    DirDown  -> pos { y = (+) (y $ pos) 1 }
                    DirUp    -> pos { y = (-) (y $ pos) 1 }
                    DirLeft  -> pos { x = (-) (x $ pos) 1 }
                    DirRight -> pos { x = (+) (x $ pos) 1 }

        worm' = if hadMeal
                then newHeadPos : head w : init w
                else newHeadPos : init w

        newScore = score' + if hadMeal then 1 else 0

    s <- get
    put s { worm = worm'
          , clearPositions = last worm' : clearPositions s
          , score = newScore
          , omnom = omnom'
          }
    get >>= return

omnomEaten :: State GameState Bool
omnomEaten = get >>= \s -> return $ (head . worm) s == omnom s

handleInput :: Maybe Event -> State GameState ()
handleInput event = do
    s <- get
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
                        _                             -> direction s
            put s { direction = newDirection, quit = isQuit }

        Nothing -> return ()

randomPosition :: State GameState Position
randomPosition = do
    x' <- randomNum 0 gameWidth
    y' <- randomNum 0 gameHeight
    return $ Position { x = x', y = y' }

randomNum :: Int -> Int -> State GameState Int
randomNum lower upper = do
    s <- get
    let (x', g') = randomR (lower, upper - 1) (randomGen s)
    put $ s { randomGen = g' }
    return x'

selfCollision :: [Position] -> Bool
selfCollision []            = False
selfCollision (head':tail') = any (== head') tail'

nonBlockingPeekEvent :: Window -> Curses (Maybe Event)
nonBlockingPeekEvent w = getEvent w (Just 0)

drawGame :: GameState -> ReaderT GameEnv Update ()
drawGame s = do
    env <- ask
    drawWorm $ worm s
    lift $ do
        mapM_ (stringPos " ") (fmap screenOffset (clearPositions s))

        setColor $ omnomColorID env
        (cursorPos . screenOffset $ omnom s) >> drawString "+"
        setColor defaultColorID

        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

        moveCursor 0 1
        drawString $ "Score: " ++ (show $ score s)

drawWorm :: [Position] -> ReaderT GameEnv Update ()
drawWorm [] = return ()
drawWorm (head':tail') = do
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

