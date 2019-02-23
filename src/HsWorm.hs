module HsWorm where

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


initialWormLength :: Int
initialWormLength = 3

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
    { worm           :: [Position]
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
        else let renderAction =
                     if updated
                     then defaultWindow >>=
                          flip updateWindow drawUpdate >>
                          render
                     else pure ()
             in renderAction >> gameLoop env s


updateGame :: Milliseconds -> Maybe Event -> State GameState Bool
updateGame now event = do
    handleInput event
    s <- get
    put s { currentTick = now, clearPositions = [] }
    nextUpdate <- nextUpdateAt

    if now > nextUpdate
        then do s' <- moveWorm
                put s' { lastUpdate = now }
                pure True
        else pure False

isWormAlive :: GameState -> Bool
isWormAlive s =
    let wormX = (x . head . worm) s
        wormY = (y . head . worm) s
        isInsideGameArea = wormX >= 0 && wormX <= gameWidth &&
                           wormY >= 0 && wormY < gameHeight

    in
        isInsideGameArea &&
        (not $ selfCollision $ worm s) &&
        (not $ omnomMissed s)

omnomMissed :: GameState -> Bool
omnomMissed s =
    currentTick' > endTime (omnom') &&
    head worm' /= position omnom'
    where
        currentTick' = currentTick s
        worm' = worm s
        omnom' = omnom s


nextUpdateAt :: State GameState Milliseconds
nextUpdateAt = get >>= \s -> pure $ lastUpdate s + moveInterval s

willEat :: State GameState Bool
willEat = get >>= \s -> pure $ (head . worm) s == (position . omnom) s

tryToEat :: State GameState Bool
tryToEat = do
    hadMeal <- willEat
    currentTick' <- get >>= \s -> pure $ currentTick s
    omnom' <- if hadMeal
              then randomPosition >>=
                   \p -> pure Omnom { position = Position { x = x p
                                                            , y = y p
                                                            },
                                        endTime = (currentTick') + omnomTime
                                      }
              else get >>= \s -> pure $ omnom s
    s <- get
    put s { omnom = omnom', score = score s + if hadMeal then 1 else 0 }
    pure hadMeal

moveWorm :: State GameState GameState
moveWorm = do
    hadMeal <- tryToEat
    s <- get
    let w = worm s

    let newHeadPos =
            let pos = head w
            in
                case direction s of
                    DirDown  -> pos { y = (+) (y $ pos) 1 }
                    DirUp    -> pos { y = (-) (y $ pos) 1 }
                    DirLeft  -> pos { x = (-) (x $ pos) 1 }
                    DirRight -> pos { x = (+) (x $ pos) 1 }

        worm' = if hadMeal
                then newHeadPos : head w : init w
                else newHeadPos : init w

    put s { worm = worm'
          , clearPositions = last worm' : clearPositions s
          , alive = isWormAlive s
          }
    get >>= pure

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

        Nothing -> pure ()

randomPosition :: State GameState Position
randomPosition = do
    x' <- randomNum 0 gameWidth
    y' <- randomNum 0 gameHeight
    pure $ Position { x = x', y = y' }

randomNum :: Int -> Int -> State GameState Int
randomNum lower upper = do
    s <- get
    let (x', g') = randomR (lower, upper - 1) (randomGen s)
    put $ s { randomGen = g' }
    pure x'

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
        (cursorPos . screenOffset $ (position . omnom) s) >> drawString "+"
        setColor defaultColorID

        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

        moveCursor 0 1
        drawString $ "Score: " ++ (show $ score s)

        moveCursor 0 67
        let timeLeft = (fromIntegral (endTime (omnom s) - currentTick s)) / 1000 :: Float
        drawString $ printf "Time left: %.1f" timeLeft

drawWorm :: [Position] -> ReaderT GameEnv Update ()
drawWorm [] = pure ()
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

