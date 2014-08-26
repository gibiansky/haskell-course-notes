module Main(main) where

import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Random

type Radius = Float
type Position = (Float, Float)

ballRadius :: Radius
ballRadius = 10

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: Position        -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen.
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show

randomInitialState :: StdGen -> PongGame
randomInitialState gen = Game
  { ballLoc = (a, b)
  , ballVel = (c', d')
  , player1 = 0
  , player2 = 0
  }
  where
    a:b:c:d:_ = randomRs (-50, 50) gen
    c' = c * mag
    d' = d * mag
    mag = 200 / sqrt (c^2 + d^2)


width, height, offset :: Num a => a
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

main :: IO ()
main = do
  gen <- getStdGen
  let initState = randomInitialState gen
  playIO window background fps initState render handleKeys update

render :: PongGame -> IO Picture
render game = return $ 
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture 
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Update the game by moving the ball.
update :: Float -> PongGame -> IO PongGame
update seconds game = 
  if gameEnded game'
  then do
    putStrLn "Game ended!"
    exitSuccess
  else return game'

  where
    game' = paddleBounce . wallBounce . moveBall seconds $ game

-- | Check if a game has ended.
gameEnded :: PongGame -> Bool
gameEnded game = farLeft || farRight
  where
    (x, _) = ballLoc game
    farLeft = x < -fromIntegral width / 2 + 2 * ballRadius
    farRight = x > fromIntegral width / 2 - 2 * ballRadius

-- | Respond to key events.
handleKeys :: Event -> PongGame -> IO PongGame
handleKeys event game = case event of
  EventKey (Char 'q') _ _ _ -> exitSuccess
  EventKey (Char 'w') _ _ _ -> return $
    game { player2 = player2 game + 10 }
  EventKey (Char 's') _ _ _ ->  return $
    game { player2 = player2 game - 10 }
  EventKey (SpecialKey KeyUp) _ _ _ ->  return $
    game { player1 = player1 game + 10 }
  EventKey (SpecialKey KeyDown) _ _ _ ->  return $
    game { player1 = player1 game - 10 }
  _ -> return game

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) ballRadius
          then 
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

paddleCollision :: Position -> PongGame -> Bool
paddleCollision (x, y) game = 
  (x + ballRadius > 110 && abs (y - player1 game) < 40) || 
  (x - ballRadius < -110 && abs (y - player2 game) < 40)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- The old velocities.
    (vx, vy) = ballVel game
    vx' = if paddleCollision (ballLoc game) game then -vx else vx

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
