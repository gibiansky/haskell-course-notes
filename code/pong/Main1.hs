module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = display window background drawing

drawing :: Picture
drawing = drawing1

drawing0' = pictures
  [ circleSolid 30
  , rectangleSolid 10 50
  ]

drawing0'' :: Picture
drawing0'' = pictures
  [ color ballColor $ circleSolid 30
  , color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)


drawing0''' :: Picture
drawing0''' = pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)



drawing0 :: Picture
drawing0 = pictures
  [ color red $ circleSolid 10
  , color yellow $ rectangleSolid 24 104
  , color blue $ rectangleSolid 20 100
  , color white $ rectangleSolid 24 104
  , color blue $ rectangleSolid 20 100
  ]

drawing1 :: Picture
drawing1 = pictures [ball, mkPaddle rose 120 (-20), mkPaddle orange (-120) 40, walls]
  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
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
