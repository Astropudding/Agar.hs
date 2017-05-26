module Lib where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

run :: IO ()
run = do
  g <- newStdGen
  play display bgColor fps (initWorld g) drawWorld handleWorld updateWorld
  where
    display = InWindow "String" (1000, 800) (200, 200)
    bgColor = black
    fps = 60

data Eat = Eat
  { eatPos      :: Point
  , eatColor    :: Color
  , eatRadius   :: Float
  }

data Player = Player
  { playerPos  :: Point
  , playerColor     :: Color
  , playerDirection :: Vector
  , playerRadius    :: Float
  }

data World = World
  { eat     :: [Eat]
  , players :: [Player]
  }

emptyWorld :: World
emptyWorld = World 
  { eat = []
  , players = []
  }

initPlayer :: Player
initPlayer = Player 
  { playerPos = (0, 0)
  , playerColor = green
  , playerDirection = (0, 0)
  , playerRadius = 1.0
  }

initEat :: [Eat]
initEat = Eat{eatPos = (10, 10), eatColor = red, eatRadius = 1.0} : Eat{eatPos = (-10, -10), eatColor = red, eatRadius = 1.0} : [] 

initWorld :: StdGen -> World
initWorld _ = emptyWorld
  { eat = initEat
  , players = initPlayer : [] 
  }

drawPlayer :: Player -> Picture
drawPlayer p = color (playerColor p) (uncurry translate (playerPos p) (circleSolid (playerRadius p)))

drawPlayers :: [Player] -> [Picture]
drawPlayers = map drawPlayer

drawEat :: Eat -> Picture
drawEat p = color (eatColor p) (uncurry translate (eatPos p) (circle (eatRadius p)))

drawWorld :: World -> Picture
drawWorld w = pictures
  [ pictures (drawPlayers (players w))
  , pictures (map drawEat (eat w))
  ]

handleWorld :: Event -> World -> World
handleWorld e w = w

updateWorld :: Float -> World -> World
updateWorld f w = w