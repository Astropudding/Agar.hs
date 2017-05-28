module Lib where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Control.Concurrent.STM
import System.Random
import System.Exit
import Config
import Model

run :: IO ()
run = do
  g <- newStdGen
  play display bgColor fps (initWorld g) drawWorld handleWorld updateWorld
  where
    display = InWindow "Agar.hs" (screenWidth, screenHeight) (200, 200)
    bgColor = black
    fps = 60

emptyWorld :: StdGen -> World
emptyWorld g = World 
  { playID = 0
  , eat = []
  , players = []
  , nextEat = initEat g
  }

initPlayerParts :: [PlayerPart]
initPlayerParts = [PlayerPart { playerMass = startMass
                              , playerRadius = radiusfromMass startMass
                              , playerSpeed = startSpeed
                              , playerPos = (0, 0)
                              , playerDirection = 0.0
                              , timeForSpeed = 0.0 }]

initPlayer :: Player
initPlayer = Player 
  { playerID = 1
  , playerColor = green
  , playerTarget = (0, 0)
  , playerCenterMass = (0, 0)
  , playerParts = initPlayerParts
  , timeFromSplit = 0.0
  }
  

initOneEat :: Point -> Eat
initOneEat p = Eat
  { eatPos = p
  , eatColor = red
  , eatMass = 75.0 
  , eatRadius = radiusfromMass 75.0
  } 

initEat :: StdGen -> [Eat]
initEat g = map initOneEat (randomPoints g)

initWorld :: StdGen -> World
initWorld g = (emptyWorld g)
  { playID = 1
  , players = initPlayer : []
  }

centerMass :: [PlayerPart] -> Point
centerMass xs = sum (map g xs)
  where
    m = sum (map playerMass xs)
    g x = mulSV (playerMass x / m) (playerPos x)

radiusfromMass :: Float -> Float
radiusfromMass m = sqrt (m / pi)

speedFromMass :: Float -> Float
speedFromMass m = startSpeed * startMass / m

drawPlayerPart :: PlayerPart -> Picture
drawPlayerPart p = uncurry translate (playerPos p) (circleSolid (playerRadius p))

drawPlayer :: Player -> Picture
drawPlayer p = color (playerColor p) (pictures (map drawPlayerPart (playerParts p)))

drawEat :: Eat -> Picture
drawEat p = color (eatColor p) (uncurry translate (eatPos p) (circleSolid (eatRadius p)))

drawWorld :: World -> Picture
drawWorld w = pictures
  [ pictures (map drawPlayer (players w))
  , pictures (map drawEat (eat w))
  ]

movePlayer :: Point -> Player -> Player
movePlayer m p = p
  { playerTarget = m
  , playerParts = map (\x -> x {playerDirection = argV (m - (playerPos x))}) (playerParts p)  
  }

movePlayers :: Int -> Point -> World -> World
movePlayers i m w = w 
  { players = map (\x -> if ((playerID x) == i) then (movePlayer m x) else x) (players w)}

splitPlayerPart :: PlayerPart -> [PlayerPart]
splitPlayerPart p = [ p{ playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
                    , p{ playerMass = newMass, playerRadius = newRadius, timeForSpeed = 1, playerSpeed = newSpeed}]
    where
      newMass = (playerMass p) / 2
      newRadius = radiusfromMass newMass
      newSpeed = speedFromMass newMass

splitPlayer :: Player -> Player
splitPlayer p = p 
  { playerParts = foldl (\x y -> (x ++ splitPlayerPart y)) [] (playerParts p)
  , timeFromSplit = 10} 

splitPlayers :: Int -> World -> World
splitPlayers i w = w
  {players = map (\x -> if ((playerID x) == i) then (splitPlayer x) else x) (players w)}

handleWorld :: Event -> World -> World
handleWorld (EventMotion mouse) w = movePlayers (playID w) mouse w
handleWorld (EventKey (SpecialKey KeySpace) Down _ _) w = splitPlayers (playID w) w 
handleWorld _ w = w

updatePlayerParts :: Float -> Point -> PlayerPart -> PlayerPart
updatePlayerParts dt m p  = p { playerPos = (playerPos p) + motion, timeForSpeed = max 0 $ (timeForSpeed p) - dt}
  where
    playerSpeedSum = (playerSpeed p) + (playerSpeed p) * (timeForSpeed p)
    motion
      | magV (playerPos p - m) < (playerSpeed p) = (0, 0)
      | otherwise = (mulSV (60 * dt * (playerSpeedSum)) (cos $ playerDirection p, sin $ playerDirection p))

updatePlayer :: Float -> Player -> Player
updatePlayer dt p = p { playerParts = map (updatePlayerParts dt (playerTarget p)) (playerParts p), playerCenterMass = centerMass (playerParts p)}
  
updatePlayers :: Float -> [Player] -> [Player]
updatePlayers dt = map (updatePlayer dt)

playerPartEaten :: [Eat] -> PlayerPart -> PlayerPart
playerPartEaten e p = p 
  { playerMass =  newMass
  , playerRadius = radiusfromMass newMass
  , playerSpeed = speedFromMass newMass
  }
  where
    newMass = (playerMass p) + foldl (\x y -> x + (if (eatenByPlayerPart y p) then (eatMass y) else 0)) 0 e

playerEaten :: Player -> [Eat] -> Player
playerEaten p e = p {playerParts = map (playerPartEaten e) (playerParts p)}

playersEaten :: [Player] -> [Eat] -> [Player]
playersEaten [] _ = []
playersEaten (p:ps) e = playerEaten p e : playersEaten ps e

eatenByPlayerPart :: Eat -> PlayerPart -> Bool
eatenByPlayerPart e p
    | ((magV ((playerPos p) - (eatPos e))) < (playerRadius p)) = True
    | otherwise = False

eatenByPlayer :: Eat -> Player -> Bool
eatenByPlayer e p = any (eatenByPlayerPart e) (playerParts p)
    
eatenByPlayers :: [Player] -> Eat -> Bool
eatenByPlayers p e = any (eatenByPlayer e) p

wasEaten :: World -> World
wasEaten w = w 
  { eat = newE 
  , players = newP
  }
  where
    newE = filter (not . eatenByPlayers (players w)) (eat w)
    newP = playersEaten (players w) (eat w)

updateWorld :: Float -> World -> World
updateWorld dt w = wasEaten (w 
  { eat = newEat
  , players = updatePlayers dt $ players w
  , nextEat = drop (eatCount - length (eat w)) (nextEat w)
  })
  where
    newEat
      | length (eat w) < eatCount
        = take (eatCount - length (eat w)) (nextEat w) ++ (eat w)
      | otherwise = (eat w)

randomPoints :: StdGen -> [Point]
randomPoints g = zip (randomRs eatRangeX g1) (randomRs eatRangeY g2)
  where
    (g1, g2) = split g
