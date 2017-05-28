module Lib where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Control.Concurrent.STM
import System.Random
import System.Exit
import Config
import Model

run :: IO ()
run = do
  g <- newStdGen
  initShared <- atomically $ newTVar (initWorld g)
  playIO display bgColor fps initShared drawShared handleShared updateShared
  where
    display = InWindow "Agar.hs" (screenWidth, screenHeight) (200, 200)
    bgColor = black
    fps = 60

    drawShared s = do
      w <- readTVarIO s
      return (drawWorld w)

    handleShared (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
    handleShared e s = atomically $ do
      w <- readTVar s
      writeTVar s (handleWorld e w)
      return s

    updateShared dt s = do
      atomically $ modifyTVar s (updateWorld dt)
      return s


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

initPlayerParts1 :: [PlayerPart]
initPlayerParts1 = [PlayerPart { playerMass = startMass
                              , playerRadius = radiusfromMass startMass
                              , playerSpeed = startSpeed
                              , playerPos = (100, 100)
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

initPlayer1 :: Player
initPlayer1 = Player 
  { playerID = 2
  , playerColor = green
  , playerTarget = (100, 100)
  , playerCenterMass = (100, 100)
  , playerParts = initPlayerParts1
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
  , players = initPlayer : initPlayer1 : []
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
updatePlayer dt p = p { playerParts = map (updatePlayerParts dt (playerTarget p)) (playerParts p)
                      , playerCenterMass = centerMass (playerParts p)
                      , timeFromSplit = max 0 ((timeFromSplit p) - dt)}
  
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
eatenByPlayerPart e p = forEat (playerPos p) (eatPos e) (playerRadius p)

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

checkTwoParts :: PlayerPart -> PlayerPart -> PlayerPart
checkTwoParts p1 p2
  | forEatPlayerPart p1 p2 = p1 {playerMass = newMass, playerRadius = newRadius, playerSpeed = newSpeed}
  | otherwise = p1
  where
    newMass = (playerMass p1) + (playerMass p2)
    newRadius = radiusfromMass newMass
    newSpeed = speedFromMass newMass

checkPlayersPart :: [PlayerPart] -> PlayerPart -> PlayerPart
checkPlayersPart ps p =  foldl checkTwoParts p ps

filterPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
filterPlayersParts ps1 [] = ps1
filterPlayersParts ps1 (p2 : ps2) = filterPlayersParts (filter (not . (forEatPlayerPart p2)) ps1) ps2

checkPlayersParts :: [PlayerPart] -> [PlayerPart] -> [PlayerPart]
checkPlayersParts ps1 ps2 = map (checkPlayersPart ps2) (filterPlayersParts ps1 ps2)

checkTwoPlayer :: Player -> Player -> Player
checkTwoPlayer p1 p2
  | playerID p1 == playerID p2 = p1
  | otherwise = p1{playerParts = checkPlayersParts (playerParts p1) (playerParts p2)}

checkPlayer :: [Player] -> Player -> Player
checkPlayer ps p = foldl checkTwoPlayer p ps

checkPlayers :: World -> World
checkPlayers w = w {players = map (checkPlayer (players w)) (players w)}

updateWorld :: Float -> World -> World
updateWorld dt w = checkPlayers (wasEaten (w 
  { eat = newEat
  , players = updatePlayers dt $ players w
  , nextEat = drop (eatCount - length (eat w)) (nextEat w)
  }))
  where
    newEat
      | length (eat w) < eatCount
        = take (eatCount - length (eat w)) (nextEat w) ++ (eat w)
      | otherwise = (eat w)

randomPoints :: StdGen -> [Point]
randomPoints g = zip (randomRs eatRangeX g1) (randomRs eatRangeY g2)
  where
    (g1, g2) = split g

forEat :: Point -> Point -> Float -> Bool
forEat p1 p2 r = (magV (p1 - p2)) < r

forEatPlayerPart :: PlayerPart -> PlayerPart -> Bool
forEatPlayerPart p1 p2 = forEat (playerPos p1) (playerPos p2) (playerRadius p1) && ((playerMass p1) / (playerMass p2) >= 1.25)
