module Normal
  ( execute
  ) where

import           Control.Monad.State.Strict
import           Data.List                  (find, groupBy)
import           Data.Maybe                 (fromJust)
import           System.Random

execute :: IO ()
execute = do
  gen <- getStdGen
  let result = play1000Times gen
  aggregate result

aggregate :: [Game] -> IO ()
aggregate games =
  let result =
        foldr
          (\x acc ->
             if fromJust (secondChoice x) == car x
               then (fst acc + 1, snd acc)
               else (fst acc, snd acc + 1))
          (0, 0)
          games
   in putStrLn $ "success: " ++ show (fst result) ++ ", failed: " ++ show (snd result)

play1000Times :: StdGen -> [Game]
play1000Times gen = go 1000 gen []
  where
    go :: Int -> StdGen -> [Game] -> [Game]
    go 0 gen result = result
    go count gen result =
      let (newGen, game) = play gen
       in go (count - 1) newGen (game : result)

play :: StdGen -> (StdGen, Game)
play gen =
  let
    -- Step0: Put a car and goats behind the door
      (newGen, carDoor) = randomDoor gen
      step0 = Game {car = carDoor, firstChoice = Nothing, openedDoor = Nothing, secondChoice = Nothing}
    -- Step1: Player select the door
      (newGen2, selectedDoor) = randomDoor newGen
      step1 = step0 {firstChoice = Just selectedDoor}
    -- Step2: Open the door which has a goat
      goatDoor = fromJust $ find (\x -> x /= car step1 && x /= fromJust (firstChoice step1)) doors
      step2 = step1 {openedDoor = Just goatDoor}
    -- Step3: (Maybe) change the door
      second = fromJust $ find (\x -> x /= fromJust (firstChoice step2) && x /= fromJust (openedDoor step2)) doors
      step3 = step2 {secondChoice = Just second}
   in (newGen, step3)

randomDoor :: StdGen -> (StdGen, Door)
randomDoor gen =
  let (selected, newGen) = randomR (0, 2) gen
   in (newGen, toEnum selected)

data Door
  = One
  | Two
  | Three
  deriving (Eq, Enum, Show)

-- get all doors
doors :: [Door]
doors = enumFrom One

data Game = Game
  { car          :: Door
  , firstChoice  :: Maybe Door
  , openedDoor   :: Maybe Door
  , secondChoice :: Maybe Door
  } deriving (Show)
