module Stated
  ( executeS
  ) where

import           Control.Monad.State.Strict
import           Data.List                  (find)
import           Data.Maybe                 (fromJust)
import           System.Random

type RandomState a = State StdGen a

executeS :: IO ()
executeS = do
  gen <- getStdGen
  let result = evalState play1000Times gen
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

play1000Times :: RandomState [Game]
play1000Times = forM [0 .. 999] $ \_ -> play

play :: RandomState Game
play = do
    -- Step0: Put a car and goats behind the door
      carDoor <- randomDoor
      let step0 = Game {car = carDoor, firstChoice = Nothing, openedDoor = Nothing, secondChoice = Nothing}
    -- Step1: Player select the door
      selectedDoor <- randomDoor
      let step1 = step0 {firstChoice = Just selectedDoor}
    -- Step2: Open the door which has a goat
      let goatDoor = fromJust $ find (\x -> x /= car step1 && x /= fromJust (firstChoice step1)) doors
      let step2 = step1 {openedDoor = Just goatDoor}
    -- Step3: (Maybe) change the door
      let second = fromJust $ find (\x -> x /= fromJust (firstChoice step2) && x /= fromJust (openedDoor step2)) doors
      let step3 = step2 {secondChoice = Just second}
      return step3

randomDoor :: RandomState Door
randomDoor = do
  selected <- randomRSt (0, 2)
  return $ toEnum selected

randomRSt :: (Int, Int) -> RandomState Int
randomRSt range = state $ randomR range

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
