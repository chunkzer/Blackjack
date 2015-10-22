import System.Random
import Control.Monad.Random

module eeDilemma{
seeds,
epsilonGreedy
}where

seeds :: Integer -> [Integer] -> IO [Integer]
seeds 0 seedArray = return seedArray
seeds n x = do
    seed <- getRandomR (1, 999999999)
    seeds (n-1) (seed:x)

epsilonGreedy :: Integer -> Float -> Integer -> IO Integer
epsilonGreedy  seed epsilon numberOfActions = do
    randomNumber <- getRandomR (0.0, 1.0)
    if (randomNumber < epsilon) then return (seed `mod` numberOfActions)
    else return (-1)
