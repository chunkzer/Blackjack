import Data.List
import Data.Maybe
import Text.Printf
import System.Random
import Control.Monad.Random

seeds :: Integer -> [Int] -> IO [Int]
seeds 0 seedArray = return seedArray
seeds n x = do
    seed <- getRandomR (1, 99999)
    seeds (n-1) (seed:x)

maxInd ::(Ord a) => [a] -> Integer
maxInd [] = error "this is bullshit"
maxInd array = maxim array (head array) 0 0

maxim :: (Ord a) => [a] -> a -> Integer -> Integer -> Integer
maxim [] currentMax index acc = index
maxim (x:xs) currentMax currentIndex acc
    | (x > currentMax) =  maxim xs x acc (acc+1)
    | otherwise = maxim xs currentMax currentIndex (acc+1)
    
epsilonGreedy ::  Float -> Int -> [Float] -> Integer
epsilonGreedy  epsilon seed actions 
    | (epsilon > randomNumber) =  (toInteger seed `mod` (toInteger $ length actions))
    | otherwise = maxInd actions
    where  generator = mkStdGen seed 
           randomNumber = fst (randomR (0.0, 1.0) generator)
            
