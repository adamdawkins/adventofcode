module Day1 where

calculateFuel :: Int -> Int
calculateFuel mass = (floor( fromIntegral mass / 3)) - 2
