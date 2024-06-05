----------------------------------------------------
--                    Codewars                    --
--           Find Nearest square number           --
--            Solution by Lorin Lange             --
----------------------------------------------------

module NearestSquare where 

nearestSquare :: Int -> Int
nearestSquare n = test [x*x | x <- [0..]]
  where test (x1:x2:xs) | x1 == n          = n
                        | x1 < n && x2 > n = if (n - x1) < (x2 - n) then x1 else x2
                        | otherwise        = test (x2:xs)