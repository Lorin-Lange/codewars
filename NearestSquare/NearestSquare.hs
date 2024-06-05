----------------------------------------------------
--                    Codewars                    --
--           Find Nearest square number           --
--            Solution by Lorin Lange             --
----------------------------------------------------

module NearestSquare where 

nearestSquare :: Int -> Int
nearestSquare n =
  let lb = floor $ sqrt $ fromIntegral n
      ub = ceiling $ sqrt $ fromIntegral n
  in if (ub^2 - n) < (n - lb^2) then ub^2 else lb^2

nearestSquare' :: Int -> Int
nearestSquare' n = test [x*x | x <- [0..]]
  where test (x1:x2:xs) | x1 == n          = n
                        | x1 < n && x2 > n = if (n - x1) < (x2 - n) then x1 else x2
                        | otherwise        = test (x2:xs)
