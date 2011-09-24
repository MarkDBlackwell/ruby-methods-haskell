module Main where
doubleMe :: (Num a) => a -> a
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
 then x
  else x*2
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a| a <- xs, a <= x]
      biggerSorted  = quicksort [a| a <- xs, a >  x]
  in smallerSorted ++ [x] ++ biggerSorted
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
listOfFuns = map (*) [0..]
