{-
Author: Mark D. Blackwell
Change dates:
(mdb) September 23, 2011 - create
(mdb) September 27, 2011 - add zero and negative to eachCons

Haskell version: 2010.2.0.0

Because I didn't see it in the Haskell library, I implemented Ruby's method, Enumerable#each_cons. (The people on IRC #haskell didn't think it was there, either.)

Ruby Enumerable#each_cons
For:
  eachCons 4 [1,2,3,4,5,6] would be [[1,2,3,4],[2,3,4,5],[3,4,5,6]]

Thanks to Stefan Ljungstrand (ski on #haskell) most for helping me to appreciate this next concept:

Good if eachCons is consistent with zero and negative values of 'each consecutive', fitting into a pattern. (See the test.)

Other suggestions:

From IRC #haskell on 9/26/2011, I received suggestions from:
geheimdienst:
  map (take 4) $ tails [1..6]
Cale <~Cale@CPE00026f8481b6-CM00222d55727d.cpe.net.cable.rogers.com> “Cale Gibbard”:
  (zipWith const <*> drop 4) . map (take 4) . tails $ [1..10]
DanBurton:
  foo2 n xs = take (length xs - n + 1) . map (take n) . tails $ xs
JoeyA:
  eachCons n = takeWhile ((== n) . length) . map (take n) . tails
ski <~slj@c83-254-21-112.bredband.comhem.se> “Stefan Ljungstrand”:
  n < 0 = error ("eachCons _ " ++ showsPrec 11 n "")
-}

module RubyMethods.EachCons (eachCons, testEachCons)
  where

  import Data.List (transpose)

  eachCons :: Int -> [a] -> [[a]]
  eachCons n x
    | n  < 0 = reversed
    | n == 0 = []: [ [] | _ <- x] -- Add one, to fit the pattern.
    | n  > 0 = consecutives
    where
      reversed  = map reverse $ eachCons (-n) x
      sequences = map (`drop` x) [0..(n-1)]
      consecutives = [y | y <- transpose sequences, n==length y]

  testEachCons = all (==True)
    [ finiteZero,     infiniteZero
    , finitePositive, infinitePositive
    , finiteNegative, infiniteNegative
    , pattern
    ]
    where
    infiniteMap, finiteMap :: [Int] -> [[[Int]]]
    infiniteMap = map (`eachCons` infiniteExample)
    finiteMap   = map (`eachCons`   finiteExample)

    infiniteExample = [examplesLowest..]
    finiteExample   = [examplesLowest..finiteExampleHighest]

    positive = [positiveHighest,(pred positiveHighest)..examplesLowest]
    negative = map negate positive

    examplesLowest = 1
    finiteExampleHighest = 3
    positiveHighest = 4
    shortLength = 2
    zeroLength = 0

    infiniteZero =
      (take shortLength $ eachCons zeroLength infiniteExample)  
      ==
      (take shortLength $ repeat [])
    finiteZero =
      (eachCons zeroLength finiteExample)  
      ==
      (take (succ $ length finiteExample) $ repeat [])

    infinitePositive = (map (take shortLength) $ infiniteMap positive)==
      [ [[1,2,3,4],[2,3,4,5]], [[1,2,3],[2,3,4]], [[1,2],[2,3]], [[1],[2]] ]
    infiniteNegative = (map (take shortLength) $ infiniteMap negative)==
      [ [[4,3,2,1],[5,4,3,2]], [[3,2,1],[4,3,2]], [[2,1],[3,2]], [[1],[2]] ]

    finitePositive = (finiteMap positive)==
      [ [], [[1,2,3]], [[1,2],[2,3]], [[1],[2],[3]] ]
    finiteNegative = (finiteMap negative)==
      [ [], [[3,2,1]], [[2,1],[3,2]], [[1],[2],[3]] ]

    pattern = (finiteMap $ negative ++ [0] ++ reverse positive)==
      [ [], [[3,2,1]], [[2,1],[3,2]], [[1],[2],[3]], [[],[],[],[]], [[1],[2],[3]], [[1,2],[2,3]], [[1,2,3]], [] ]

