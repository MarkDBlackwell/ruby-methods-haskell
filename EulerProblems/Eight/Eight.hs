{-
Author: Mark D. Blackwell
Change dates:
(mdb) September 23, 2011 - created

My first Haskell program.
Haskell version: 2010.2.0.0

Euler's problem #8
Found at: http://fedner.net/blog/2010/04/06/euler-problem-8/

Find the greatest product of five consecutive digits in the 1000-digit number.
(The large number, below, was copied from the above website.)

Because I don't see them in the Haskell library, I implemented various of Ruby's methods.
-}

module EulerProblems.Eight (result)
  where

  import Data.Char (digitToInt,isDigit)
  import Data.List

-- Ruby Enumerable#each_cons

  eachCons :: [a] -> Int -> [[a]]
  eachCons x n
    | n <= 0 = [[]]
    | otherwise = consecutives
    where
      sequences = map (`drop` x) [0..(n-1)]
      isFullLength :: [a] -> Bool
      isFullLength y = n==length y
      consecutives = filter (isFullLength) $ transpose sequences

  findGreatestProduct :: String -> Int -> (Int, [Int], [[Int]])
  findGreatestProduct string nConsecutiveDigits = (maxProduct, indices, maxDigits)
    where
    digits = map digitToInt string
    digitRuns = eachCons digits nConsecutiveDigits
    products = map product digitRuns
    maxProduct = maximum products
    indices = findIndices (==maxProduct) products
    maxDigits = map (digitRuns !!) indices

  cleaner :: String -> String
  cleaner = filter isDigit

  result :: (Int, [Int], [[Int]])
  result = findGreatestProduct bigString nConsecutiveDigits
    where
    nConsecutiveDigits = 5
    bigString = cleaner input

    input = input0 ++ input1 ++ input2 ++ input3 ++ input4 ++ input5 ++ input6 ++ input7 ++ input8 ++ input9 ++ input10 ++ input11 ++ input12 ++ input13 ++ input14 ++ input15 ++ input16 ++ input17 ++ input18 ++ input19

    input0  = "    73167176531330624919225119674426574742355349194934"
    input1  = "    96983520312774506326239578318016984801869478851843"
    input2  = "    85861560789112949495459501737958331952853208805511"
    input3  = "    12540698747158523863050715693290963295227443043557"
    input4  = "    66896648950445244523161731856403098711121722383113"
    input5  = "    62229893423380308135336276614282806444486645238749"
    input6  = "    30358907296290491560440772390713810515859307960866"
    input7  = "    70172427121883998797908792274921901699720888093776"
    input8  = "    65727333001053367881220235421809751254540594752243"
    input9  = "    52584907711670556013604839586446706324415722155397"
    input10 = "    53697817977846174064955149290862569321978468622482"
    input11 = "    83972241375657056057490261407972968652414535100474"
    input12 = "    82166370484403199890008895243450658541227588666881"
    input13 = "    16427171479924442928230863465674813919123162824586"
    input14 = "    17866458359124566529476545682848912883142607690042"
    input15 = "    24219022671055626321111109370544217506941658960408"
    input16 = "    07198403850962455444362981230987879927244284909188"
    input17 = "    84580156166097919133875499200524063689912560717606"
    input18 = "    05886116467109405077541002256983155200055935729725"
    input19 = "    71636269561882670428252483600823257530420752963450"
