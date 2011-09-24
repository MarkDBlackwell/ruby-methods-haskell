module Main where

questionA = "What's your OS?"
optionA :: (Integral a) => a -> String
optionAs = [0,1,2,3]
optionA 0 = "Vista"
optionA 1 = "Windows XP"
optionA 2 = "Linux"
optionA 3 = "Puppy"
loopChoiceA = do
  line <- getLine
  when (line == "") $ do
--    sequence (map print  !! optionAs 
    print optionA !! 0
    print optionA !! 1
    print optionA !! 2
    print optionA !! 3
    loopChoiceA
    

getChoiceA = do
  putStrLn questionA
  loopChoiceA

  if null line
      then do
        
    ChoiceA <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")


optionBs = [0,1,2]
optionB :: (Integral a) => a -> String
optionB 0 = "Can't log in"
optionB 1 = "Computer too slow"
optionB 2 = "Popups keep happening"
choiceB x = [ x | x <- optionBs ]

optionCs = [1,2]
optionC :: (Integral a) => a -> String
optionC 1 = "Funny sound"
optionC 2 = "Funny screen colors"
choiceC = [ x | x <- optionCs, x == 1 ]

actions = [1,2,3,4,5]
action :: (Integral a) => a -> String
action 1 = "I shall run Combofix"
action 2 = "Run Malwarebytes"
action 3 = "Run Windows Update"
action 4 = "Run Disk Clean"
action 5 = "Sorry; can't help you"

-- fixComputer :: String

state = [choiceA, choiceB, choiceC]
-- fixComputer [ action x | x <- actions, x==1, state==[1,1,1] ]
-- [choiceA, choiceB, choiceC] == [1,1,1] ]
-- fixComputer = [ action 2 | [choiceA, choiceB, choiceC] == [1,1,2] ]
-- fixComputer :: action

-- fixComputer = [ action x | x <- actions, x <- [1], state==[1,1,1] ]
fixComputer = [ action x | x <- actions, x <- [1] ]

