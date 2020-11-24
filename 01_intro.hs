import Debug.Trace

-- Testing out function definitions, and learning about the `otherwise` guard
test_func :: Int -> String
test_func n
  | n `mod` 2 == 0 = "Fizz"
  | otherwise  = "Buzz"

-- Lists are singly linked lists, Strings are lists of characters (as expected)
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- Cons operator appends to head, literal [] is syntactic sugar
isDefinitelyTrue = [2,3,4] == 2 : 3 : 4 : []

-- Cool sidenote, loading the same file again unloads the things that were loaded before
-- This is quite clever, and very nice. It's cool that things aren't left behind
-- Tested this by commenting out `helloSame` above, and after reloading it's not in scope
-- Presumably, undefined - not just out of scope

-- Pattern matching
-- We can pattern match head, or any number of elements deep into a list
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs


-- Exercises

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = splitDigits [n]

splitDigits :: [Integer] -> [Integer]
splitDigits [] = []
splitDigits (n:nums)
  | n >= 10 = splitDigits(div n 10 : mod n 10 : nums)
  | otherwise = n : nums

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2

-- First attempt
-- Not sure that reversing works at all with infinite sequences
-- The task definition to double from the right means this only works with finite lists
-- Reversing a singly linked list is expensive, gonna try again
doubleEveryOther1 :: [Integer] -> [Integer]
doubleEveryOther1 nums = reverse (doubleEveryOther1Reversed (reverse nums))

doubleEveryOther1Reversed :: [Integer] -> [Integer]
doubleEveryOther1Reversed [] = []
doubleEveryOther1Reversed (m:n:nums) = m : 2 * n : doubleEveryOther1Reversed nums
doubleEveryOther1Reversed (n:[]) = [n]

-- Second attempt
-- Looks more efficient that reversing then reversing again
-- Definitely need to deal with the difference between even and odd length lists
-- The task definition to double from the right means this only works with finite lists
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n:[]) = [n]
doubleEveryOther (m:n:nums)
  | odd (length nums) = m : doubleEveryOther (n: nums)
  | otherwise = 2 * m : n : doubleEveryOther nums

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits (nums) = sum (concat (map toDigits nums))

-- Exercise 4

validate :: Integer -> Bool
validate num = (calculateChecksum num == 0)

calculateChecksum num = (sumDigits (doubleEveryOther (toDigits num))) `mod` 10

-- Exercise 5

type PegLabel = String
type Move = (Peg, Peg)
type Peg = (PegLabel, [Disc])
type Disc = Integer

generateDiscs :: Integer -> [Integer]
generateDiscs numDiscs = take (fromIntegral numDiscs) [1..]

generatePegs :: [PegLabel] -> [Peg]
generatePegs pegLabels = map createPeg pegLabels

createPeg :: PegLabel -> Peg
createPeg pegLabel = (pegLabel, [])

addDiscsToPeg :: Peg -> [Disc] -> Peg
addDiscsToPeg peg discs = (fst peg, concat [discs, (snd peg)])

addDiscsToFirstPeg :: [Peg] -> [Disc] -> [Peg]
addDiscsToFirstPeg (p:pegs) discs = (addDiscsToPeg p discs) : pegs

--initializePegs :: Integer -> PegLabel -> PegLabel -> PegLabel -> [Peg]
--initializePegs numDiscs pegA pegB pegC =
initializePegs :: Integer -> [Peg]
initializePegs numDiscs =
  (addDiscsToFirstPeg
    (generatePegs ["a", "b", "c"])
    (generateDiscs numDiscs))

-- Need to get comfortable with maybes
-- The disc is not guaranteed to exist, but `head` explodes when
-- there are no elements. I'll wrap this one day to handle the edge
locateDisc :: Disc -> [Peg] -> Peg
locateDisc disc pegs = head (filter (\peg -> elem disc (snd peg)) pegs)

-- The peg is not guaranteed to exist, but `head` explodes when
-- there are no elements. I'll wrap this one day to handle the edge
locatePegByName :: PegLabel -> [Peg] -> Peg
locatePegByName pegLabel pegs = head (filter (\peg -> fst peg == pegLabel) pegs)

-- There is only one temp peg right now, but this will come in handy when
-- I go after the 4+ pegs scenario. Selecting pegs there will need some
-- intelligence that isn't required when there are only 3 pegs
tempPegs :: PegLabel -> PegLabel -> [Peg] -> [Peg]
tempPegs sourcePegLabel destinationPegLabel pegs =
  (filter (\peg -> fst peg /= sourcePegLabel && fst peg /= destinationPegLabel) pegs)

selectTempPeg :: Peg -> Peg -> [Peg] -> Peg
selectTempPeg sourcePeg destinationPeg pegs= head (tempPegs (fst sourcePeg) (fst destinationPeg) pegs)

moveDisc :: Disc -> PegLabel -> [Peg] -> [Peg]
moveDisc disc destinationPegLabel pegs =
  traceShow ("moveDisc", disc, sourcePegLabel, destinationPegLabel, pegs) map
  (\p -> if fst sourcePeg == fst p
           then (fst sourcePeg, tail (snd sourcePeg))
         else
           if fst destinationPeg == fst p
             then (fst destinationPeg, disc:(snd destinationPeg))
           else p
  )
  pegs
  where sourcePeg = locateDisc disc pegs
        sourcePegLabel = fst sourcePeg
        destinationPeg = locatePegByName destinationPegLabel pegs
        tempPeg = head (tempPegs (fst sourcePeg) (fst destinationPeg) pegs)

-- Learned about where clauses. Those are cool for predefining things that
-- require some computation. Saves some wasted effort, and allow use within
-- guard clauses
hanoiPerform :: Disc -> PegLabel -> [Peg] -> [Peg]
hanoiPerform disc destinationPegLabel pegs
    | disc == 1 =
      moveDisc disc destinationPegLabel pegs
    | otherwise =
      hanoiPerform (disc - 1) destinationPegLabel (
        moveDisc disc destinationPegLabel (
          hanoiPerform (disc - 1) tempPegLabel pegs
        )
      )
  where sourcePeg = locateDisc disc pegs
        destinationPeg = locatePegByName destinationPegLabel pegs
        tempPegLabel = fst (selectTempPeg sourcePeg destinationPeg pegs)

hanoi :: Integer -> [Peg]
hanoi numDiscs =
  (hanoiPerform numDiscs destinationPegLabel pegs)
  where pegs = initializePegs numDiscs
        destinationPegLabel = (fst (last pegs))

demo = hanoi 3
