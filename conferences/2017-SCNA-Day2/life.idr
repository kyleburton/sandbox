module Main

iterate : List Bool -> List Bool
--iterate = id
iterate [] = []
iterate (_ :: a :: xs) = False :: a :: xs
iterate (_ :: xs) = False :: xs

testHasOneNeighbor : Bool
testHasOneNeighbor = head (iterate [True, False]) == False

testHasOneLiveNeighbor : Bool
testHasOneLiveNeighbor = head (iterate [True, True]) == True

-- testTwoNeighbors : Bool
-- dot is funcitonal composition (head . head is head(head))
-- testTwoNeighbors = (head . head)  (iterate [True, True, True]) == True

runTests : Bool
runTests = testHasOneNeighbor

main : IO ()
main = putStrLn $ show runTests

