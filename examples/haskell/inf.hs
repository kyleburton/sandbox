import Data.Ratio

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

ints = 1 : [ succ ii | ii <- ints ]

facts = 1 : zipWith (*) [1 ..] facts

squares = [ ii*ii | ii <- [1 ..]]

powersOf2 = [ 2^ii | ii <- [0 ..]]

powersOfN nn = [ nn^ii | ii <- [0 ..]]

fractions = [ 1/ii | ii <- [1 ..]]
rfractions = [ 1 % ii | ii <- [1 ..]]


main = do
  putStrLn $ "Fibs: " ++ show (take 10 fibs)
  putStrLn $ "Ints: " ++ show (take 10 ints)
  putStrLn $ "Facts: " ++ show (take 10 facts)
  putStrLn $ "Squares: " ++ show (take 10 squares)
  putStrLn $ "Powers of 3: " ++ show (take 10 (powersOfN 3))
  putStrLn $ "Powers of 17: " ++ show (take 10 (powersOfN 17))
  putStrLn $ "Fractions: " ++ show (take 100 fractions)
  putStrLn $ "Fractions (ratio): " ++ show (take 100 rfractions)
