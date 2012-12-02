import Char

intListToString l = [ chr x | x <- l ]

main = do
  putStrLn $ "the string: " ++ (intListToString [72,73,74,75])

