module Main (main) where

main :: IO ()
main = do
  if 1 == 2
    then putStrLn "Nope."
    else putStrLn "Covered."
