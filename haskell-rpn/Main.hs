module Main (main) where

import RPN

mainLoop :: [Float] -> IO ()
mainLoop stack = do
    putStrLn "Give me an expression: "
    l <- getLine
    case l of "q" -> return ()
              "c" -> mainLoop []
              l   -> do
                  let newStack = rpn (words l) stack
                  putStrLn $ show newStack
                  mainLoop newStack

main = mainLoop []
