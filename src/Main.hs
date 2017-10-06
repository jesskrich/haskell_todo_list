module Main where


import Control.Concurrent(threadDelay)
import Control.Monad(forM_)
import System.Console.ANSI


mainMenuOptions :: IO ()
mainMenuOptions = do
    putStrLn ""
    putStrLn "What would you like to do?"
    putStrLn ""
    putStrLn "OPTIONS"
    putStrLn ""
    putStrLn "'+' : add items  🖌  |   '-' : remove items ⛔️"
    putStrLn "'&' : edit items ✂️  |   'x' : mark items complete ✅"


prompt :: [String] -> IO () -> Bool -> IO ()
prompt todos showOptions operatable = do
    showTaskList todos
    showOptions
    input <- getLine
    if operatable == True
        then operate input todos operatable
    else
         navigate input todos operatable


navigate :: String -> [String] -> Bool -> IO ()
navigate input todos operatable
    | input == "r" = prompt todos mainMenuOptions operatable
    | input == "+" = prompt todos (putStrLn "Enter the TASK you'd like to ADD") True
    | otherwise = do
                    showCustomErrMsg
                    prompt todos mainMenuOptions operatable

operate :: String -> [String] -> Bool -> IO ()
operate input todos operatable
    | input == "r" = prompt todos mainMenuOptions operatable
    | otherwise = prompt (todos ++ [" 👉  " ++ input]) mainMenuOptions False

showCustomErrMsg :: IO ()
showCustomErrMsg = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "SORRY, did not get that! Please enter a valid option."
    setSGR [Reset]

task :: (Int, String) -> IO ()
task (n, item) = putStrLn (show n ++ "" ++ item)


showTaskList :: [String] -> IO ()
showTaskList todos = do
    putStrLn ""
    putStrLn "MY LIST 📝"
    mapM_ task (zip [1..] todos)

colorizeText :: [String] -> IO ()
colorizeText strings = do
    setSGR [SetColor Foreground Vivid Blue]
        mapM_ putStrLn strings
    setSGR [Reset]

main :: IO ()
main = do
    colorizeText ["                _ _                  ", "|_| _. _|  _ ||  | _ | \' _  | o __|_", "| |(_|_>|<(/_||  |(_)|_/(_) |_|_> |_" ]
    prompt [] mainMenuOptions False

-- YOOOOOOOO
