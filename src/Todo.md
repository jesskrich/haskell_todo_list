module Main where


import Control.Concurrent(threadDelay)
import Control.Monad(forM_)
import System.Console.ANSI


{- COMMENT
dsdfd-}
-- import Todo
-- main :: IO ()
load :: IO ()
load = do
    -- replicate 4 (".")
    threadDelay (100000)
    putStr "."
    threadDelay (100000)
    putStr "."
    threadDelay (100000)
    putStr "."

foo :: IO ()
foo = forM_ [1..5] $ \i ->
         putStr (".") >>
         threadDelay (100000)

showMainMenuOptions :: IO ()
showMainMenuOptions = do
    putStrLn ""
    putStrLn "What would you like to do?"
    putStrLn ""
    putStrLn "OPTIONS"
    putStrLn ""
    putStrLn "'+' : add items  🖌  |   '-' : remove items ⛔️"
    putStrLn "'&' : edit items ✂️  |   'x' : mark items complete ✅"


showSubMenuOptions ::  [String] -> String -> IO ()
showSubMenuOptions todos operation = do
    putStrLn ""
    if operation == "add"
        then do putStrLn ("Type in the TASK you'd like to " ++ operation ++ ", then hit ENTER")
                -- addListItem todos
    else do
        putStrLn ("Type in the NUMBER of the task you'd like to " ++ operation ++ ", then hit ENTER")
        putStrLn "('r' : return to the main menu)"

prompt :: [String] -> String -> IO () -> IO ()
prompt todos prevInput showOptions = do
    showTasks todos
    showOptions
    putStrLn prevInput
    input <- getLine
    interpret input todos prevInput


interpret :: String -> [String] -> String -> IO ()
interpret input todos prevInput
    | input == "r" = prompt todos prevInput showMainMenuOptions
    | input == "+" = showSubMenuOptions todos "add"
    | input == "-" = showSubMenuOptions todos "remove"
    | input == "&" = showSubMenuOptions todos "edit"
    | input == "x" = showSubMenuOptions todos "mark as completed"
    | otherwise = do
                    putStrLn ""
                    putStrLn "SORRY, did not get that! Please enter a valid option."
                    prompt todos prevInput showMainMenuOptions

task :: (Int, String) -> IO ()
task (n, item) = putStrLn (show n ++ "" ++ item)

listHasItems :: [String] -> Bool
listHasItems todos
    | length todos > 0 = True
    | otherwise = False

showTasks :: [String] -> IO ()
showTasks todos = do
    putStrLn ""
    putStrLn "MY LIST 📝"
    if listHasItems todos == True
        then mapM_ task (zip [1..] todos)
    else
        putStrLn "(empty)"

-- addListItem :: [String] -> IO ()
-- addListItem todos = do
--     showTasks todos
--     item <- getLine
--     if item == "r"
--         then prompt todos showMainMenuOptions
--     else do addListItem(todos ++ [" [ ] " ++ item])
--     putStrLn ""

-- handleInput :: [String] -> String -> IO ()
-- handleInput todos operator
--     showTasks todos
--     putStrLn ""
--     putStrLn "Type in the task you'd like to " ++ operator ++ ", then hit ENTER"
--     putStrLn "('r' : return to the main menu)"
--     if input == "r"
--         then prompt todos
--     else do interpret
--     putStrLn ""

validateItem :: [String] -> String -> IO ()
validateItem todos item
    | (x > 0 && x <= length todos) = putStrLn "valid entry"
    | otherwise = putStrLn "Invalid entry! Please enter a valid task number."
    where x = read item :: Int

-- removeListItem :: [String] -> IO ()
-- removeListItem todos = do
--     showTasks todos
--     putStrLn ""
--     if listHasItems todos == False
--         then putStrLn "No items to delete!" >>
--              putStrLn "('r' : return to the main menu )"
--     else do
--         putStrLn "Type in the task you'd like to remove, then hit ENTER"
--         putStrLn "('r' : return to the main menu )"
--         item <- getLine
--         if item == "r"
--             then prompt todos showMainMenuOptions
--         else do validateItem todos item

main :: IO ()
main = do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "                _ _                  "
    putStrLn "|_| _. _|  _ ||  | _ | \' _  | o __|_"
    putStrLn "| |(_|_>|<(/_||  |(_)|_/(_) |_|_> |_"
    setSGR [Reset]
    prompt [] "test" showMainMenuOptions
    -- addListItem []
    -- testFunc "this is a test"
  -- putStrLn "Welcome to Haskell To-do List"
  -- name <- getLine
  -- putStrLn ("Hello " ++ name)
