-- \* Entry point for a Haskell application
-- Typically, this file is short and most code is part of a reusable library
-- module Main where

-- -- >>> someDecl

module Main where

import Command (Command (..), CommandError (..), commandHandler)
import CommandParser (parseInput)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)

commands :: [Command]
commands = [] -- define the commands later

main :: IO ()
main = do
  input <- getLine
  result <- runExceptT $ do
    -- Parse the input
    cmd <- except $ parseInput commands input
    -- Handle the command
    output <- except $ commandHandler cmd
    -- Print the output if it is not empty
    unless (null output) $ liftIO $ putStrLn output
  -- Handle errors
  case result of
    Left (CommandError errMsg) -> putStrLn $ "Error: " ++ errMsg
    Right () -> return ()

