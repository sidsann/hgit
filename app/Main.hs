module Main where

import CommandHandler (commandHandler, commands)
import CommandParser (Command (..), CommandError (..), parseInput)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getArgs
  result <- runExceptT $ do
    -- Parse the input
    cmd <- except $ parseInput commands input
    -- Handle the command
    cmdResult <- liftIO $ commandHandler cmd
    output <- except cmdResult
    -- Print the output if it is not empty
    unless (null output) $ liftIO $ putStrLn output
  -- Handle errors
  case result of
    Left (CommandError errMsg) -> putStrLn $ "[ERROR] " ++ errMsg
    Right () -> return ()
