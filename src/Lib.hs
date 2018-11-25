{-
  This file is part of RPCalc.

  fct is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  fct is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with fct.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2018 Zachary Young
  -}

{-# LANGUAGE LambdaCase #-}

module Lib (mainIO) where

import Control.Concurrent.STM (TVar, readTVarIO, atomically, modifyTVar', writeTVar, newTVarIO)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

type App = ReaderT Env IO ()
type Silence = Bool

data Command = Command UTCTime Double String
  deriving (Show, Read)

data Env = Env { envCommands :: TVar [Command]
               , envSilent :: TVar Silence

               }

data Calc t = Operand t | Operator (t -> t -> t)
 
configFile :: String
configFile = "config.rc"

prettyTime :: UTCTime -> String
prettyTime = formatTime defaultTimeLocale "%F-%R"

prettyCommand :: Command -> String
prettyCommand ( Command t y c ) = " |" ++ prettyTime t ++ "\t| " ++ show y ++ "\t| " ++ c

printCommands :: App
printCommands = ask
  >>= \env ->
  liftIO
    -- $   putStrLn "  |date\t\t\t| eval\t\t| command"
    $   readTVarIO (envCommands env)
    >>= mapM_ (\( n, c ) -> putStrLn $ (show n) ++ prettyCommand c)
        . zip ( [1..] :: [Int] )

execute :: Command -> App
execute cmd = ask
  >>= \env -> 
  liftIO
    $ atomically
    . modifyTVar' (envCommands env) $ ((++) [cmd])

clear :: App
clear = ask
  >>= \env ->
  liftIO
    $ atomically
    $ writeTVar (envCommands env) []

toggleSilent :: App
toggleSilent = ask
  >>= \env ->
  liftIO
    $   readTVarIO (envSilent env)
    >>= \isSilent ->
    liftIO
      $ atomically
      $ writeTVar (envSilent env) (not isSilent)

saveConfig :: App
saveConfig = ask
  >>= \env ->
  liftIO
    $   putStrLn "Writing Configuration"
    >>  readTVarIO (envCommands env)
    >>= writeFile configFile
      . show
  
deleteCommand :: Int -> App
deleteCommand num = ask
  >>= \env ->
  liftIO
    $   readTVarIO (envCommands env)
    >>= \cmds ->
    liftIO
      $ atomically
      $ writeTVar (envCommands env) ((take (num-1) cmds) ++ (drop num cmds))
  
touchConfig :: IO ()
touchConfig = appendFile configFile ""
  
readConfig :: IO [Command]
readConfig =
  readFile configFile
  >>= \fileStr ->
  return
    $ fromMaybe [] (readMaybe fileStr :: Maybe [Command])

getInt :: IO Int
getInt =
  getLine
  >>= return
    . (fromMaybe 0 :: Maybe Int -> Int)
    . (readMaybe :: String -> Maybe Int)

toCalc :: String -> Calc Double
toCalc "+" = Operator (+)
toCalc "-" = Operator (-)
toCalc "*" = Operator (*)
toCalc "/" = Operator (/)
toCalc n   = Operand (read n)
 
eval :: [Double] -> Calc Double -> [Double]
eval acc (Operand n)         = n : acc
eval (a:b:acc) (Operator op) = (op b a) : acc
 
calc :: String -> Double
calc = head . foldl eval [0] . map toCalc . words

mainLoop :: Env -> IO ()
mainLoop env = do
  let run = (flip runReaderT) env

  now <- getCurrentTime
  isSilent <- readTVarIO ( envSilent env )
  putStr $ if isSilent then "[q]uit, [p]rint, [e]xecute, [d]elete [c]lear, [r]eplay, [w]rite, [s]ilent> " else "> "

  ln <- getLine
  case ln of
    "q" -> print "quitting..."
           >> return ()

    "p" -> run printCommands
           >> mainLoop env

    "s" -> run toggleSilent
           >> mainLoop env

    "e" -> putStr "execute> "
           >>  getLine
           >>= \case
             ""      -> mainLoop env
             command -> run (execute $ Command now (calc $ command) command) 
                        >> mainLoop env

    "c" -> run clear
           >> mainLoop env

    "r" -> readTVarIO (envCommands env)
           >>= \case
             []    -> print "Command History is Empty"
             cmds  -> run printCommands
                      >>  putStr "command #> "
                      >>  getInt
                      >>= \case
                            0 -> putStrLn "Not a Choice"
                            num -> if   num `elem` (take (length cmds) ([1..] :: [Int]))
                                   then putStrLn $ "executing: " ++ prettyCommand ( head . drop ( num - 1 ) $ cmds )
                                   else putStrLn "Not a choice"
           >> mainLoop env

    "d" -> readTVarIO (envCommands env)
           >>= \case
             []    -> print "Command History is Empty"
             cmds  -> run printCommands
                      >>  putStr "command #> "
                      >>  getInt
                      >>= \case
                            0 -> putStrLn "Not a Choice"
                            num -> if   num `elem` (take (length cmds) ([1..] :: [Int]))
                                   then run (deleteCommand num)
                                   else putStrLn "Not a choice"
           >> mainLoop env

    "w" -> run saveConfig
           >> mainLoop env
             
    _   -> mainLoop env

mainIO :: IO ()
mainIO = hSetBuffering stdout NoBuffering
  >>  touchConfig
  >>  readConfig
  >>= (newTVarIO :: [Command] -> IO (TVar [Command]))
  >>= \cmdsTVar  ->
  newTVarIO (True :: Silence)
    >>= mainLoop . (Env cmdsTVar)
