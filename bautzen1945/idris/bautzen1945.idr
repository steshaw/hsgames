module Main

import Bautzen.Options
import Bautzen.REPL
import Bautzen.Server
import Bautzen.Client
import System

%cg chez libidris_net.so

main : IO ()
main = do
  (_ :: args) <- getArgs
  case processOptions args of
    Left err => putStrLn err
    Right options@(MkOptions _ _ ServerMode) => do
      Right () <- server options
        | Left err => putStrLn err
      pure ()
    Right options@(MkOptions _ _ ClientMode) => do
      Right () <- client options
        | Left err => putStrLn err
      pure ()
