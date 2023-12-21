module Main (main) where

import Pipes
import qualified Pipes.Prelude as P

input :: Producer String IO ()
input = P.stdinLn >-> P.takeWhile (/= "quit")

name :: ListT IO String
name = do
    firstName <- Select input
    lastName <- Select input
    return (firstName ++ " " ++ lastName)

main :: IO ()
main = runEffect $ every name >-> P.stdoutLn
