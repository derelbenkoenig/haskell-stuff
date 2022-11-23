module ContThings where

import Control.Monad.Cont

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foo :: Cont r a
foo = undefined
