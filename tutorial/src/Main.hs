{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
    
-- import Foo

eval :: String -> IO String
eval s = return $ "seq " <> s <> " (return ())"

main :: IO ()
main = do
  putStrLn "hello world"

--
-- example to show laziness
-- 

primes :: [Integer]
primes = sieve [2..] where
    sieve ~(x:xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)

primes2 :: [Integer]
primes2 = let sieve ~(x:xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)
           in sieve [2..]

--
-- example to show data types and pattern matching
-- 

data Rectangle = Rectangle Double Double | Square Double

instance Eq Rectangle where
    Square s1 == Square s2 = s1 == s2
    Rectangle l1 w1 == Rectangle l2 w2 = l1 == l2 && w1 == w2
    Rectangle l w == Square s = s == l && s == w
    Square s == Rectangle l w = s == l && s == w

area :: Rectangle -> Double
area (Rectangle len wid) = len * wid
area (Square side) = side ^ 2

area2 :: Rectangle -> Double
area2 rect = case rect of
            Rectangle l w -> l * w
            Square s      -> s ^ 2

-- requires LambdaCase extension
area3 :: Rectangle -> Double
area3 = \case
    Rectangle l w -> l * w
    Square s -> s ^ 2

--
-- example to show recursive data, and functor
-- 

-- you can think of fmap as taking the function and the container,
--      and applying the function over the elements of the container
--
-- **OR** you can think of fmap as taking a function that works on values,
--      and transforming it into a function that works on containers of values
--
-- example of an Applicative
-- It can also be a Monad, but I'm skipping Monad
-- 

--
-- example to show functor that isn't just a container,
-- and an example of Applicative.
-- It can also be a Monad, but I'm skipping Monad
-- 


--
-- Example of Compose functors
--

newtype Compose f g a = Compose (f (g a))
    deriving (Eq, Show)

-- you have to fmap the function twice to make it work on the things in the things
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap fun (Compose a) = Compose ((fmap . fmap) fun a)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    -- pure a = Compose (pure (pure a))
    pure = Compose . pure . pure

    -- hint: partially apply (<*>)
    -- (<*>) :: h (a -> b) -> (h a -> h b)
    -- we want to apply (<*>) onto the g (a -> b) but it's inside an f
    -- hint: fmap (<*>) onto 'fun'
    (<*>) :: forall a b. Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fun) <*> (Compose arg) =
        let
            foo :: f (g a -> g b)
            foo = fmap (<*>) fun
            bar = foo <*> arg
         in Compose bar

