{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}

module SkiFinal where

import Data.Kind
import Data.Functor.Identity
import Data.Function (fix)

class Ski f where
    -- s = \f g x -> f x (g x)
    s :: f ((e -> a -> b) -> (e -> a) -> e -> b)
    -- k = \x y -> x
    k :: f (a -> b -> a)
    -- i = \x -> x
    i :: f (a -> a)
    -- application
    ($$) :: f (a -> b) -> f a -> f b
    -- fix point combinator
    y :: f ((a -> a) -> a)

infixl 9 $$

-- other combinators as helpers to write expressions and ultimately render as
-- SKI only

-- b = \f g x -> f (g x)
b :: Ski f => f ((b -> c) -> (a -> b) -> (a -> c))
b = s $$ (k $$ s) $$ k

-- t = \f y x -> f x y
c :: Ski f => f ((a -> b -> c) -> (b -> a -> c))
c = s $$ (b $$ b $$ s) $$ (k $$ k)

-- t = \x f -> f x
t :: Ski f => f (a -> (a -> b) -> b)
t = c $$ i

-- composition applied in the metalanguage level rather than in the combinators
after :: Ski f => f (b -> c) -> f (a -> b) -> f (a -> c)
after g h = b $$ g $$ h

infixr 8 `after`

instance Ski Identity where
    s = Identity (<*>)
    k = Identity const
    i = Identity id
    y = Identity fix
    (Identity f) $$ (Identity x) = Identity (f x)

newtype PrintSki a = PrintSki { runPrintSki :: Int -> ShowS }

printSki :: PrintSki a -> String
printSki x = runPrintSki x 0 ""

instance Ski PrintSki where
    s = PrintSki $ const $ showString "S"
    k = PrintSki $ const $ showString "K"
    i = PrintSki $ const $ showString "I"
    y = PrintSki $ const $ showString "Y"
    PrintSki f $$ PrintSki x = PrintSki $ \d -> showParen (d > 10) $
        f 11 . showString " " . x 11

type ChurchNumeral a = (a -> a) -> (a -> a)

churchInt :: (Ski f, Integral n) => n -> f (ChurchNumeral a)
churchInt n
  | n < 0 = error "negative church numeral"
  | n == 0 = k $$ i
  | otherwise = churchSucc $$ churchInt (n - 1)

fromChurchInt :: Integral n => Identity (ChurchNumeral n) -> n
fromChurchInt (Identity f) = f (+ 1) 0

-- \n f x -> f ((n f) x)
-- = \n f x -> (f . n f) x
-- = \n f -> f . n f
-- = \n f -> s (k f) (n f)
-- = \n f -> (s . k) f (n f)
-- = \n f -> (s (k s) k) f (n f)
-- = \n f -> s (s (k s) k) n f
-- = \n -> s (s (k s) k) n
-- = s (s (k s) k)
-- (s (k s) k) is b, AKA (.) composition
-- ...working backwards...
-- s b n f x
-- = b f (n f) x
-- = f (n f x)
-- ✓
churchSucc :: Ski f => f (ChurchNumeral a -> ChurchNumeral a)
churchSucc = s $$ (s $$ (k $$ s) $$ k)

-- I really want this type to have a 'forall c.' instead of c as a third
-- argument, but for the most part it seems to work better if I include the c
-- in this definition and then let the c be in implicitly quantified parameter
-- in other declarations that use this, letting haskell's type inference figure
-- out that part of the polymorphism
type ChurchPair a b c = a -> b -> (a -> b -> c) -> c

--   \a b f -> f a b
-- = \a b f -> (f a) b
-- = \a b -> S (\f -> f a) (K b)
-- = \a b -> S (S I (K a)) (K b)
-- = \a -> S (S (S I (K a))) K
-- ... something with that many right-associated applications is annoying to
-- translate. I looked it up, it's B C T, so I just defined B, C and T above.
churchPair :: Ski f => f (ChurchPair a b c)
churchPair = b $$ c $$ t

-- just for fun, I'm not using these right now

-- zero tuple is just the identity function (takes a "zero-argument continuation")
-- r -> r
-- a0 -> (a0 -> r) -> r
-- a1 -> a0 -> (a1 -> a0 -> r) -> r
-- a2 -> a1 -> a0 -> (a2 -> a1 -> a0 -> r) -> r
-- etc.
type family ChurchTuple (ts :: [Type]) r where
    ChurchTuple ts r = FunN ts (FunN ts r -> r)

-- i.e.:
-- r
-- a1-> r
-- a2 -> a1 -> r
-- a3 -> a2 -> a1 -> r
-- etc.
type family FunN (ts :: [Type]) r where
    FunN '[] r = r
    FunN (t ': ts) r = t -> FunN ts r

type ChurchBool a = a -> a -> a

-- true = \x y -> x
--      = \x -> k x
--      = k
churchTrue :: Ski f => f (ChurchBool a)
churchTrue = k

-- false = \x y -> y
--       = \x -> i
--       = k i
churchFalse :: Ski f => f (ChurchBool a)
churchFalse = k $$ i

-- halfIncrement = \pair -> if snd pair then (succ (fst pair), false) else (fst pair, true)
-- = \pair -> pair (\n isOdd -> isOdd (\f -> f (succ n) false) (\f -> f n true))
-- = S I (K (\n isOdd -> isOdd (\f -> (f (succ n)) false) (\f -> (f n) true)))
-- = S I (K (\n isOdd -> isOdd (\f -> (f (succ n)) false) (S (\f -> f n) (K true))))
-- = S I (K (\n isOdd -> isOdd (\f -> (f (succ n)) false) (S (S I (K n)) (K true))))
-- = S I (K (\n isOdd -> isOdd (S (\f -> f (succ n)) (K false)) (S (S I (K n)) (K true))))
-- = S I (K (\n isOdd -> isOdd (S (S I (K (succ n))) (K false)) (S (S I (K n)) (K true))))
-- = S I (K (\n isOdd ->  isOdd (S (S I (K (succ n))) (K false))  (S (S I (K n)) (K true))))
-- = S I (K (\n isOdd -> (isOdd (S (S I (K (succ n))) (K false))) (S (S I (K n)) (K true))))
-- ... I don't want to expand out those nested applications to n, try again
-- using more B/C/T or something

-- halfIncrement =
--   \pair -> pair (\n isOdd -> isOdd (\f -> f (succ n) false) (\f -> f n true))
-- = S I (K (\n isOdd -> isOdd (\f -> f (succ n) false) (\f -> f n true)))

-- want a helper for (\x f -> f (g x) (h x))
-- \x f -> f (g x) (h x)
-- = \x -> S (\f -> f (g x)) (K (h x))
-- = \x -> S (S I (K (g x))) (K (h x))
-- = \x -> S ((S I . K . g ) x) ((K . h) x)
-- = \x -> ((S . S I . K . g ) x) ((K . h) x)
-- = S (S . S I . K . g) (K . h)
halfIncrementHelper :: Ski f => f (e -> a0) -> f (e -> a1) -> f (e -> (a0 -> a1 -> b) -> b)
halfIncrementHelper g h = s $$ (s `after` (s $$ i) `after` k `after` g) $$ (k `after` h)

-- ok, so...
-- halfIncrement
-- = S I (K (halfIncrementHelper (\n f -> f (succ n) false)) (halfIncrementHelper (\n f -> f n true)))
-- = S I (K (halfIncrementHelper (halfIncrementHelper succ (K false))) (halfIncrementHelper (halfIncrementHelper I (K true))))
halfIncrement :: Ski f => f (ChurchPair (ChurchNumeral a) (ChurchBool _) r -> ChurchPair (ChurchNumeral a) (ChurchBool _) r)
halfIncrement = undefined
    -- s
    -- $$ i
    -- $$ (k 
    --     $$ halfIncrementHelper
    --         (halfIncrementHelper churchSucc (k $$ churchFalse))
    --         (halfIncrementHelper i (k $$ churchTrue)))
