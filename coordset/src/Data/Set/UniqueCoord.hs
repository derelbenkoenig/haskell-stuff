{-# LANGUAGE GADTs #-}
module Data.Set.UniqueCoord where

import qualified Data.Set as S
import Data.Foldable

newtype OnFstPair a b = OnFstPair { unOnFstPair :: (a, b) }
instance Eq a => Eq (OnFstPair a b) where
    OnFstPair (a, _) == OnFstPair (a', _) = a == a'
instance Ord a => Ord (OnFstPair a b) where
    compare (OnFstPair (a, _)) (OnFstPair (a', _)) = compare a a'

newtype OnSndPair a b = OnSndPair { unOnSndPair :: (a, b) }
instance Eq b => Eq (OnSndPair a b) where
    OnSndPair (_, b) == OnSndPair (_, b') = b == b'
instance Ord b => Ord (OnSndPair a b) where
    compare (OnSndPair (_, b)) (OnSndPair (_, b')) = compare b b'

-- Set could take 2 type params, (Set a b), but its kind needs to be Type -> Type
-- to be able to have a Foldable instance
data Set ab where
    Set :: forall a b. S.Set (OnFstPair a b) -> S.Set (OnSndPair a b) -> Set (a, b)

(...) :: (a -> b -> c) -> (b' -> b) -> a -> b' -> c
f ... g = \a b' -> f a (g b')

instance Foldable Set where
    foldr f z (Set s1 _) = foldr (f . unOnFstPair) z s1
    foldr' f z (Set s1 _) = foldr' (f . unOnFstPair) z s1
    foldl f z (Set s1 _) = foldl (f ... unOnFstPair) z s1
    foldl' f z (Set s1 _) = foldl' (f ... unOnFstPair) z s1
    foldMap f (Set s1 _) = foldMap (f . unOnFstPair) s1
    foldMap' f (Set s1 _) = foldMap' (f . unOnFstPair) s1
    length (Set s1 _) = length s1
    null (Set s1 _) = null s1
    toList (Set s1 _) = map unOnFstPair (toList s1)

-- Require that an item is inserted into both sets or neither.
insert :: (Ord a, Ord b, ab ~ (a, b)) => ab -> Set ab -> Set ab
insert ab s@(Set s1 s2) =
    if OnFstPair ab `S.member`  s1 || OnSndPair ab `S.member` s2
       then s
       else Set (S.insert (OnFstPair ab) s1) (S.insert (OnSndPair ab) s2)

empty :: Set (a, b)
empty = Set S.empty S.empty

singleton :: (a, b) -> Set (a, b)
singleton xy = Set (S.singleton (OnFstPair xy)) (S.singleton (OnSndPair xy))

member :: (Ord a, Ord b, ab ~ (a, b)) => (a, b) -> Set (a, b) -> Bool
member xy (Set s1 _) = S.member (OnFstPair xy) s1

fromList :: (Ord a, Ord b, ab ~ (a, b)) => [ab] -> Set ab
fromList = foldl' (flip insert) empty

union :: (Ord a, Ord b, ab ~ (a, b)) => Set ab -> Set ab -> Set ab
union = foldl' (flip insert)

difference :: (Ord a, Ord b, ab ~ (a, b)) => Set ab -> Set ab -> Set ab
difference (Set s1 s2) (Set s1' s2') = Set (S.difference s1 s1') (S.difference s2 s2')
