module List where

import Control.Applicative
import Data.Monoid(Monoid, (<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons a la) = Cons a $ take' (n-1) la

--instance (Monoid a) => Monoid (List a) where
instance Monoid (List a) where
  mempty = Nil
  mappend Nil a = a
  mappend a Nil = a
--mappend (Cons a la) (Cons b lb) = Cons(a <> b) (la <> lb) -- так тоже правильно
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List  where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure  = mempty
  (<*>) Nil _ = Nil
  (<*>) (Cons x xs) Nil = Nil
  (<*>) (Cons f xs) ys = fmap f ys <> (xs <*> ys)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a la) f = f a <> (la >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    la <- arbitrary
    frequency [(1,return  Nil), (9, return (Cons a la))]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys


{--
main = do
  let trigger = undefined :: List (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
--}
