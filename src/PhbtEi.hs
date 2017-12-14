module PhbtEi where

import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhbtEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

instance Applicative (PhbtEither b) where
  pure = Left
  Left a <*> Right b = Right b
  Right b <*> Left a = Right b
  Left a <*> Left b = Left (a b)

instance Monad (PhbtEither b) where
  return = pure
  Left a >>= f = f a
  Right b >>= f = Right b

instance (Eq a, Eq b) => EqProp(PhbtEither a b) where (=-=) = eq


{--
main = do
  let trigger = undefined :: PhhhbbtttEither String (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
--}
