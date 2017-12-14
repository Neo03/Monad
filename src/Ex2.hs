module Ex2 where

import Data.Monoid(Monoid, (<>))
import Control.Monad
import Control.Applicative

j:: Monad m => m(m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a1 :: Monad m => m a -> (a -> m b) -> m b
a1 = (>>=)

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
--meh (x:xs) f = (++) <$> (fmap (\a -> [a]) $ f x) <*> meh xs f
--meh (x:xs) f = (++) <$> (\a -> [a]) <$> f x <*> meh xs f
--meh (x:xs) f = (++) <$> (: []) <$> f x <*> meh xs f
meh (x:xs) f = ((++) . (: []) <$> f x) <*> meh xs f

{--
main = do
  print $ meh [1..10] (\x -> if odd x then Just x else Just (x * 2))
--}

flipType :: Monad m => [m a] -> m [a]
flipType xs  = meh xs $ join . pure
