module EitherMonad  where

import Control.Monad

type Founded = Int

type Coders  = Int

data SoftwearShop = Shop {
    founded     :: Founded
  , programmers :: Coders
} deriving (Eq, Show)

data FoundedError =
  NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n 
