module Cow where

import Control.Monad
--newtype Name = Name  String deriving (Eq, Show)

data Cow = Cow {
  name   :: String
, age    :: Int
, weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty name  | name /= "" = Just name
              | otherwise = Nothing

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

--- If our cow is Bess then her weight under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let n = name c
      w = weight c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agy ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agy weighty)

-------------------------- with Monads ---------------------------------------
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name age weight = do
  name'   <- noEmpty name
  age'    <- noNegative age
  weight' <- noNegative weight
  weightCheck (Cow name' age' weight')

------------------- with Tiefighter (>>=) ------------------------------------

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy ->
    noNegative age' >>=
      \ agy ->
      noNegative weight' >>=
        \weighty ->
        weightCheck (Cow nammy agy weighty)

-------------------  doSomething ---------------------------------------------
f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
    then Just(i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("9001" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a,b,c)

-- doSomething'' n = (f n) <*> () -? how to rewrite with Applicative? Impossible 
