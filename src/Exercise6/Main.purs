module Exercise6 where


import Data.Foldable(foldl)
import Prelude
import Data.Array.Partial (head, tail)
import Partial.Unsafe(unsafePartial)

checkTrue :: Array Boolean -> Boolean
checkTrue arr =
	foldl (\x elt-> x && elt ) true arr

count :: forall a. (a -> Boolean) -> Array a -> Int -> Int
count _ [] acc = acc
count p xs acc= if p (unsafePartial head xs)
	then count p (unsafePartial tail xs) acc+1
	else count p (unsafePartial tail xs) acc

{-
count2 :: forall a. Int -> (a -> Boolean) -> Array a -> Int
count2 x _ []= x
count2 x p xs= if p (unsafePartial head xs)
	then count x+1 p (unsafePartial tail xs)
	else count x p (unsafePartial tail xs)
foldl (\acc n -> acc <> show n) "" [1,2,3,4,5]
"12345"
-}
reverse :: Array Int -> String
reverse arr =
	foldl (\acc n -> show n <> acc) "" arr
