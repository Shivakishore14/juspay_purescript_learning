module Exercise3 where

import Prelude
import Control.Monad.Eff.Console(log, CONSOLE)
import Data.Array(null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe(unsafePartial)
import Control.Monad.Eff(Eff)
--import Data.Int.Bits ((.&.))

main :: forall a. Eff(console :: CONSOLE | a) Unit
main = log("S")


--isEven using recursion
isEven :: Int -> Boolean
isEven num =
	if num == 0
		then true
	else if num == 1
		then false
	else isEven (num-2)

countEven :: Array Int -> Int
countEven arr =
	if null arr
		then 0
	else
		isEvenNum (unsafePartial head arr) + countEven(unsafePartial tail arr)
	where
		isEvenNum :: Int -> Int
		isEvenNum num =
			if (isEven num)
				then 1
			else 0
