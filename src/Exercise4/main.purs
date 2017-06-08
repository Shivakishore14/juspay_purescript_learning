module Exercise4 where

import Control.Monad.Eff.Console(log, CONSOLE)
import Control.Monad.Eff(Eff)
import Data.Array (filter)
import Prelude

main :: forall a. Eff(console :: CONSOLE | a) Unit
main = log("S")

findSquare :: Array Int -> Array Int
findSquare arr =
	(\n -> n * n) <$> arr

filterNegative :: Array Int -> Array Int
filterNegative arr =
	filter (\n -> n > 0) arr

infix 4 filter as <$?>

filterNegativeUsingInFix :: Array Int -> Array Int
filterNegativeUsingInFix arr =
	(\n -> n > 0) <$?> arr
