module Exercise1 where

{-
	1. (Easy) Use the Math.pi constant to write a function circleArea which computes the area of
	a circle with a given radius. Test your function using PSCi (Hint: don’t forget to import pi
	by modifying the import Math statement).

	2. (Medium) Use bower install to install the purescript-globals package as a dependency.
	Test out its functions in PSCi (Hint: you can use the :browse command in PSCi to browse
	the contents of a module).
-}

import Prelude
import Math (sqrt,pi)
import Control.Monad.Eff.Console (CONSOLE,log)
import Control.Monad.Eff(Eff)
import Global.Unsafe(unsafeToFixed)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

main :: forall a. Eff(console :: CONSOLE | a) Unit
main =
	log $ unsafeToFixed 2 $ circleArea 1.3
