module Main where

import Prelude
import Control.Monad.Eff.Console(log, CONSOLE)
import Control.Monad.Eff(Eff)

main :: forall a. Eff(console :: CONSOLE | a) Unit
main = log("S")
