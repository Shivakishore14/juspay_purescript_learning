module Exercise5 where


import Data.Array ((..), length, concat)
import Control.MonadZero (guard)
import Prelude


factors :: Int -> Array (Array Int)
factors n = do
	i <- 1 .. n
	j <- i .. n
	guard $ i * j == n
	[[i, j]]

isPrime :: Int -> Boolean
isPrime n =
	if (length (factors n)) == 1
		then true
	else
		false

pythagoreanTriple :: Int -> Array ( Array Int )
pythagoreanTriple n = do
	i <- 0 .. n
	j <- 0 .. n
	k <- 0 .. n
	guard $ (i * i) + (j * j) == k * k
	[[i,j,k]]

factorizations :: Int -> Array Int
factorizations n =  do
	i <- 1 .. (n/2-1)
	guard $ mod n i == 0
	concat [[i,otherFact i]]
	where
		otherFact :: Int -> Int
		otherFact i = n / i

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct arr1 arr2 = do
	i <- arr1
	j <- arr2
	[[i,j]]
