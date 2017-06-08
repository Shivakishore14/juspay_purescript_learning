module Exercise2 where

{-
	1. (Easy) Test your understanding of the findEntry function by writing down the types of each
	of its major subexpressions. For example, the type of the head function as used is specialized
	to AddressBook -> Maybe Entry .

	2. (Medium) Write a function which looks up an Entry given a street address, by reusing the
	existing code in findEntry . Test your function in PSCi.

	3. (Medium) Write a function which tests whether a name appears in a AddressBook , returning
	a Boolean value. Hint: Use PSCi to find the type of the Data.List.null function, which test
	whether a list is empty or not.

	4. (Difficult) Write a function removeDuplicates which removes duplicate address book entries
	with the same first and last names. Hint: Use PSCi to find the type of the Data.List.nubBy
	function, which removes duplicate elements from a list based on an equality predicate.
-}

import Prelude
import Control.Monad.Eff.Console(log, CONSOLE)
import Control.Monad.Eff(Eff)
import Data.List(List(..), head, filter,null,nubBy)
import Control.Plus (empty)
import Data.Maybe (Maybe)

main :: forall a. Eff(console :: CONSOLE | a) Unit
main = log("s")

type Entry =
	{ firstName :: String
	, lastName  :: String
	, address   :: Address
	}
type Address =
	{ street :: String
	, city   :: String
	, state  :: String
	}

type AddressBook = List Entry

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

showEntry :: Entry -> String
showEntry entry =
	entry.lastName <> ", " <>
	entry.firstName <> ", " <>
	showAddress entry.address

showAddress :: Address -> String
showAddress address =
	address.street <> ", " <>
	address.city <> ", " <>
	address.state

findEntry :: String -> AddressBook -> Maybe Entry
findEntry fname book = head $ filter filterEntry book
	where
		filterEntry :: Entry -> Boolean
		filterEntry entry = entry.firstName == fname

findEntryUsingStreet :: String -> AddressBook -> Maybe Entry
findEntryUsingStreet street book = head $ filter filterEntry book
	where
		filterEntry :: Entry -> Boolean
		filterEntry entry = entry.address.street == street

checkEntry :: String -> AddressBook -> Boolean
checkEntry fname book =  null $ filter filterEntry book
	where
		filterEntry :: Entry -> Boolean
		filterEntry entry = entry.firstName == fname

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy check book
	where
		check :: Entry -> Entry -> Boolean
		check entry1 entry2 =
			entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName

emptyBook :: AddressBook
emptyBook = empty
