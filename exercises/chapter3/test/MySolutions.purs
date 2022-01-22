module Test.MySolutions where

import Data.AddressBook
import Prelude
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe, fromMaybe, isJust)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((==) street <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = isJust $ findEntry firstName lastName book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq pred
  where
  pred e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
