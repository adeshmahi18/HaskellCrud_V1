{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Model where

import GHC.Generics
import Data.Aeson
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- Define the Person data type
data Person = Person
  { personId   :: Int
  , personName :: String
  , age        :: Int
  } deriving (Eq, Show, Generic, FromRow, ToRow, ToJSON, FromJSON)


data Address = Address
  {
   addressId     :: Int
  , addressStreet :: String
  , addressCity   :: String
  , addressZip    :: String
  ,  personMapId :: Int
  } deriving (Eq, Show, Generic, FromRow, ToRow, ToJSON, FromJSON)