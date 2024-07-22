{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CRUD.Query where

import Database.SQLite.Simple
import CRUD.Model

-- Get database connection
getConn :: IO Connection
getConn = open "C:/sqlite/servantDB"

-- Person queries
fetchPersonsQ :: IO [Person]
fetchPersonsQ = do
  conn <- getConn
  personList <- query_ conn "SELECT * FROM persons;"
  close conn
  pure personList



insertPersonQ :: Person -> IO ()
insertPersonQ person = do
  conn <- getConn
  execute conn "INSERT INTO persons (person_id, person_name, age) VALUES (?,?,?);" person
  close conn

updatePersonQ :: Person -> IO ()
updatePersonQ Person{..} = do
  conn <- getConn
  execute conn "UPDATE persons SET person_name = ?, age = ? WHERE person_id = ?;" (personName, age, personId)
  close conn

deletePersonQ :: Int -> IO ()
deletePersonQ personId = do
  conn <- getConn
  execute conn "DELETE FROM persons WHERE person_id = ?" (Only personId)
  close conn

-- Address queries
fetchAddressesQ :: IO [(Person, Address)]
fetchAddressesQ = do
  conn <- getConn
  rows <- query_ conn "SELECT p.person_id, p.person_name, p.age, a.address_id, a.address_street, a.address_city, a.address_zip, a.person_map_id FROM persons p JOIN addresses a ON p.person_id = a.person_map_id;"
  close conn
  return $ map (\(pid, pname, page, aid, astreet, acity, azip, apid) ->
    (Person pid pname page, Address aid astreet acity azip apid)) rows

insertAddressQ :: Address -> IO ()
insertAddressQ address = do
  conn <- getConn
  execute conn "INSERT INTO addresses (address_id, address_street, address_city, address_zip,person_map_id) VALUES (?,?,?,?,?);" address
  close conn

updateAddressQ :: Address -> IO ()
updateAddressQ Address{..} = do
  conn <- getConn
  execute conn "UPDATE addresses SET address_street = ?, address_city = ?, address_zip = ? WHERE address_id = ?;" (addressStreet, addressCity, addressZip, addressId)
  close conn

deleteAddressQ :: Int -> IO ()
deleteAddressQ addressId = do
  conn <- getConn
  execute conn "DELETE FROM addresses WHERE address_id = ?" (Only addressId)
  close conn