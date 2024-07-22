{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Core where

import Servant
import CRUD.Model (Person, Address)
import Control.Monad.IO.Class (liftIO)
import CRUD.Query

-- Define the Person API
type PersonAPI = "persons" :> Get '[JSON] [Person]   -- GET /persons
                :<|> "persons" :> ReqBody '[JSON] Person :> Post '[JSON] Person  -- POST /persons
                :<|> "persons" :> ReqBody '[JSON] Person :> Put '[JSON] Person  -- PUT /persons
                :<|> "persons" :> Capture "id" Int :> Delete '[JSON] ()   -- DELETE /persons/:id

-- Define the Address API
type AddressAPI = "addresses" :> Get '[JSON] [Address]   -- GET /addresses
                 :<|> "addresses" :> ReqBody '[JSON] Address :> Post '[JSON] Address  -- POST /addresses
                 :<|> "addresses" :> ReqBody '[JSON] Address :> Put '[JSON] Address  -- PUT /addresses
                 :<|> "addresses" :> Capture "id" Int :> Delete '[JSON] ()   -- DELETE /addresses/:id

-- Define the handlers for Person
fetchPersons :: Handler [Person]
fetchPersons = liftIO fetchPersonsQ

insertPerson :: Person -> Handler Person
insertPerson person = do
  liftIO $ insertPersonQ person
  return person

updatePerson :: Person -> Handler Person
updatePerson person = do
  liftIO $ updatePersonQ person
  return person

deletePerson :: Int -> Handler ()
deletePerson personId = do
  liftIO $ deletePersonQ personId
  return ()

-- Define the handlers for Address
fetchAddresses :: Handler [Address]
fetchAddresses = liftIO fetchAddressesQ

insertAddress :: Address -> Handler Address
insertAddress address = do
  liftIO $ insertAddressQ address
  return address

updateAddress :: Address -> Handler Address
updateAddress address = do
  liftIO $ updateAddressQ address
  return address

deleteAddress :: Int -> Handler ()
deleteAddress addressId = do
  liftIO $ deleteAddressQ addressId
  return ()

-- Define the Person API server
personAPI :: Server PersonAPI
personAPI = fetchPersons :<|> insertPerson :<|> updatePerson :<|> deletePerson

-- Define the Address API server
addressAPI :: Server AddressAPI
addressAPI = fetchAddresses :<|> insertAddress :<|> updateAddress :<|> deleteAddress

-- Combine both APIs
api :: Proxy (PersonAPI :<|> AddressAPI)
api = Proxy

-- Define the application
app :: Application
app = serve api (personAPI :<|> addressAPI)