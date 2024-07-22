{-# LANGUAGE DataKinds #-}
module CRUD.Core where

import Network.Wai.Handler.Warp
import Network.Wai
import Servant.Server
import Servant ((:>),Get,JSON,(:<|>)(..),ReqBody,Post,Put,Delete,Capture)
import Data.Proxy
import CRUD.Query
import Control.Monad.IO.Class

type PersonAPI = "persons" :> Get '[JSON] [Person] -- "/persons" GET [Person]
                :<|> "persons" :> ReqBody '[JSON] Person :> Post '[JSON] Person -- "/persons" POST Person
                :<|> "persons" :> ReqBody '[JSON] Person :> Put '[JSON] Person -- "/persons" Put Person
                :<|> "persons" :> Capture "id" Int :> Delete '[JSON] () -- "/persons" Put ()

fetchPerson :: Handler [Person]
fetchPerson = do
  personList <- liftIO fetchPersonsQ
  return personList

insertPerson :: Person -> Handler Person
insertPerson person = do
  liftIO $ insertPersonQ person
  return person

updatePerson :: Person -> Handler Person
updatePerson person = do
  liftIO $ updatePersonQ person
  pure person

deletePerson :: Int -> Handler ()
deletePerson personId = do
  liftIO $ deletePersonQ personId
  pure ()

personAPI :: Server PersonAPI
personAPI = (fetchPerson :<|> insertPerson :<|> updatePerson :<|> deletePerson)

app :: Application
app = serve (Proxy :: Proxy PersonAPI) personAPI

main :: IO ()
main = do
  putStrLn "server started at port 8080"
  run 8080 app

test :: IO ()
test = putStrLn "Hello from Core"