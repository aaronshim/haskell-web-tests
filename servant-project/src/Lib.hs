{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class
import qualified System.Random as R

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- we can define APIs and then combine them with the special :<|> operator
type UsersAPI = "users" :> Get '[JSON] [User]
type RootAPI = Get '[PlainText] String
type API = UsersAPI :<|> RootAPI

-- Main IO action
startApp :: IO ()
startApp = run 8080 app

-- the abstract representation of the app-- won't run until we run it with IO action
app :: Application
app = serve api server

-- yeah I'm still not clear why this needs to be here, but it does?
api :: Proxy API
api = Proxy

-- think of the API as the type signature of the web framework's fnunctions
-- Server is the implementation of the API (which is the metaphor for the type signature)
-- can combine servers with the same combinator as type-level API's BUT only in that exact order!
server :: Server API
server = usersServer :<|> rootServer

-- type synonyms?
--usersServer :: Server UsersAPI
usersServer :: Handler [User]
usersServer = return users

-- type synonyms?
--rootServer :: Server RootAPI
rootServer :: Handler String
rootServer = do
  r <- liftIO $ R.randomRIO (0, 100)
  return $ "Hello, world! " ++ (show (r :: Integer))

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
