{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import System.Random
import qualified Data.Text as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef (Int, StdGen))

main :: IO ()
main =
    do gen <- getStdGen
       ref <- newIORef (0, gen)
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           do (DummyAppState ref) <- getState
              -- we will store the stdGen in the Application State
              stdGen <- liftIO $ atomicModifyIORef' ref $ \(i, g) -> ((i, g), g)
              let passphrase = base64string 10 stdGen
              -- and we are going to manually update the generator
              newGen <- liftIO newStdGen
              liftIO $ putStrLn $ "Hi there " ++ show (passphrase) ++ " with generator " ++ show (newGen)
              _ <- liftIO $ atomicModifyIORef' ref $ \(i, g) -> ((i, newGen), g)
              -- display
              text $ "Hello World!\nYour random word is " <> passphrase
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \(i, g) -> ((i+1, g), i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

-- not very efficient but whatever (later I guess we'll use a library of sorts?)
base64string :: RandomGen g => Int -> g -> T.Text
base64string size gen =
    let
        allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
        indexRange = (0, length allowedChars - 1)
    in
        T.pack $ map (\x -> allowedChars !! x) $ take size $ randomRs indexRange gen