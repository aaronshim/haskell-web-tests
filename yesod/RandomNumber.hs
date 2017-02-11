{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module RandomNumber where

import Foundation
import Yesod.Core

-- we can try this package's random generator
import Crypto.Random (SystemRandom, newGenIO)
import Control.Monad.CryptoRandom

-- as well as the system one
import qualified System.Random as R

getRandomNumberR :: Handler Html
getRandomNumberR = do
    -- the complicated fancy library's way of doing Integer generation
    -- (apparently this guy is more perfomant and accounts explicitly for failure?)
    gen <- liftIO newGenIO -- unwrap an IO generator
    genEval <- evalCRandT (getCRandomR (1, 100)) (gen :: SystemRandom) -- evaluate (monad because it carries seed state, so needs unwrapping)
    random <-
        case genEval of -- manually deal with errors
            Left e -> error $ show (e :: GenError)
            Right n -> return n
    -- and the simpler system library's way
    -- (where is liftIO coming from? It's probably Yesod's.)
    random' <- liftIO $ R.getStdRandom $ R.randomR (1, 100) -- answer is IO Int (stays wrapped in IO til results computed), need to unwrap
    -- and the "extract the gen" analogue of the fancy library for System.Random that uses liftIO first
    stdGen <- liftIO R.getStdGen
    let passphrase = base64string 10 stdGen -- Base64 String (too many non-printing characters otherwise)
    -- then HTML template
    getRandomNumberHTMLPage random random' passphrase

-- not very efficient but whatever (later I guess we'll use a library of sorts?)
base64string :: R.RandomGen g => Int -> g -> String
base64string size gen =
    let
        allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
        indexRange = (0, length allowedChars - 1)
    in
        map (\x -> allowedChars !! x) $ take size $ R.randomRs indexRange gen

getRandomNumberHTMLPage :: Int -> Int -> String -> Handler Html
getRandomNumberHTMLPage random random' passphrase = defaultLayout $ do
    setTitle "Welcome"
    [whamlet|
        <h1>Hello, World!
        <p>Fancy package's random number: #{random}
        <p>System.Random's random number: #{random'}
        <p>Random string: #{passphrase}
    |]