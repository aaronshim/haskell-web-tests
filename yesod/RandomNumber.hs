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
    -- then HTML template
    getRandomNumberHTMLPage random random'

getRandomNumberHTMLPage :: Int -> Int -> Handler Html
getRandomNumberHTMLPage random random' = defaultLayout $ do
    setTitle "Welcome"
    [whamlet|
        <h1>Hello, World!
        <p>Fancy package's random number: #{random}
        <p>System.Random's random number: #{random'}
    |]