{-# LANGUAGE OverloadedStrings #-}
module Main where
import Debug.Trace

import System.Environment ( getArgs, getEnv )
import System.IO
import Network.HTTP.Conduit
import Network.HTTP.Types.Header ( HeaderName )
import Data.Conduit
import Data.Conduit.Binary ( sinkFile )
import Data.Aeson ( decode, encode )
import Control.Applicative ( (<$>) )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource ( runResourceT )
import Data.List.Split ( splitOn )
import Data.Time

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L

import Kakeibo.Server.Models.MoneyTransfer
import Kakeibo.Server.Models.MoneyTransferJson

-- TODO: merge with Kakeibo.Server.Middlewares.APIAuthentication
tokenHeaderName :: HeaderName
tokenHeaderName = "X-Kakeibo-Token"

main :: IO ()
main = do
    action <- head <$> getArgs
    apiToken <- getEnv $ T.unpack "KAKEIBO_API_TOKEN"
    runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        liftIO $
            case action of
                "list" -> showTransfers (B8.pack apiToken) manager
                "add" -> createTransfers (B8.pack apiToken) manager
                _ -> putStrLn "Invalid action"

showTransfers :: B8.ByteString -> Manager -> IO ()
showTransfers apiToken manager = do
    req <- liftIO $ parseUrl "http://localhost:3000/api/transfers"
    res <- httpLbs req { requestHeaders = (tokenHeaderName, apiToken) : requestHeaders req } manager
    let Just transferList = (decode $ responseBody res) :: Maybe [MoneyTransfer]
    sequence $ map putStrLn $ map show transferList
    return ()

prompt :: String -> IO String
prompt promptStr = do
    putStr $ promptStr ++ " > "
    hFlush stdout
    getLine

createTransfers :: B8.ByteString -> Manager -> IO ()
createTransfers apiToken manager = do
    amount <- read <$> prompt "How much?"
    description <- T.pack <$> prompt "What is it for?"
    category <- T.pack <$> prompt "Category?"
    date <- prompt "When was this (YYYY-MM-DD)?"
    putStrLn ""

    currentTime <- getCurrentTime
    let dateTimeToSave = case date of
            "" -> currentTime
            _ -> UTCTime (fromGregorian y m d) (secondsToDiffTime 0)
                where dateArr = splitOn "-" date
                      y = read $ dateArr!!0 :: Integer
                      m = read $ dateArr!!1 :: Int
                      d = read $ dateArr!!2 :: Int

    let valueBS = encode $ MoneyTransferUnsaved amount dateTimeToSave description category
    req' <- liftIO $ parseUrl "http://localhost:3000/api/transfers"
    let req = req' { method = "POST"
                   , requestHeaders = (tokenHeaderName, apiToken) : requestHeaders req'
                   , requestBody = RequestBodyLBS valueBS
                   }
    res <- httpLbs req manager
    let Just newTransfer = (decode $ responseBody res) :: Maybe MoneyTransfer
    putStrLn $ show newTransfer
