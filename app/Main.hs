{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as Scotty
import Network.HTTP.Simple
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , decode
    , encode
    , object
    , (.=)
    , (.:)
    , (.:?)
    , Value(..)
    )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy (Text)
import Data.Maybe (fromMaybe)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Config (pageAccessToken, verifyToken)
import Prelude hiding (id)
import Network.HTTP.Types.Status (status404, status403)

data WebhookEvent = WebhookEvent
    { eventObject :: Text
    , entry :: [Entry]
    } deriving (Show)

data Entry = Entry
    { messaging :: [Messaging]
    } deriving (Show)

data Messaging = Messaging
    { sender :: Sender
    , message :: Maybe Message
    } deriving (Show)

data Sender = Sender
    { senderId :: Text
    } deriving (Show)

data Message = Message
    { messageText :: Maybe Text
    } deriving (Show)

instance FromJSON WebhookEvent where
    parseJSON (Object v) = WebhookEvent <$> v .: "object" <*> v .: "entry"
    parseJSON _ = mzero

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$> v .: "messaging"
    parseJSON _ = mzero

instance FromJSON Messaging where
    parseJSON (Object v) = Messaging <$> v .: "sender" <*> v .:? "message"
    parseJSON _ = mzero

instance FromJSON Sender where
    parseJSON (Object v) = Sender <$> v .: "id"
    parseJSON _ = mzero

instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .:? "text"
    parseJSON _ = mzero

callSendApi :: Text -> Text -> IO ()
callSendApi senderPsid responseText = do
    let url = "https://graph.facebook.com/v15.0/me/messages?access_token=" <> TL.pack pageAccessToken
    let payload = object
            [ "recipient" .= object ["id" .= senderPsid]
            , "message" .= object ["text" .= responseText]
            , "messaging_type" .= ("RESPONSE" :: Text)
            ]
    initReq <- parseRequest (TL.unpack url)
    let req = setRequestMethod "POST" $
                setRequestHeaders [("Content-Type", "application/json")] $
                setRequestBodyLBS (encode payload) initReq
    response <- httpLBS req
    BL.putStrLn $ getResponseBody response

handleMessage :: Text -> Message -> IO ()
handleMessage senderPsid (Message (Just msgText)) = callSendApi senderPsid msgText
handleMessage senderPsid _ = callSendApi senderPsid "only text"

main :: IO ()
main = Scotty.scotty 4567 $ do
    Scotty.get "/home" $ Scotty.text "test doang"

    Scotty.post "/webhook" $ do
        body <- Scotty.body
        let webhookEvent = decode body :: Maybe WebhookEvent
        case webhookEvent of
            Just (WebhookEvent "page" entries) -> do
                mapM_ (\entry -> mapM_ (\messaging -> liftIO $ handleMessage (senderId $ sender messaging) (fromMaybe (Message Nothing) $ message messaging)) $ messaging entry) entries
                Scotty.text "EVENT_RECEIVED"
            _ -> Scotty.status status404 >> Scotty.text "ERROR"


    Scotty.get "/webhook" $ do
        mode <- Scotty.param "hub.mode" :: Scotty.ActionM Text
        token <- Scotty.param "hub.verify_token" :: Scotty.ActionM Text
        challenge <- Scotty.param "hub.challenge" :: Scotty.ActionM Text
        if mode == TL.pack "subscribe" && token == TL.pack verifyToken
            then Scotty.text challenge
            else Scotty.status status403 >> Scotty.text "ERROR"
