{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as UV
import qualified Network.Matrix.Client as MTX
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Namegen
import System.IO

botArgumentsId :: MTX.RoomID
botArgumentsId = MTX.RoomID "REDACTED"

nextTxnID :: IO MTX.TxnID
nextTxnID = do
  uuid <- UV.nextRandom
  return (MTX.TxnID $ U.toText uuid)

toMessage :: T.Text -> MTX.MessageText
toMessage t = MTX.MessageText t MTX.TextType Nothing Nothing

toRoomMessage :: MTX.MessageText -> MTX.RoomMessage
toRoomMessage t = MTX.RoomMessageText t

toMessageEvent :: MTX.RoomMessage -> MTX.Event
toMessageEvent m = MTX.EventRoomMessage m

textEvent :: String -> MTX.Event
textEvent = toMessageEvent . toRoomMessage . toMessage . T.pack . prependNick

prependNick :: String -> String
prependNick s = "<The Greatest Namegen>: " ++ s

freshSession :: IO MTX.ClientSession
freshSession = do
  token <- MTX.getTokenFromEnv "MATRIX_TOKEN"
  sess <- MTX.createSession "https://matrix.org" token
  return sess

namegenSend :: MTX.ClientSession
            -> MTX.RoomID
            -> IO (Either MTX.MatrixError MTX.EventID)
namegenSend session roomId = do
  name  <- namegen
  txnId <- nextTxnID
  let event = textEvent name
  MTX.sendMessage session roomId event txnId

getLastMessages :: MTX.ClientSession
                 -> Int
                 -> MTX.MatrixIO MTX.PaginatedRoomMessages
getLastMessages session limit =
  ( MTX.getRoomMessages
      session
      botArgumentsId
      MTX.B
      Nothing
      ""
      (Just limit)
      Nothing
  )

getContent :: MTX.RoomEvent -> T.Text
getContent (MTX.RoomEvent
             (MTX.EventRoomMessage
               (MTX.RoomMessageText
                 (MTX.MessageText text _ _ _)
               )
             ) _ _ _
           ) = text
getContent _ = ""

isNamegenCommand :: MTX.RoomEvent -> Bool
isNamegenCommand roomEvent = "nameme" `T.isPrefixOf` message
  where message = getContent roomEvent

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

namegenRequested :: MTX.ClientSession
                 -> IO (Either MTX.MatrixError Bool)
namegenRequested session = do
  messagesResult <- getLastMessages session 1
  case messagesResult of
    (Left err) -> return (Left err)
    (Right (MTX.PaginatedRoomMessages messages _ _ _)) ->
      case safeHead  messages of
        Nothing          -> return (Right False)
        Just lastMessage ->
          case isNamegenCommand lastMessage of
            True  -> return (Right True)
            False -> return (Right False)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  session <- freshSession
  forever $ do
    shouldDoNamegen <- namegenRequested session
    case shouldDoNamegen of
      (Left     err) -> print err
      (Right result) ->
        case result of
          True  -> do
            sendResult <- namegenSend session botArgumentsId
            case sendResult of
              (Right eventId) -> print $ eventId
              (Left err)      -> print $ err
          False -> return ()
    threadDelay 5000000
