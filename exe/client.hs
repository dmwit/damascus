{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Network.Socket
import System.IO

import qualified Data.Vector as V
import qualified Network.Socket.ByteString.Lazy as LS
import qualified Network.Socket.ByteString as S

import Damascus.Message

main :: IO ()
main = withSocketsDo $ do
	sock <- socket AF_INET Stream defaultProtocol
	connect sock (SockAddrInet 9082 (tupleToHostAddress (127, 0, 0, 1)))
	s <- clientState sock
	negotiateVersion s

data ClientState = ClientState
	{ fromServer :: Chan ServerMessage
	, toServer :: Chan ClientMessage
	}

clientState :: Socket -> IO ClientState
clientState sock = do
	f <- newChan
	t <- newChan
	_ <- forkIO (receiveThread sock f)
	_ <- forkIO (sendThread sock t)
	pure $ ClientState
		{ fromServer = f
		, toServer = t
		}

receiveThread :: Socket -> Chan ServerMessage -> IO ()
receiveThread sock f = LS.getContents sock >>= go where
	go bs = case decode bs of
		Left err -> hPrint stderr (err, bs)
		Right (bs', msg) -> writeChan f msg >> go bs'

sendThread :: Socket -> Chan ClientMessage -> IO ()
sendThread sock t = forever $ do
	msg <- readChan t
	S.send sock (encode msg)

receive :: ClientState -> IO ServerMessage
receive = readChan . fromServer

send :: ClientState -> ClientMessage -> IO ()
send = writeChan . toServer

negotiateVersion :: ClientState -> IO ()
negotiateVersion s = do
	msg <- receive s
	case msg of
		SMProposeVersion vs | V.length vs >= 1 -> send s (CMAcceptVersion (V.head vs))
		_ -> pure ()
