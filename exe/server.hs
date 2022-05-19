{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Network.Socket
import Network.Socket.ByteString

import Damascus.Message

main :: IO ()
main = withSocketsDo $ do
	sock <- socket AF_INET Stream defaultProtocol
	bind sock (SockAddrInet 9082 (tupleToHostAddress (127, 0, 0, 1)))
	listen sock 256
	forever $ do
		(conn, _addr) <- accept sock
		forkIO (serve conn)

serve :: Socket -> IO ()
serve conn = do
	sendAll conn (encode (SMProposeVersion ["0"]))
	go
	where
	go = do
		bs <- recv conn 1024
		case bs of
			"" -> gracefulClose conn 1000
			_ -> print bs >> go
