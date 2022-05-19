{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.ClassSharing
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Text (Text)
import Data.Traversable
import Data.Vector (Vector)
import Language.Haskell.TH
import System.Exit
import Test.Feat
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Damascus.Message as DM

do
	tys <- sequence [[t|Int64|], [t|Text|], [t|Vector Int64|], [t|HashMap Int64 Int64|], [t|DM.ServerMessage|], [t|DM.ClientMessage|]]
	let	testName ty = mkName ("ty_" ++ filter isAlphaNum (show ty) ++ "_RoundTrip")
	   	names = testName <$> tys
	   	makeTest name ty = sequence
	   		[ sigD name [t| $(return ty) -> Bool |]
	   		, valD (varP name) (normalB [e|messageRoundTrip|]) []
	   		]
	testDeclss <- zipWithM makeTest names tys
	checks <- for names $ \name -> [e| verboseCheckResult $(varE name)|]
	topDecls <- [d|
		testMessageRoundTripProperties :: IO Bool
		testMessageRoundTripProperties = all isSuccess <$> sequence $(return (ListE checks))
		|]
	return . concat $ topDecls : testDeclss

main :: IO ()
main = do
	success <- testMessageRoundTripProperties
	unless success (exitWith (ExitFailure 1))

messageRoundTrip :: (Eq a, DM.ImmutableMessage a) => a -> Bool
messageRoundTrip a = (DM.decode . BS.fromStrict . DM.encode) a == Right (mempty, a)

instance Arbitrary Text where
	arbitrary = T.pack <$> arbitrary
	shrink t = T.pack <$> shrink (T.unpack t)

instance Arbitrary a => Arbitrary (Vector a) where
	arbitrary = V.fromList <$> arbitrary
	shrink v = V.fromList <$> shrink (V.toList v)

instance Enumerable a => Enumerable (Vector a) where
	enumerate = share . Shareable . fmap (fmap V.fromList) . runShared $ enumerate

instance Enumerable Text where
	enumerate = share . Shareable . fmap (fmap T.pack) . runShared $ enumerate

instance (Arbitrary k, Arbitrary v, Hashable k) => Arbitrary (HashMap k v) where
	arbitrary = M.fromList <$> arbitrary
	shrink v = M.fromList <$> shrink (M.toList v)

concat <$> traverse deriveEnumerable
	[''DM.ClientMessage, ''DM.ServerMessage]

instance Arbitrary DM.ClientMessage where arbitrary = sized uniform
instance Arbitrary DM.ServerMessage where arbitrary = sized uniform
