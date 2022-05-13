{-# LANGUAGE LambdaCase #-}

module Damascus.Message.Internal where

import Control.Applicative
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Vector (Vector)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified TextShow as TS

-- | efficiently (C)omposable building blocks; see also their variants without C
class ImmutableMessage a where
	abstractionC :: a -> TS.Builder
	decodeC :: CBOR.Decoder s a
	encodeC :: a -> CBOR.Encoding

instance ImmutableMessage Int64 where
	abstractionC = TS.showb
	decodeC = CBOR.decodeInt64
	encodeC = CBOR.encodeInt64

instance ImmutableMessage T.Text where
	abstractionC = id
		. bracket '"' '"'
		. TS.fromText
		. T.replace (T.pack "\"") (T.pack "\\\"")
		. T.replace (T.pack "\\") (T.pack "\\\\")
	decodeC = CBOR.decodeString
	encodeC = CBOR.encodeString

instance ImmutableMessage a => ImmutableMessage (Vector a) where
	abstractionC = id
		. bracket '[' ']'
		. foldMap (abstractionC <> const TS.showbCommaSpace)
	decodeC = CBOR.decodeListLenOrIndef >>= \case
		Just n -> V.replicateM n decodeC
		Nothing -> V.unfoldrM go () where
			go _ = CBOR.decodeBreakOr >>= \case
				False -> fmap (\a -> Just (a, ())) decodeC
				True -> pure Nothing
	encodeC = (CBOR.encodeListLen . fromIntegral . V.length) <> foldMap encodeC

instance (Hashable k, ImmutableMessage k, ImmutableMessage v) => ImmutableMessage (HashMap k v) where
	abstractionC = id
		. bracket '{' '}'
		. getConst
		. M.traverseWithKey (\k v -> Const (abstractionC k <> TS.fromString ": " <> abstractionC v <> TS.showbCommaSpace))
	decodeC = CBOR.decodeMapLenOrIndef >>= \case
		Just n0 -> go n0 M.empty where
			go 0 m = pure m
			go n m = do
				k <- decodeC
				v <- decodeC
				go (n-1) (M.insert k v m)
		Nothing -> go M.empty where
			go m = CBOR.decodeBreakOr >>= \case
				True -> pure m
				False -> do
					k <- decodeC
					v <- decodeC
					go (M.insert k v m)
	encodeC = (CBOR.encodeMapLen . fromIntegral . M.size) <> M.foldMapWithKey (\k v -> encodeC k <> encodeC v)

bracket :: Char -> Char -> TS.Builder -> TS.Builder
bracket l r m = TS.singleton l <> m <> TS.singleton r
