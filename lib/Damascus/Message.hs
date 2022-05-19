{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Damascus.Message (
	ImmutableMessage(..),
	Constrained(..),
	Message(..),
	abstraction,
	encode,
	decode,
	AtomicTextEdit(..),
	AtomicTextEditAction(..),
	AtomicSequenceEdit(..),
	AtomicSequenceEditAction(..),
	AtomicDictionaryEdit(..),
	ClientMessage(..),
	ServerMessage(..),

	-- * re-exports
	Int64,
	T.Text,
	Vector,
	CBOR.DeserialiseFailure(..),
	) where

import Damascus.Message.Internal
import Damascus.Message.TH

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Vector (Vector)

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified TextShow as TS

abstraction :: ImmutableMessage a => a -> TL.Text
abstraction = TS.toLazyText . abstractionC

encode :: ImmutableMessage a => a -> BS.ByteString
encode = CBOR.toStrictByteString . encodeC

decode :: ImmutableMessage a => LBS.ByteString -> Either CBOR.DeserialiseFailure (LBS.ByteString, a)
decode = CBOR.deserialiseFromBytes decodeC

-- This is split out into its own class so that the Template Haskell doesn't
-- have to figure out how to convert an English description into code.
-- TODO: implement "unconstrained" and maybe "recursivelyConstrained" in the
-- Template Haskell to write some of the more boring instances
class Constrained a where
	invalid :: a -> Bool

class (ImmutableMessage (DPatch a), ImmutableMessage a, Constrained a) => Message a where
	type DPatch a
	dpatch :: a -> DPatch a -> a

	-- TODO: type NPatch a; type N a; npatch :: a -> NPatch a -> N a
	-- this is hard: the vector instance wants to represent a vector of N a's,
	-- which means application of N (Vector a)'s need to actually do like
	--     npatch' :: N a -> NPatch a -> N a
	-- which leads you to want N to be sort of monad-y so that you can get this
	-- by just calling bind on npatch, but then that's hard because how do you
	-- apply a function to an Indeterminate, there's no way to know all the
	-- outputs it could have?

instance Constrained Int64 where
	invalid _ = False

instance Message Int64 where
	type DPatch Int64 = Int64
	dpatch = (+)

instance Constrained T.Text where
	invalid _ = False

instance Message T.Text where
	type DPatch T.Text = Vector AtomicTextEdit
	dpatch = V.foldl' astrpatch

instance Constrained a => Constrained (Vector a) where
	invalid = any invalid

instance Message a => Message (Vector a) where
	type DPatch (Vector a) = Vector (AtomicSequenceEdit a)
	dpatch = V.foldl' aseqpatch

instance (Constrained k, Constrained v) => Constrained (HashMap k v) where
	invalid = M.foldrWithKey (\k v a -> a || invalid k || invalid v) False

instance (Hashable k, ImmutableMessage k, Constrained k, Message v) => Message (HashMap k v) where
	type DPatch (HashMap k v) = HashMap k (AtomicDictionaryEdit k v)
	dpatch d p = M.mapMaybeWithKey apply p `M.union` (d `M.difference` p) where
		apply k = \case
			ADEDelete -> Nothing
			ADEInsert v -> Just v
			ADEModify  p' -> (`dpatch` p') <$> M.lookup k  d
			ADECopy k' p' -> (`dpatch` p') <$> M.lookup k' d

data AtomicTextEdit = AtomicTextEdit
	{ ateOffset :: Int64
	, ateAction :: AtomicTextEditAction
	} deriving (Eq, Ord, Read, Show)

data AtomicTextEditAction
	= ATEADelete Int64
	| ATEAInsert T.Text
	| ATEAReplace T.Text
	deriving (Eq, Ord, Read, Show)

astrpatch :: T.Text -> AtomicTextEdit -> T.Text
astrpatch t (AtomicTextEdit i d) = case d of
	ATEADelete  len -> b <> T.drop (fromIntegral len) e
	ATEAInsert  p   -> b <> p <> e
	ATEAReplace p   -> b <> p <> T.drop (T.length p) e
	where (b, e) = T.splitAt (fromIntegral i) t

instance ImmutableMessage AtomicTextEdit where
	abstractionC = undefined
	decodeC = undefined
	encodeC = undefined

data AtomicSequenceEdit a = AtomicSequenceEdit
	{ aseOffset :: Int64
	, aseAction :: AtomicSequenceEditAction a
	}

deriving instance (Eq   a, Eq   (DPatch a)) => Eq   (AtomicSequenceEdit a)
deriving instance (Ord  a, Ord  (DPatch a)) => Ord  (AtomicSequenceEdit a)
deriving instance (Read a, Read (DPatch a)) => Read (AtomicSequenceEdit a)
deriving instance (Show a, Show (DPatch a)) => Show (AtomicSequenceEdit a)

data AtomicSequenceEditAction a = ASEADelete Int64 | ASEAInsert (V.Vector a) | ASEAModify (V.Vector (DPatch a))

deriving instance (Eq   a, Eq   (DPatch a)) => Eq   (AtomicSequenceEditAction a)
deriving instance (Ord  a, Ord  (DPatch a)) => Ord  (AtomicSequenceEditAction a)
deriving instance (Read a, Read (DPatch a)) => Read (AtomicSequenceEditAction a)
deriving instance (Show a, Show (DPatch a)) => Show (AtomicSequenceEditAction a)

instance ImmutableMessage a => ImmutableMessage (AtomicSequenceEdit a) where
	abstractionC = undefined
	decodeC = undefined
	encodeC = undefined

aseqpatch :: Message a => V.Vector a -> AtomicSequenceEdit a -> V.Vector a
aseqpatch s (AtomicSequenceEdit i d) = case d of
	ASEADelete len -> b <> V.drop (fromIntegral len) e
	ASEAInsert p   -> b <> p <> e
	ASEAModify p   -> b <> V.zipWith dpatch e p <> V.drop (V.length p) e
	where (b, e) = V.splitAt (fromIntegral i) s

data AtomicDictionaryEdit k v
	= ADEDelete
	| ADEInsert v
	| ADEModify (DPatch v)
	| ADECopy k (DPatch v)

deriving instance (Eq   k, Eq   v, Eq   (DPatch v)) => Eq   (AtomicDictionaryEdit k v)
deriving instance (Ord  k, Ord  v, Ord  (DPatch v)) => Ord  (AtomicDictionaryEdit k v)
deriving instance (Read k, Read v, Read (DPatch v)) => Read (AtomicDictionaryEdit k v)
deriving instance (Show k, Show v, Show (DPatch v)) => Show (AtomicDictionaryEdit k v)

instance (ImmutableMessage k, Message v) => ImmutableMessage (AtomicDictionaryEdit k v) where
	abstractionC = undefined
	decodeC = undefined
	encodeC = undefined

declareCustomTypes $ tail [undefined
	, CustomType "server message" $ tail [undefined
		, Variant "propose version" 24 $ tail [undefined
			, Field (Sequence String) "" ""
			]
		]
	, CustomType "client message" $ tail [undefined
		, Variant "accept version" 24 $ tail [undefined
			, Field String "" ""
			]
		, Variant "reject all versions" 25 []
		]
	]
