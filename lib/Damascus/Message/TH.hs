{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Damascus.Message.TH where

import Damascus.Message.Internal

import Control.Applicative
import Control.Monad
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified TextShow as TS

data DamascusType
	= I64
	| String
	| Sequence DamascusType
	| Dictionary DamascusType DamascusType
	| Custom String
	deriving (Eq, Ord, Read, Show)

data Field = Field
	{ fType :: DamascusType
	, fConstraintDescription :: String
	, fMeaning :: String
	} deriving (Eq, Ord, Read, Show)

data Variant = Variant
	{ vAbstraction :: String
	, vTag :: Int64
	, vFields :: [Field]
	} deriving (Eq, Ord, Read, Show)

data CustomType = CustomType
	{ cName :: String
	, cVariants :: [Variant]
	} deriving (Eq, Ord, Read, Show)

data CustomTypeSummary = CustomTypeSummary
	{ sName :: Name
	, sNewtype :: Maybe (DamascusType, DamascusType) -- ^ the immediately wrapped type and the fully, recursively unwrapped type
	, sUnsummary :: CustomType
	} deriving (Eq, Ord, Show)

type Env = HashMap String CustomTypeSummary

createEnv :: [CustomType] -> Q (HashMap String CustomTypeSummary)
createEnv tyList = do
	let tys = M.fromList [(cName c, c) | c <- tyList]
	    newtypes = flip fmap tys $ \case
	    	CustomType { cVariants = [Variant { vFields = [Field { fType = ty }] }] } -> Just . (,) ty $ case ty of
	    		Custom nm -> case M.lookup nm newtypes of
	    			Just (Just (_, ty')) -> ty'
	    			_ -> ty
	    		_ -> ty
	    	_ -> Nothing
	names <- traverse (newTyCon . cName) tys
	pure $ CustomTypeSummary <$> names !<*> newtypes !<*> tys
	where
	-- not <*> lol
	infixl 4 !<*>
	(!<*>) = M.intersectionWith ($)

(!) :: MonadFail m => Env -> String -> m CustomTypeSummary
env ! s = case M.lookup s env of
	Nothing -> fail $ "Requested type summary for " ++ s ++ ", but that did not occur in the environment. The environment had " ++ intercalate ", " (M.keys env) ++ "."
	Just cts -> pure cts

sanitizeTyCon :: MonadFail m => String -> m String
sanitizeTyCon s = case s of
	[] -> die "empty name"
	_ -> go (' ':s)
	where
	go (' ':'Δ':cs) = ('D':) <$> go cs
	go (' ':c:cs)
		| simple c = (toUpper c:) <$> go cs
		| otherwise = die $ "don't know how to convert " ++ [c] ++ " to an upper-case letter"
	go (c:cs)
		| simple c = (c:) <$> go cs
		| otherwise = die $ "invalid name component " ++ [c]
	go [] = pure []

	simple c = isAlpha c && isAscii c
	die err = fail $ "Error while converting " ++ s ++ " to constructor name: " ++ err

newTyCon :: String -> Q Name
newTyCon = sanitizeTyCon >=> newName

datConPrefix :: MonadFail m => String -> m String
datConPrefix = fmap (filter isUpper) . sanitizeTyCon

sanitizeDatCon :: MonadFail m => String -> String -> m String
sanitizeDatCon ty da = liftA2 (++) (datConPrefix ty) (sanitizeTyCon da)

newDatCon :: String -> String -> Q Name
newDatCon ty = sanitizeDatCon ty >=> newName

reflectTyCon :: MonadFail m => Env -> String -> m Name
reflectTyCon env s = sName <$> env ! s

reflectDamascusType :: Env -> DamascusType -> Q Type
reflectDamascusType env = go where
	go = \case
		I64 -> [t|Int64|]
		String -> [t|Text|]
		Sequence ty -> [t|Vector $(go ty)|]
		Dictionary k v -> [t|HashMap $(go k) $(go v)|]
		Custom s -> ConT <$> reflectTyCon env s

downgradeBang :: MonadFail m => Env -> Bang -> DamascusType -> m Bang
downgradeBang _ original@(Bang NoSourceUnpackedness _) = \_ -> pure original
downgradeBang env original@(Bang _ strictness) = \case
	I64 -> pure original
	String -> downgraded
	Sequence _ -> downgraded
	Dictionary _ _ -> downgraded
	Custom s -> do
		nt <- sNewtype <$> env ! s
		case nt of
			Nothing -> downgraded
			Just (_, wrapped) -> downgradeBang env original wrapped
	where
	downgraded = pure $ Bang NoSourceUnpackedness strictness

reflectField :: Env -> Bang -> Field -> Q BangType
reflectField env b f = liftA2 (,)
	(downgradeBang env b (fType f))
	(reflectDamascusType env (fType f))

reflectVariant :: Env -> String -> Bang -> Variant -> Q Con
reflectVariant env tyPre b v = pure NormalC
	<*> newDatCon tyPre (vAbstraction v)
	<*> traverse (reflectField env b) (vFields v)

reflectCustomType :: Env -> CustomType -> Q Dec
reflectCustomType env c = do
	s <- env ! cName c
	let (style, b) = case sNewtype s of
	    	Just _ -> (\ctx nm tyvars k [con] -> NewtypeD ctx nm tyvars k con, Bang NoSourceUnpackedness NoSourceStrictness)
	    	Nothing -> (DataD, Bang SourceUnpack SourceStrict)
	tyPre <- sanitizeTyCon (cName c)
	cons <- traverse (reflectVariant env tyPre b) (cVariants c)
	pure $ style [] (sName s) [] Nothing cons [DerivClause Nothing (ConT <$> [''Eq, ''Ord, ''Read, ''Show])]

reflectCustomTypes :: [CustomType] -> Q [Dec]
reflectCustomTypes tys = do
	env <- createEnv tys
	traverse (reflectCustomType env) tys

matchVariant :: (Variant -> [Name] -> Q Exp) -> String -> Variant -> Q Match
matchVariant body tyPre v = do
	con <- sanitizeDatCon tyPre (vAbstraction v)
	fields <- traverse (\_ -> newName "x") (vFields v)
	b <- body v fields
	pure $ Match (ConP (mkName con) (map VarP fields)) (NormalB b) []

matchCustomType :: (Variant -> [Name] -> Q Exp) -> CustomType -> Q Exp
matchCustomType body c = lamCaseE (matchVariant body (cName c) <$> cVariants c)

abstractVariant :: Variant -> [Name] -> Q Exp
abstractVariant v fields = case fields of
	[] -> abstractCon
	_ -> [e| $abstractCon <> bracket '(' ')' (mconcat (intersperse TS.showbCommaSpace $(
	     	listE [[e| abstractionC $(varE field)|] | field <- fields]
	     )))|]
	where
	abstractCon = [e| TS.fromText (T.pack $(lift (vAbstraction v))) |]

abstractCustomType :: CustomType -> Q Exp
abstractCustomType = matchCustomType abstractVariant

encodeVariant :: CustomType -> Variant -> [Name] -> Q Exp
encodeVariant c v fields = case items of
	[i] -> i
	_ -> [e| mconcat
		$ CBOR.encodeListLen $(lift $ (fromIntegral $ length items :: Word))
		: $(listE items)
		|]
	where
	tag = vTag v
	items = []
		++ [[e| CBOR.encodeInt64 tag |] | not . null . drop 1 . cVariants $ c]
		++ [[e| encodeC $(varE f) |] | f <- fields]

encodeCustomType :: CustomType -> Q Exp
encodeCustomType c = matchCustomType (encodeVariant c) c

data PeekResult = ListToken | I64Token | OtherToken
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

peekResult :: CBOR.TokenType -> PeekResult
peekResult = \case
	CBOR.TypeListLen -> ListToken
	CBOR.TypeListLen64 -> ListToken
	CBOR.TypeListLenIndef -> ListToken
	CBOR.TypeUInt -> I64Token
	CBOR.TypeUInt64 -> I64Token
	CBOR.TypeNInt -> I64Token
	CBOR.TypeNInt64 -> I64Token
	_ -> OtherToken

-- callers assume that this returns a BindS (VarP _) _
decodeField :: Field -> Q Stmt
decodeField _ = bindS (varP =<< newName "x") [e|decodeC|]

intE :: Integral a => a -> Q Exp
intE = litE . integerL . toInteger

decodeMultifield :: String -> [Variant] -> Q Exp
decodeMultifield nm vs = [e|do
	len <- CBOR.decodeListLenOrIndef
	tag <- CBOR.decodeInt64
	(n, cons, result) <- $(caseE (varE 'tag) $
		[ match
			(litP (IntegerL (toInteger (vTag v))))
			(normalB . appE [e|fmap ((,,) $(intE (length (vFields v) + 1)) $(litE (stringL (vAbstraction v))))|] $ foldl'
				(\con _ -> [e|$con <*> decodeC|])
				(appE (varE 'pure) (conE . mkName =<< sanitizeDatCon nm (vAbstraction v)))
				(vFields v)
			)
			[]
		| v <- vs
		] ++
		[match wildP
			(normalB [e|fail $ "unknown multifield variant tag " ++ show tag ++ " for " ++ $nmE|])
			[]
		])
	result <$ case len of
		Nothing -> do
			done <- CBOR.decodeBreakOr
			unless done . fail $ "too many fields while decoding " ++ $nmE ++ "/" ++ cons
		Just n'
			| n /= n' -> fail $ "expected " ++ show n ++ " fields while decoding " ++ $(litE $ StringL nm) ++ "/" ++ cons ++ ", but found " ++ show n'
			| otherwise -> pure ()
	|]
	where
	nmE = litE $ StringL nm

decodeFieldless :: String -> [Variant] -> Q Exp
decodeFieldless nm vs = [e|do
	tag <- CBOR.decodeInt64
	$(caseE (varE 'tag) $
		[ match
			(litP (IntegerL (toInteger (vTag v))))
			(normalB (appE (varE 'pure) (conE . mkName =<< sanitizeDatCon nm (vAbstraction v))))
			[]
		| v <- vs
		] ++
		[match wildP
			(normalB [e|fail $ "unknown fieldless variant tag " ++ show tag ++ " for " ++ $(litE $ StringL nm)|])
			[]
		])
	|]

decodeCustomType :: CustomType -> Q Exp
decodeCustomType c = case cVariants c of
	[v] -> traverse decodeField (vFields v) >>= \case
		[_] -> [e| $(datConE v) <$> decodeC |]
		stmts -> do
			start@(BindS (VarP indef) _) <- bindS (varP =<< newName "indef") (varE 'CBOR.decodeListLenOrIndef)
			datCon <- datConE v
			let result = foldl' (\f (BindS (VarP var) _) -> AppE f (VarE var)) datCon stmts
			stop <- noBindS [e|$(pure result) <$ case $(varE indef) of
				Nothing -> do
					done <- CBOR.decodeBreakOr
					unless done $ fail $(litE . StringL $ "too many fields while decoding " ++ cName c ++ "/" ++ vAbstraction v)
				Just n | n /= $(litE . IntegerL . toInteger $ length stmts) -> fail $ $(litE . StringL $ "expected " ++ show (length stmts) ++ " fields while decoding " ++ cName c ++ "/" ++ vAbstraction v ++ ", but found ") ++ show n
				       | otherwise -> pure ()
				|]
			pure . DoE $ []
				++ [start]
				++ stmts
				++ [stop]
	vs -> case partition fieldless vs of
		([], _) -> decodeMultifield nm vs
		(_, []) -> decodeFieldless nm vs
		(less, multi) -> [e|do
			tty <- CBOR.peekTokenType
			case peekResult tty of
				ListToken -> $(decodeMultifield nm multi)
				I64Token -> $(decodeFieldless nm less)
				OtherToken -> fail $ "expected a list or bare variant tag, but saw strange token type " ++ show tty ++ " instead"
			|]
	where
	datConE v = conE . mkName =<< sanitizeDatCon (cName c) (vAbstraction v)
	fieldless v = null (vFields v)
	nm = cName c

immutableMessageCustomType :: CustomType -> Q [Dec]
immutableMessageCustomType c = [d|
	instance ImmutableMessage $(ConT . mkName <$> sanitizeTyCon (cName c)) where
		abstractionC = $(abstractCustomType c)
		encodeC = $(encodeCustomType c)
		decodeC = $(decodeCustomType c)
	|]

declareCustomTypes :: [CustomType] -> Q [Dec]
declareCustomTypes cs = do
	tys <- reflectCustomTypes cs
	ims <- traverse immutableMessageCustomType cs
	pure (concat (tys:ims))
