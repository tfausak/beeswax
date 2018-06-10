module Beeswax
  ( version
  , decodeObject
  , encodeObject
  , Object(..)
  , unwrapObject
  , Pairs(..)
  , unwrapPairs
  , Pair(..)
  , Key(..)
  , unwrapKey
  , Value(..)
  , Tag(..)
  , ObjectId(..)
  , Binary(..)
  , Array(..)
  , unwrapArray
  , Regex(..)
  , DbPointer(..)
  , Quad(..)
  , unwrapQuad
  , Code(..)
  , Uuid(..)
  , Md5(..)
  , unwrapMd5
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Data.Word as Word
import qualified Paths_beeswax as This
import qualified Unsafe.Coerce as Unsafe

-- | The version number for this package.
--
-- >>> Version.Version branch tags = version
-- >>> null branch
-- False
-- >>> null tags
-- True
version :: Version.Version
version = This.version

decodeObject :: Bytes.ByteString -> Either String Object
decodeObject x = fst (runGet (getSized (Bytes.length x) getObject) x)

newtype Get a =
  Get (Bytes.ByteString -> (Either String a, Bytes.ByteString))

runGet :: Get a -> Bytes.ByteString -> (Either String a, Bytes.ByteString)
runGet (Get x) = x

instance Functor Get where
  fmap = fmapGet

fmapGet :: (a -> b) -> Get a -> Get b
fmapGet f p = Get
  (\b -> case runGet p b of
    (Left l, c) -> (Left l, c)
    (Right r, c) -> (Right (f r), c)
  )

instance Applicative Get where
  pure = pureGet
  (<*>) = apGet

pureGet :: a -> Get a
pureGet x = Get (\b -> (Right x, b))

apGet :: Get (a -> b) -> Get a -> Get b
apGet pf px = Get
  (\b -> case runGet pf b of
    (Left l, c) -> (Left l, c)
    (Right f, c) -> case runGet px c of
      (Left l, d) -> (Left l, d)
      (Right x, d) -> (Right (f x), d)
  )

instance Monad Get where
  (>>=) = bindGet
  fail = Fail.fail

bindGet :: Get a -> (a -> Get b) -> Get b
bindGet p f = Get
  (\b -> case runGet p b of
    (Left l, c) -> (Left l, c)
    (Right x, c) -> runGet (f x) c
  )

instance Fail.MonadFail Get where
  fail = failGet

failGet :: String -> Get a
failGet l = Get (\b -> (Left l, b))

get :: Get Bytes.ByteString
get = Get (\b -> (Right b, b))

put :: Bytes.ByteString -> Get ()
put b = Get (const (Right (), b))

encodeObject :: Object -> Bytes.ByteString
encodeObject = runPut putObject

type Put a = a -> Builder.Builder

runPut :: Put a -> a -> Bytes.ByteString
runPut f x = LazyBytes.toStrict (Builder.toLazyByteString (f x))

newtype Object =
  Object Pairs
  deriving (Eq, Show)

unwrapObject :: Object -> Pairs
unwrapObject (Object x) = x

getObject :: Get Object
getObject = do
  size <- getInt32
  pairs <- getSized (int32ToInt size - 4) getPairs
  pure (Object pairs)

putObject :: Put Object
putObject object = putSized (+ 4) putPairs (unwrapObject object)

newtype Pairs =
  Pairs [Pair]
  deriving (Eq, Show)

unwrapPairs :: Pairs -> [Pair]
unwrapPairs (Pairs x) = x

getPairs :: Get Pairs
getPairs = do
  maybeTag <- getMaybeTag
  case maybeTag of
    Nothing -> pure (Pairs [])
    Just tag -> do
      pair <- getPair tag
      pairs <- getPairs
      pure (Pairs (pair : unwrapPairs pairs))

putPairs :: Put Pairs
putPairs pairs = mappend
  (foldMap
    (\pair -> mappend (putTag (valueToTag (pairValue pair))) (putPair pair))
    (unwrapPairs pairs)
  )
  (Builder.word8 0x00)

data Tag
  = TagDouble
  | TagString
  | TagObject
  | TagArray
  | TagBinData
  | TagUndefined
  | TagObjectId
  | TagBool
  | TagDate
  | TagNull
  | TagRegex
  | TagDbPointer
  | TagJavaScript
  | TagSymbol
  | TagJavaScriptWithScope
  | TagInt
  | TagTimestamp
  | TagLong
  | TagDecimal
  | TagMaxKey
  | TagMinKey
  deriving (Eq, Show)

getMaybeTag :: Get (Maybe Tag)
getMaybeTag = do
  tag <- getWord8
  case tag of
    0x00 -> pure Nothing
    0x01 -> pure (Just TagDouble)
    0x02 -> pure (Just TagString)
    0x03 -> pure (Just TagObject)
    0x04 -> pure (Just TagArray)
    0x05 -> pure (Just TagBinData)
    0x06 -> pure (Just TagUndefined)
    0x07 -> pure (Just TagObjectId)
    0x08 -> pure (Just TagBool)
    0x09 -> pure (Just TagDate)
    0x0A -> pure (Just TagNull)
    0x0B -> pure (Just TagRegex)
    0x0C -> pure (Just TagDbPointer)
    0x0D -> pure (Just TagJavaScript)
    0x0E -> pure (Just TagSymbol)
    0x0F -> pure (Just TagJavaScriptWithScope)
    0x10 -> pure (Just TagInt)
    0x11 -> pure (Just TagTimestamp)
    0x12 -> pure (Just TagLong)
    0x13 -> pure (Just TagDecimal)
    0x7F -> pure (Just TagMaxKey)
    0xFF -> pure (Just TagMinKey)
    _ -> fail (mappend "invalid tag: " (show tag))

putTag :: Put Tag
putTag tag = Builder.word8
  (case tag of
    TagDouble -> 0x01
    TagString -> 0x02
    TagObject -> 0x03
    TagArray -> 0x04
    TagBinData -> 0x05
    TagUndefined -> 0x06
    TagObjectId -> 0x07
    TagBool -> 0x08
    TagDate -> 0x09
    TagNull -> 0x0A
    TagRegex -> 0x0B
    TagDbPointer -> 0x0C
    TagJavaScript -> 0x0D
    TagSymbol -> 0x0E
    TagJavaScriptWithScope -> 0x0F
    TagInt -> 0x10
    TagTimestamp -> 0x11
    TagLong -> 0x12
    TagDecimal -> 0x13
    TagMaxKey -> 0x7F
    TagMinKey -> 0xFF
  )

data Pair = Pair
  { pairKey :: Key
  , pairValue :: Value
  } deriving (Eq, Show)

getPair :: Tag -> Get Pair
getPair tag = do
  key <- getKey
  value <- getValue tag
  pure (Pair key value)

putPair :: Put Pair
putPair pair = mappend (putKey (pairKey pair)) (putValue (pairValue pair))

newtype Key =
  Key Text.Text
  deriving (Eq, Show)

unwrapKey :: Key -> Text.Text
unwrapKey (Key x) = x

getKey :: Get Key
getKey = fmap Key getCString

putKey :: Put Key
putKey key = putCString (unwrapKey key)

data Value
  = ValueDouble Double
  | ValueString Text.Text
  | ValueObject Object
  | ValueArray Array
  | ValueBinData Binary
  | ValueUndefined
  | ValueObjectId ObjectId
  | ValueBool Bool
  | ValueDate Int.Int64
  | ValueNull
  | ValueRegex Regex
  | ValueDbPointer DbPointer
  | ValueJavaScript Text.Text
  | ValueSymbol Text.Text
  | ValueJavaScriptWithScope Code
  | ValueInt Int.Int32
  | ValueTimestamp Word.Word64
  | ValueLong Int.Int64
  | ValueDecimal Quad
  | ValueMaxKey
  | ValueMinKey
  deriving (Eq, Show)

getValue :: Tag -> Get Value
getValue tag = case tag of
  TagDouble -> getValueDouble
  TagString -> getValueString
  TagObject -> getValueObject
  TagArray -> getValueArray
  TagBinData -> getValueBinData
  TagUndefined -> getValueUndefined
  TagObjectId -> getValueObjectId
  TagBool -> getValueBool
  TagDate -> getValueDate
  TagNull -> getValueNull
  TagRegex -> getValueRegex
  TagDbPointer -> getValueDbPointer
  TagJavaScript -> getValueJavaScript
  TagSymbol -> getValueSymbol
  TagJavaScriptWithScope -> getValueJavaScriptWithScope
  TagInt -> getValueInt
  TagTimestamp -> getValueTimestamp
  TagLong -> getValueLong
  TagDecimal -> getValueDecimal
  TagMaxKey -> getValueMaxKey
  TagMinKey -> getValueMinKey

putValue :: Put Value
putValue value = case value of
  ValueDouble x -> putValueDouble x
  ValueString x -> putValueString x
  ValueObject x -> putValueObject x
  ValueArray x -> putValueArray x
  ValueBinData x -> putValueBinData x
  ValueUndefined -> putValueUndefined ()
  ValueObjectId x -> putValueObjectId x
  ValueBool x -> putValueBool x
  ValueDate x -> putValueDate x
  ValueNull -> putValueNull ()
  ValueRegex x -> putValueRegex x
  ValueDbPointer x -> putValueDbPointer x
  ValueJavaScript x -> putValueJavaScript x
  ValueSymbol x -> putValueSymbol x
  ValueJavaScriptWithScope x -> putValueJavaScriptWithScope x
  ValueInt x -> putValueInt x
  ValueTimestamp x -> putValueTimestamp x
  ValueLong x -> putValueLong x
  ValueDecimal x -> putValueDecimal x
  ValueMaxKey -> putValueMaxKey ()
  ValueMinKey -> putValueMinKey ()

getValueDouble :: Get Value
getValueDouble = fmap (ValueDouble . word64ToDouble) getWord64

putValueDouble :: Put Double
putValueDouble = Builder.doubleLE

getValueString :: Get Value
getValueString = fmap ValueString getString

putValueString :: Put Text.Text
putValueString = putString

getValueObject :: Get Value
getValueObject = fmap ValueObject getObject

putValueObject :: Put Object
putValueObject = putObject

getValueArray :: Get Value
getValueArray = fmap ValueArray getArray

putValueArray :: Put Array
putValueArray = putArray

getValueBinData :: Get Value
getValueBinData = fmap ValueBinData getBinary

putValueBinData :: Put Binary
putValueBinData = putBinary

getValueUndefined :: Get Value
getValueUndefined = pure ValueUndefined

putValueUndefined :: Put ()
putValueUndefined () = mempty

getValueObjectId :: Get Value
getValueObjectId = fmap ValueObjectId getObjectId

putValueObjectId :: Put ObjectId
putValueObjectId = putObjectId

getValueBool :: Get Value
getValueBool = do
  byte <- getWord8
  case byte of
    0x00 -> pure (ValueBool False)
    0x01 -> pure (ValueBool True)
    _ -> fail (mappend "invalid bool: " (show byte))

putValueBool :: Put Bool
putValueBool x = Builder.word8 (if x then 0x01 else 0x00)

getValueDate :: Get Value
getValueDate = fmap ValueDate getInt64

putValueDate :: Put Int.Int64
putValueDate = Builder.int64LE

getValueNull :: Get Value
getValueNull = pure ValueNull

putValueNull :: Put ()
putValueNull () = mempty

getValueRegex :: Get Value
getValueRegex = fmap ValueRegex getRegex

putValueRegex :: Put Regex
putValueRegex = putRegex

getValueDbPointer :: Get Value
getValueDbPointer = fmap ValueDbPointer getDbPointer

putValueDbPointer :: Put DbPointer
putValueDbPointer = putDbPointer

getValueJavaScript :: Get Value
getValueJavaScript = fmap ValueJavaScript getString

putValueJavaScript :: Put Text.Text
putValueJavaScript = putString

getValueSymbol :: Get Value
getValueSymbol = fmap ValueSymbol getString

putValueSymbol :: Put Text.Text
putValueSymbol = putString

getValueJavaScriptWithScope :: Get Value
getValueJavaScriptWithScope = fmap ValueJavaScriptWithScope getCode

putValueJavaScriptWithScope :: Put Code
putValueJavaScriptWithScope = putCode

getValueInt :: Get Value
getValueInt = fmap ValueInt getInt32

putValueInt :: Put Int.Int32
putValueInt = Builder.int32LE

getValueTimestamp :: Get Value
getValueTimestamp = fmap ValueTimestamp getWord64

putValueTimestamp :: Put Word.Word64
putValueTimestamp = Builder.word64LE

getValueLong :: Get Value
getValueLong = fmap ValueLong getInt64

putValueLong :: Put Int.Int64
putValueLong = Builder.int64LE

getValueDecimal :: Get Value
getValueDecimal = fmap ValueDecimal getQuad

putValueDecimal :: Put Quad
putValueDecimal = putQuad

getValueMaxKey :: Get Value
getValueMaxKey = pure ValueMaxKey

putValueMaxKey :: Put ()
putValueMaxKey () = mempty

getValueMinKey :: Get Value
getValueMinKey = pure ValueMinKey

putValueMinKey :: Put ()
putValueMinKey () = mempty

data Code = Code
  { codeSource :: Text.Text
  , codeScope :: Object
  } deriving (Eq, Show)

getCode :: Get Code
getCode = do
  size <- getInt32
  getSized
    (int32ToInt size - 4)
    (do
      x <- getString
      y <- getObject
      pure (Code x y)
    )

putCode :: Put Code
putCode = putSized
  (+ 4)
  (\x -> mappend (putString (codeSource x)) (putObject (codeScope x)))

-- TODO: Use better representation.
newtype Quad =
  Quad Bytes.ByteString
  deriving (Eq, Show)

unwrapQuad :: Quad -> Bytes.ByteString
unwrapQuad (Quad x) = x

getQuad :: Get Quad
getQuad = fmap Quad (getBytes 16)

putQuad :: Put Quad
putQuad quad = Builder.byteString (unwrapQuad quad)

data DbPointer = DbPointer
  { dbPointerRef :: Text.Text
  , dbPointerId :: ObjectId
  } deriving (Eq, Show)

getDbPointer :: Get DbPointer
getDbPointer = do
  x <- getString
  y <- getObjectId
  pure (DbPointer x y)

putDbPointer :: Put DbPointer
putDbPointer x =
  mappend (putString (dbPointerRef x)) (putObjectId (dbPointerId x))

data Regex = Regex
  { regexPattern :: Text.Text
  , regexOptions :: Text.Text
  } deriving (Eq, Show)

getRegex :: Get Regex
getRegex = do
  x <- getCString
  y <- getCString
  pure (Regex x y)

putRegex :: Put Regex
putRegex x =
  mappend (putCString (regexPattern x)) (putCString (regexOptions x))

newtype Array =
  Array [Value]
  deriving (Eq, Show)

unwrapArray :: Array -> [Value]
unwrapArray (Array x) = x

getArray :: Get Array
getArray =
  fmap (Array . fmap pairValue . unwrapPairs . unwrapObject) getObject

putArray :: Put Array
putArray array = putObject
  (Object
    (Pairs
      (fmap
        (\(index, value) -> Pair (Key (Text.pack (show (index :: Word)))) value
        )
        (zip [0 ..] (unwrapArray array))
      )
    )
  )

data ObjectId = ObjectId
  { objectIdA :: Word.Word32
  , objectIdB :: Word.Word64
  } deriving (Eq, Show)

getObjectId :: Get ObjectId
getObjectId = do
  a <- getWord32BE
  b <- getWord64BE
  pure (ObjectId a b)

putObjectId :: Put ObjectId
putObjectId objectId = mappend
  (Builder.word32BE (objectIdA objectId))
  (Builder.word64BE (objectIdB objectId))

data Binary
  = BinaryGeneric Bytes.ByteString
  | BinaryFunction Bytes.ByteString
  | BinaryGenericOld Bytes.ByteString
  | BinaryUuidOld Uuid
  | BinaryUuid Uuid
  | BinaryMd5 Md5
  | BinaryUserDefined Word.Word8
                      Bytes.ByteString
  deriving (Eq, Show)

getBinary :: Get Binary
getBinary = do
  size <- getInt32
  subtype <- getWord8
  case subtype of
    0x00 -> fmap BinaryGeneric (getBytes (int32ToInt size))
    0x01 -> fmap BinaryFunction (getBytes (int32ToInt size))
    0x02 -> fmap
      BinaryGenericOld
      (getSized
        (int32ToInt size)
        (do
          innerSize <- getInt32
          getBytes (int32ToInt innerSize)
        )
      )
    0x03 -> fmap BinaryUuidOld (getSized (int32ToInt size) getUuid)
    0x04 -> fmap BinaryUuid (getSized (int32ToInt size) getUuid)
    0x05 -> fmap BinaryMd5 (getSized (int32ToInt size) getMd5)
    _ -> if subtype >= 0x80
      then fmap (BinaryUserDefined subtype) (getBytes (int32ToInt size))
      else fail (mappend "invalid subtype: " (show subtype))

putBinary :: Put Binary
putBinary = putSized
  (subtract 1)
  (\binary -> case binary of
    BinaryGeneric bytes ->
      mappend (Builder.word8 0x00) (Builder.byteString bytes)
    BinaryFunction bytes ->
      mappend (Builder.word8 0x01) (Builder.byteString bytes)
    BinaryGenericOld bytes ->
      mappend (Builder.word8 0x02) (putSized id Builder.byteString bytes)
    BinaryUuidOld uuid -> mappend (Builder.word8 0x03) (putUuid uuid)
    BinaryUuid uuid -> mappend (Builder.word8 0x04) (putUuid uuid)
    BinaryMd5 md5 -> mappend (Builder.word8 0x05) (putMd5 md5)
    BinaryUserDefined subtype bytes ->
      mappend (Builder.word8 subtype) (Builder.byteString bytes)
  )

data Uuid = Uuid
  { uuidA :: Word.Word32
  , uuidB :: Word.Word32
  , uuidC :: Word.Word32
  , uuidD :: Word.Word32
  } deriving (Eq, Show)

getUuid :: Get Uuid
getUuid = do
  a <- getWord32
  b <- getWord32
  c <- getWord32
  d <- getWord32
  pure (Uuid a b c d)

putUuid :: Put Uuid
putUuid uuid = mconcat
  [ Builder.word32LE (uuidA uuid)
  , Builder.word32LE (uuidB uuid)
  , Builder.word32LE (uuidC uuid)
  , Builder.word32LE (uuidD uuid)
  ]

newtype Md5 =
  Md5 Bytes.ByteString
  deriving (Eq, Show)

unwrapMd5 :: Md5 -> Bytes.ByteString
unwrapMd5 (Md5 x) = x

getMd5 :: Get Md5
getMd5 = fmap Md5 (getBytes 16)

putMd5 :: Put Md5
putMd5 md5 = Builder.byteString (unwrapMd5 md5)

getSized :: Int -> Get a -> Get a
getSized size f = do
  let getSize = fmap Bytes.length get
  before <- getSize
  result <- f
  after <- getSize
  if after + size == before
    then pure result
    else fail (mappend "unexpected leftovers: " (show size))

putSized :: (Int.Int32 -> Int.Int32) -> Put a -> Put a
putSized modifySize f x =
  let bytes = runPut f x
  in
    mappend
      (Builder.int32LE (modifySize (intToInt32 (Bytes.length bytes))))
      (Builder.byteString bytes)

getCString :: Get Text.Text
getCString = do
  bytes <- getUntil (== 0x00)
  _ <- getWord8
  decodeUtf8 bytes

putCString :: Put Text.Text
putCString text =
  mappend (Builder.stringUtf8 (Text.unpack text)) (Builder.word8 0x00)

decodeUtf8 :: Bytes.ByteString -> Get Text.Text
decodeUtf8 bytes = case Text.decodeUtf8' bytes of
  Left problem -> fail (mappend "invalid utf-8: " (show problem))
  Right text -> pure text

getString :: Get Text.Text
getString = do
  size <- getInt32
  bytes <- getBytes (int32ToInt size - 1)
  terminator <- getWord8
  if terminator == 0x00
    then decodeUtf8 bytes
    else fail (mappend "invalid string terminator: " (show terminator))

putString :: Put Text.Text
putString = putSized id putCString

getInt64 :: Get Int.Int64
getInt64 = fmap word64ToInt64 getWord64

getInt32 :: Get Int.Int32
getInt32 = fmap word32ToInt32 getWord32

getWord64BE :: Get Word.Word64
getWord64BE = fmap bytesToWord64BE (getBytes 8)

getWord32BE :: Get Word.Word32
getWord32BE = fmap bytesToWord32BE (getBytes 4)

getWord64 :: Get Word.Word64
getWord64 = fmap bytesToWord64 (getBytes 8)

getWord32 :: Get Word.Word32
getWord32 = fmap bytesToWord32 (getBytes 4)

getWord8 :: Get Word.Word8
getWord8 = fmap bytesToWord8 (getBytes 1)

getUntil :: (Word.Word8 -> Bool) -> Get Bytes.ByteString
getUntil predicate = do
  bytes <- get
  let (before, after) = Bytes.break predicate bytes
  put after
  pure before

getBytes :: Int -> Get Bytes.ByteString
getBytes size = do
  bytes <- get
  Monad.when (size < 0) (fail (mappend "size too small: " (show size)))
  Monad.when
    (size > Bytes.length bytes)
    (fail (mappend "size too large: " (show size)))
  put (Bytes.drop size bytes)
  pure (Bytes.take size bytes)

bytesToWord64BE :: Bytes.ByteString -> Word.Word64
bytesToWord64BE bytes =
  Bits.shiftL (word8ToWord64 (Bytes.index bytes 0)) 56
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 1)) 48
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 2)) 40
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 3)) 32
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 4)) 24
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 5)) 16
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 6)) 8
    Bits..|. word8ToWord64 (Bytes.index bytes 7)

bytesToWord32BE :: Bytes.ByteString -> Word.Word32
bytesToWord32BE bytes =
  Bits.shiftL (word8ToWord32 (Bytes.index bytes 0)) 24
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index bytes 1)) 16
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index bytes 2)) 8
    Bits..|. word8ToWord32 (Bytes.index bytes 3)

bytesToWord64 :: Bytes.ByteString -> Word.Word64
bytesToWord64 bytes =
  word8ToWord64 (Bytes.index bytes 0)
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 1)) 8
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 2)) 16
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 3)) 24
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 4)) 32
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 5)) 40
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 6)) 48
    Bits..|. Bits.shiftL (word8ToWord64 (Bytes.index bytes 7)) 56

bytesToWord32 :: Bytes.ByteString -> Word.Word32
bytesToWord32 bytes =
  word8ToWord32 (Bytes.index bytes 0)
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index bytes 1)) 8
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index bytes 2)) 16
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index bytes 3)) 24

bytesToWord8 :: Bytes.ByteString -> Word.Word8
bytesToWord8 bytes = Bytes.index bytes 0

word64ToDouble :: Word.Word64 -> Double
word64ToDouble = Unsafe.unsafeCoerce

word64ToInt64 :: Word.Word64 -> Int.Int64
word64ToInt64 = fromIntegral

word32ToInt32 :: Word.Word32 -> Int.Int32
word32ToInt32 = fromIntegral

word8ToWord64 :: Word.Word8 -> Word.Word64
word8ToWord64 = fromIntegral

word8ToWord32 :: Word.Word8 -> Word.Word32
word8ToWord32 = fromIntegral

intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral

int32ToInt :: Int.Int32 -> Int
int32ToInt = fromIntegral

valueToTag :: Value -> Tag
valueToTag value = case value of
  ValueDouble{} -> TagDouble
  ValueString{} -> TagString
  ValueObject{} -> TagObject
  ValueArray{} -> TagArray
  ValueBinData{} -> TagBinData
  ValueUndefined{} -> TagUndefined
  ValueObjectId{} -> TagObjectId
  ValueBool{} -> TagBool
  ValueDate{} -> TagDate
  ValueNull{} -> TagNull
  ValueRegex{} -> TagRegex
  ValueDbPointer{} -> TagDbPointer
  ValueJavaScript{} -> TagJavaScript
  ValueSymbol{} -> TagSymbol
  ValueJavaScriptWithScope{} -> TagJavaScriptWithScope
  ValueInt{} -> TagInt
  ValueTimestamp{} -> TagTimestamp
  ValueLong{} -> TagLong
  ValueDecimal{} -> TagDecimal
  ValueMaxKey{} -> TagMaxKey
  ValueMinKey{} -> TagMinKey
