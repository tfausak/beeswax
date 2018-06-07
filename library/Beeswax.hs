module Beeswax
  ( defaultMain
  , version
  , decodeObject
  , encodeObject
  , Object(..)
  , Pairs(..)
  , Pair(..)
  , Key(..)
  , Value(..)
  , Tag(..)
  , ObjectId(..)
  , Binary(..)
  , Array(..)
  , Regex(..)
  , DbPointer(..)
  , Quad(..)
  , Code(..)
  , Uuid(..)
  , Md5(..)
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
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Unsafe.Coerce as Unsafe

defaultMain :: IO ()
defaultMain = do
  IO.hPrint IO.stderr version
  [input, output] <- Environment.getArgs
  contents <- Bytes.readFile input
  case decodeObject contents of
    Left problem -> fail problem
    Right object -> Bytes.writeFile output (encodeObject object)

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

newtype Parser a = Parser
  { runParser :: Bytes.ByteString -> (Either String a, Bytes.ByteString)
  }

instance Functor Parser where
  fmap f p =
    Parser
      (\b ->
         case runParser p b of
           (Left l, c) -> (Left l, c)
           (Right r, c) -> (Right (f r), c))

instance Applicative Parser where
  pure x = Parser (\b -> (Right x, b))
  pf <*> px =
    Parser
      (\b ->
         case runParser pf b of
           (Left l, c) -> (Left l, c)
           (Right f, c) ->
             case runParser px c of
               (Left l, d) -> (Left l, d)
               (Right x, d) -> (Right (f x), d))

instance Monad Parser where
  p >>= f =
    Parser
      (\b ->
         case runParser p b of
           (Left l, c) -> (Left l, c)
           (Right x, c) -> runParser (f x) c)
  fail = Fail.fail

instance Fail.MonadFail Parser where
  fail l = Parser (\b -> (Left l, b))

get :: Parser Bytes.ByteString
get = Parser (\b -> (Right b, b))

put :: Bytes.ByteString -> Parser ()
put b = Parser (const (Right (), b))

type Get = Parser

runGet :: Get a -> Bytes.ByteString -> (Either String a, Bytes.ByteString)
runGet = runParser

encodeObject :: Object -> Bytes.ByteString
encodeObject = runPut putObject

type Put a = a -> Builder.Builder

runPut :: Put a -> a -> Bytes.ByteString
runPut f x = LazyBytes.toStrict (Builder.toLazyByteString (f x))

newtype Object = Object
  { unwrapObject :: Pairs
  } deriving (Eq, Show)

getObject :: Get Object
getObject = do
  size <- getInt32
  pairs <- getSized (int32ToInt size - 4) getPairs
  pure (Object pairs)

putObject :: Put Object
putObject object = putSized (+ 4) putPairs (unwrapObject object)

newtype Pairs = Pairs
  { unwrapPairs :: [Pair]
  } deriving (Eq, Show)

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
  = T01 -- double
  | T02 -- string
  | T03 -- object
  | T04 -- array
  | T05 -- bindata
  | T06 -- undefined
  | T07 -- objectid
  | T08 -- bool
  | T09 -- date
  | T0A -- null
  | T0B -- regex
  | T0C -- dbpointer
  | T0D -- javascript
  | T0E -- symbol
  | T0F -- javascriptwithscope
  | T10 -- int
  | T11 -- timestamp
  | T12 -- long
  | T13 -- decimal
  | T7F -- maxkey
  | TFF -- minkey
  deriving (Eq, Show)

getMaybeTag :: Get (Maybe Tag)
getMaybeTag = do
  tag <- getWord8
  case tag of
    0x00 -> pure Nothing
    0x01 -> pure (Just T01)
    0x02 -> pure (Just T02)
    0x03 -> pure (Just T03)
    0x04 -> pure (Just T04)
    0x05 -> pure (Just T05)
    0x06 -> pure (Just T06)
    0x07 -> pure (Just T07)
    0x08 -> pure (Just T08)
    0x09 -> pure (Just T09)
    0x0A -> pure (Just T0A)
    0x0B -> pure (Just T0B)
    0x0C -> pure (Just T0C)
    0x0D -> pure (Just T0D)
    0x0E -> pure (Just T0E)
    0x0F -> pure (Just T0F)
    0x10 -> pure (Just T10)
    0x11 -> pure (Just T11)
    0x12 -> pure (Just T12)
    0x13 -> pure (Just T13)
    0x7F -> pure (Just T7F)
    0xFF -> pure (Just TFF)
    _ -> fail (mappend "invalid tag: " (show tag))

putTag :: Put Tag
putTag tag = Builder.word8
  (case tag of
    T01 -> 0x01
    T02 -> 0x02
    T03 -> 0x03
    T04 -> 0x04
    T05 -> 0x05
    T06 -> 0x06
    T07 -> 0x07
    T08 -> 0x08
    T09 -> 0x09
    T0A -> 0x0A
    T0B -> 0x0B
    T0C -> 0x0C
    T0D -> 0x0D
    T0E -> 0x0E
    T0F -> 0x0F
    T10 -> 0x10
    T11 -> 0x11
    T12 -> 0x12
    T13 -> 0x13
    T7F -> 0x7F
    TFF -> 0xFF
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

newtype Key = Key
  { unwrapKey :: Text.Text
  } deriving (Eq, Show)

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
  T01 -> getValueDouble
  T02 -> getValueString
  T03 -> getValueObject
  T04 -> getValueArray
  T05 -> getValueBinData
  T06 -> getValueUndefined
  T07 -> getValueObjectId
  T08 -> getValueBool
  T09 -> getValueDate
  T0A -> getValueNull
  T0B -> getValueRegex
  T0C -> getValueDbPointer
  T0D -> getValueJavaScript
  T0E -> getValueSymbol
  T0F -> getValueJavaScriptWithScope
  T10 -> getValueInt
  T11 -> getValueTimestamp
  T12 -> getValueLong
  T13 -> getValueDecimal
  T7F -> getValueMaxKey
  TFF -> getValueMinKey

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

newtype Quad = Quad
  { unwrapQuad :: Bytes.ByteString -- TODO: Use better representation.
  } deriving (Eq, Show)

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

newtype Array = Array
  { unwrapArray :: [Value]
  } deriving (Eq, Show)

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

-- TODO: Is this right, or does it need to be big-endian?
data ObjectId = ObjectId
  { objectIdA :: Word.Word32
  , objectIdB :: Word.Word64
  } deriving (Eq, Show)

getObjectId :: Get ObjectId
getObjectId = do
  a <- getWord32
  b <- getWord64
  pure (ObjectId a b)

putObjectId :: Put ObjectId
putObjectId objectId = mappend
  (Builder.word32LE (objectIdA objectId))
  (Builder.word64LE (objectIdB objectId))

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

newtype Md5 = Md5
  { unwrapMd5 :: Bytes.ByteString
  } deriving (Eq, Show)

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
  ValueDouble{} -> T01
  ValueString{} -> T02
  ValueObject{} -> T03
  ValueArray{} -> T04
  ValueBinData{} -> T05
  ValueUndefined{} -> T06
  ValueObjectId{} -> T07
  ValueBool{} -> T08
  ValueDate{} -> T09
  ValueNull{} -> T0A
  ValueRegex{} -> T0B
  ValueDbPointer{} -> T0C
  ValueJavaScript{} -> T0D
  ValueSymbol{} -> T0E
  ValueJavaScriptWithScope{} -> T0F
  ValueInt{} -> T10
  ValueTimestamp{} -> T11
  ValueLong{} -> T12
  ValueDecimal{} -> T13
  ValueMaxKey{} -> T7F
  ValueMinKey{} -> TFF
