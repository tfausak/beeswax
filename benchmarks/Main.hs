module Main ( main ) where

import qualified Beeswax
import qualified Criterion.Main as Criterion
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Bson as Bson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Bson.Binary as Bson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

main :: IO ()
main = Criterion.defaultMain
  [ Criterion.bgroup "decode"
    [ let bytes = Bytes.pack [ 0x05, 0x00, 0x00, 0x00, 0x00 ] in Criterion.bgroup "nothing"
      [ Criterion.bench "beeswax" $ Criterion.whnf decodeBeeswax bytes
      , Criterion.bench "bson" $ Criterion.whnf decodeBson bytes
      ]
    , let bytes = Bytes.pack [ 0xF4, 0x01, 0x00, 0x00, 0x07, 0x5F, 0x69, 0x64, 0x00, 0x57, 0xE1, 0x93, 0xD7, 0xA9, 0xCC, 0x81, 0xB4, 0x02, 0x74, 0x98, 0xB5, 0x02, 0x53, 0x74, 0x72, 0x69, 0x6E, 0x67, 0x00, 0x07, 0x00, 0x00, 0x00, 0x73, 0x74, 0x72, 0x69, 0x6E, 0x67, 0x00, 0x10, 0x49, 0x6E, 0x74, 0x33, 0x32, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x12, 0x49, 0x6E, 0x74, 0x36, 0x34, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x44, 0x6F, 0x75, 0x62, 0x6C, 0x65, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0xBF, 0x05, 0x42, 0x69, 0x6E, 0x61, 0x72, 0x79, 0x00, 0x10, 0x00, 0x00, 0x00, 0x03, 0xA3, 0x4C, 0x38, 0xF7, 0xC3, 0xAB, 0xED, 0xC8, 0xA3, 0x78, 0x14, 0xA9, 0x92, 0xAB, 0x8D, 0xB6, 0x05, 0x42, 0x69, 0x6E, 0x61, 0x72, 0x79, 0x55, 0x73, 0x65, 0x72, 0x44, 0x65, 0x66, 0x69, 0x6E, 0x65, 0x64, 0x00, 0x05, 0x00, 0x00, 0x00, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05, 0x0D, 0x43, 0x6F, 0x64, 0x65, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x66, 0x75, 0x6E, 0x63, 0x74, 0x69, 0x6F, 0x6E, 0x28, 0x29, 0x20, 0x7B, 0x7D, 0x00, 0x0F, 0x43, 0x6F, 0x64, 0x65, 0x57, 0x69, 0x74, 0x68, 0x53, 0x63, 0x6F, 0x70, 0x65, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x66, 0x75, 0x6E, 0x63, 0x74, 0x69, 0x6F, 0x6E, 0x28, 0x29, 0x20, 0x7B, 0x7D, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x03, 0x53, 0x75, 0x62, 0x64, 0x6F, 0x63, 0x75, 0x6D, 0x65, 0x6E, 0x74, 0x00, 0x12, 0x00, 0x00, 0x00, 0x02, 0x66, 0x6F, 0x6F, 0x00, 0x04, 0x00, 0x00, 0x00, 0x62, 0x61, 0x72, 0x00, 0x00, 0x04, 0x41, 0x72, 0x72, 0x61, 0x79, 0x00, 0x28, 0x00, 0x00, 0x00, 0x10, 0x30, 0x00, 0x01, 0x00, 0x00, 0x00, 0x10, 0x31, 0x00, 0x02, 0x00, 0x00, 0x00, 0x10, 0x32, 0x00, 0x03, 0x00, 0x00, 0x00, 0x10, 0x33, 0x00, 0x04, 0x00, 0x00, 0x00, 0x10, 0x34, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x11, 0x54, 0x69, 0x6D, 0x65, 0x73, 0x74, 0x61, 0x6D, 0x70, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x0B, 0x52, 0x65, 0x67, 0x65, 0x78, 0x00, 0x70, 0x61, 0x74, 0x74, 0x65, 0x72, 0x6E, 0x00, 0x00, 0x09, 0x44, 0x61, 0x74, 0x65, 0x74, 0x69, 0x6D, 0x65, 0x45, 0x70, 0x6F, 0x63, 0x68, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x44, 0x61, 0x74, 0x65, 0x74, 0x69, 0x6D, 0x65, 0x50, 0x6F, 0x73, 0x69, 0x74, 0x69, 0x76, 0x65, 0x00, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x09, 0x44, 0x61, 0x74, 0x65, 0x74, 0x69, 0x6D, 0x65, 0x4E, 0x65, 0x67, 0x61, 0x74, 0x69, 0x76, 0x65, 0x00, 0x00, 0x00, 0x00, 0x80, 0xFF, 0xFF, 0xFF, 0xFF, 0x08, 0x54, 0x72, 0x75, 0x65, 0x00, 0x01, 0x08, 0x46, 0x61, 0x6C, 0x73, 0x65, 0x00, 0x00, 0x03, 0x44, 0x42, 0x52, 0x65, 0x66, 0x00, 0x3D, 0x00, 0x00, 0x00, 0x02, 0x24, 0x72, 0x65, 0x66, 0x00, 0x0B, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x6C, 0x6C, 0x65, 0x63, 0x74, 0x69, 0x6F, 0x6E, 0x00, 0x07, 0x24, 0x69, 0x64, 0x00, 0x57, 0xFD, 0x71, 0xE9, 0x6E, 0x32, 0xAB, 0x42, 0x25, 0xB7, 0x23, 0xFB, 0x02, 0x24, 0x64, 0x62, 0x00, 0x09, 0x00, 0x00, 0x00, 0x64, 0x61, 0x74, 0x61, 0x62, 0x61, 0x73, 0x65, 0x00, 0x00, 0xFF, 0x4D, 0x69, 0x6E, 0x6B, 0x65, 0x79, 0x00, 0x7F, 0x4D, 0x61, 0x78, 0x6B, 0x65, 0x79, 0x00, 0x0A, 0x4E, 0x75, 0x6C, 0x6C, 0x00, 0x00 ] in Criterion.bgroup "everything"
      [ Criterion.bench "beeswax" $ Criterion.whnf decodeBeeswax bytes
      , Criterion.bench "bson" $ Criterion.whnf decodeBson bytes
      ]
    ]
  , Criterion.bgroup "encode"
    [ Criterion.bgroup "nothing"
      [ Criterion.bench "beeswax" . Criterion.whnf encodeBeeswax . Beeswax.Object $ Beeswax.Pairs []
      , Criterion.bench "bson" $ Criterion.whnf encodeBson []
      ]
    , Criterion.bgroup "everything"
      [ Criterion.bench "beeswax" . Criterion.whnf encodeBeeswax . Beeswax.Object $ Beeswax.Pairs
        [ Beeswax.Pair (Beeswax.Key (Text.pack "1")) (Beeswax.ValueDouble 0)
        , Beeswax.Pair (Beeswax.Key (Text.pack "2")) (Beeswax.ValueString Text.empty)
        , Beeswax.Pair (Beeswax.Key (Text.pack "3")) (Beeswax.ValueObject (Beeswax.Object (Beeswax.Pairs [])))
        , Beeswax.Pair (Beeswax.Key (Text.pack "4")) (Beeswax.ValueArray (Beeswax.Array []))
        , Beeswax.Pair (Beeswax.Key (Text.pack "5")) (Beeswax.ValueBinData (Beeswax.BinaryGeneric Bytes.empty))
        -- , Beeswax.Pair (Beeswax.Key (Text.pack "6")) Beeswax.ValueUndefined
        , Beeswax.Pair (Beeswax.Key (Text.pack "7")) (Beeswax.ValueObjectId (Beeswax.ObjectId 0 0))
        , Beeswax.Pair (Beeswax.Key (Text.pack "8")) (Beeswax.ValueBool False)
        , Beeswax.Pair (Beeswax.Key (Text.pack "9")) (Beeswax.ValueDate 0)
        , Beeswax.Pair (Beeswax.Key (Text.pack "10")) Beeswax.ValueNull
        , Beeswax.Pair (Beeswax.Key (Text.pack "11")) (Beeswax.ValueRegex (Beeswax.Regex Text.empty Text.empty))
        -- , Beeswax.Pair (Beeswax.Key (Text.pack "12")) (Beeswax.ValueDBPointer Text.empty (Bytes.replicate 12 0))
        -- , Beeswax.Pair (Beeswax.Key (Text.pack "13")) (Beeswax.ValueJavaScript Text.empty)
        , Beeswax.Pair (Beeswax.Key (Text.pack "14")) (Beeswax.ValueSymbol Text.empty)
        , Beeswax.Pair (Beeswax.Key (Text.pack "15")) (Beeswax.ValueJavaScriptWithScope (Beeswax.Code Text.empty (Beeswax.Object (Beeswax.Pairs []))))
        , Beeswax.Pair (Beeswax.Key (Text.pack "16")) (Beeswax.ValueInt 0)
        , Beeswax.Pair (Beeswax.Key (Text.pack "17")) (Beeswax.ValueTimestamp 0)
        , Beeswax.Pair (Beeswax.Key (Text.pack "18")) (Beeswax.ValueLong 0)
        -- , Beeswax.Pair (Beeswax.Key (Text.pack "19")) (Beeswax.ValueDecimal (Bytes.replicate 16 0))
        , Beeswax.Pair (Beeswax.Key (Text.pack "20")) Beeswax.ValueMaxKey
        , Beeswax.Pair (Beeswax.Key (Text.pack "21")) Beeswax.ValueMinKey
        ]
      , Criterion.bench "bson" $ Criterion.whnf encodeBson
        [ Text.pack "1" Bson.:= Bson.Float 0
        , Text.pack "2" Bson.:= Bson.String Text.empty
        , Text.pack "3" Bson.:= Bson.Doc []
        , Text.pack "4" Bson.:= Bson.Array []
        -- , Text.pack "5" Bson.:= Bson.Bin (Bson.Binary Bytes.empty)
        -- , Text.pack "5" Bson.:= Bson.Fun (Bson.Function Bytes.empty)
        -- , Text.pack "5" Bson.:= Bson.Uuid (Bson.UUID Bytes.empty)
        -- , Text.pack "5" Bson.:= Bson.Md5 (Bson.MD5 Bytes.empty)
        , Text.pack "5" Bson.:= Bson.UserDef (Bson.UserDefined Bytes.empty)
        , Text.pack "7" Bson.:= Bson.ObjId (Bson.Oid 0 0)
        , Text.pack "8" Bson.:= Bson.Bool False
        , Text.pack "9" Bson.:= Bson.UTC (Time.UTCTime (Time.fromGregorian 1970 1 1) 0)
        , Text.pack "10" Bson.:= Bson.Null
        , Text.pack "11" Bson.:= Bson.RegEx (Bson.Regex Text.empty Text.empty)
        , Text.pack "14" Bson.:= Bson.Sym (Bson.Symbol Text.empty)
        , Text.pack "15" Bson.:= Bson.JavaScr (Bson.Javascript [] Text.empty)
        , Text.pack "16" Bson.:= Bson.Int32 0
        , Text.pack "17" Bson.:= Bson.Stamp (Bson.MongoStamp 0)
        , Text.pack "18" Bson.:= Bson.Int64 0
        , Text.pack "20" Bson.:= Bson.MinMax Bson.MaxKey
        , Text.pack "21" Bson.:= Bson.MinMax Bson.MinKey
        ]
      ]
    ]
  ]

decodeBeeswax :: Bytes.ByteString -> Beeswax.Object
decodeBeeswax = either error id . Beeswax.decodeObject

decodeBson :: Bytes.ByteString -> Bson.Document
decodeBson = Binary.runGet Bson.getDocument . LazyBytes.fromStrict

encodeBeeswax :: Beeswax.Object -> Bytes.ByteString
encodeBeeswax = Beeswax.encodeObject

encodeBson :: Bson.Document -> Bytes.ByteString
encodeBson = LazyBytes.toStrict . Binary.runPut . Bson.putDocument