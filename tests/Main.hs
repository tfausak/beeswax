module Main
  ( main
  )
where

import qualified Beeswax
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.HUnit as HUnit

main :: IO HUnit.Counts
main = do
  let directory = "data"
  files <- Directory.listDirectory directory
  groups <- mapM (loadGroup . FilePath.combine directory)
    $ filter (hasExtension "json") files
  HUnit.runTestTT . HUnit.TestList . fmap groupToTest $ List.sortOn
    groupDescription
    groups

loadGroup :: FilePath -> IO Group
loadGroup filePath = do
  contents <- LazyBytes.readFile filePath
  either fail pure $ Aeson.eitherDecode contents

groupToTest :: Group -> HUnit.Test
groupToTest group = HUnit.TestLabel (groupDescription group) $ HUnit.TestList
  [ validsToTest (groupValid group)
  , decodeErrorsToTest (groupDecodeErrors group)
  ]

validsToTest :: [Valid] -> HUnit.Test
validsToTest =
  HUnit.TestList . fmap validToTest . List.sortOn validDescription

validToTest :: Valid -> HUnit.Test
validToTest valid =
  HUnit.TestLabel (validDescription valid) . HUnit.TestCase $ do
    let input = unwrapBytes $ validCanonicalBson valid
    case Beeswax.decodeObject input of
      Left problem ->
        HUnit.assertFailure $ "failed to decode valid BSON: " <> problem
      Right object -> HUnit.assertEqual "changed representation" input
        $ Beeswax.encodeObject object

decodeErrorsToTest :: [DecodeError] -> HUnit.Test
decodeErrorsToTest =
  HUnit.TestList . fmap decodeErrorToTest . List.sortOn decodeErrorDescription

decodeErrorToTest :: DecodeError -> HUnit.Test
decodeErrorToTest decodeError =
  HUnit.TestLabel (decodeErrorDescription decodeError)
    . HUnit.TestCase
    $ case Beeswax.decodeObject . unwrapBytes $ decodeErrorBson decodeError of
        Right _ -> HUnit.assertFailure "successfully decoded invalid BSON"
        Left _ -> pure ()

data Group = Group
  { groupDescription :: String
  , groupValid :: [Valid]
  , groupDecodeErrors :: [DecodeError]
  } deriving (Show)

instance Aeson.FromJSON Group where
  parseJSON =
    Aeson.withObject "Group" $ \object ->
      Group <$> requiredKey object "description" <*>
      optionalKeyWithDefault object "valid" [] <*>
      optionalKeyWithDefault object "decodeErrors" []

data Valid = Valid
  { validDescription :: String
  , validCanonicalBson :: Bytes
  } deriving (Show)

instance Aeson.FromJSON Valid where
  parseJSON =
    Aeson.withObject "Valid" $ \object ->
      Valid <$> requiredKey object "description" <*>
      requiredKey object "canonical_bson"

data DecodeError = DecodeError
  { decodeErrorDescription :: String
  , decodeErrorBson :: Bytes
  } deriving (Show)

instance Aeson.FromJSON DecodeError where
  parseJSON =
    Aeson.withObject "DecodeError" $ \object ->
      DecodeError <$> requiredKey object "description" <*>
      requiredKey object "bson"

newtype Bytes = Bytes
  { unwrapBytes :: Bytes.ByteString
  } deriving (Show)

instance Aeson.FromJSON Bytes where
  parseJSON =
    Aeson.withText "Bytes" $
    fmap (Bytes . Bytes.pack) . mapM toByte . Text.chunksOf 2

toByte :: Text.Text -> Aeson.Parser Word.Word8
toByte text =
  maybe (fail $ "invalid byte: " <> show text) (pure . intToWord8)
    $ fromBase 16 fromHexDigit text

fromHexDigit :: Char -> Maybe Int
fromHexDigit char =
  if Char.isHexDigit char then Just $ Char.digitToInt char else Nothing

intToWord8 :: Int -> Word.Word8
intToWord8 = fromIntegral

fromBase :: (Monad m, Num a) => a -> (Char -> m a) -> Text.Text -> m a
fromBase base convert = Text.foldl
  (\result char -> do
    number <- result
    digit <- convert char
    pure $ number * base + digit
  )
  (pure 0)

hasExtension :: String -> FilePath -> Bool
hasExtension extension filePath =
  FilePath.takeExtension filePath == '.' : extension

requiredKey
  :: Aeson.FromJSON json => Aeson.Object -> String -> Aeson.Parser json
requiredKey object key = object Aeson..: Text.pack key

optionalKey
  :: Aeson.FromJSON json => Aeson.Object -> String -> Aeson.Parser (Maybe json)
optionalKey object key = object Aeson..:? Text.pack key

optionalKeyWithDefault
  :: Aeson.FromJSON json => Aeson.Object -> String -> json -> Aeson.Parser json
optionalKeyWithDefault object key default_ =
  optionalKey object key Aeson..!= default_
