module Hash
  ( compress,
    decompress,
    sha1Hash,
    byteStringToText,
    textToByteString,
    stringToByteString
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text

-- Compresses a strict ByteString using zlib
compress :: BS.ByteString -> BS.ByteString
compress = BL.toStrict . Zlib.compress . BL.fromStrict

-- Decompresses a strict ByteString using zlib
decompress :: BS.ByteString -> Either String BS.ByteString
decompress bs =
  case safeDecompress bs of
    Left err -> Left $ "Decompression error: " ++ err
    Right result -> Right $ BL.toStrict result
  where
    safeDecompress input = Right (Zlib.decompress $ BL.fromStrict input)

-- | Pads a single hex digit with a leading zero if necessary
padZero :: String -> String
padZero s = if length s == 1 then '0' : s else s

-- | Computes the SHA-1 hash of a strict ByteString and returns it as a hexadecimal String
sha1Hash :: BS.ByteString -> String
sha1Hash bs = concatMap (padZero . (`showHex` "")) (BS.unpack $ SHA1.hash bs)

-- Converts a ByteString to Text using UTF-8 decoding with error handling
byteStringToText :: BS.ByteString -> Either String T.Text
byteStringToText bs =
  case TE.decodeUtf8' bs of
    Right text -> Right text
    Left err -> Left $ "Text decoding error: " ++ show err

-- alias for TE.encodeUtf8
textToByteString :: T.Text -> BS.ByteString
textToByteString = TE.encodeUtf8

-- string to text and text to bytestring
stringToByteString :: String -> BS.ByteString
stringToByteString = TE.encodeUtf8.Data.Text.pack