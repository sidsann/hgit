module Hash
  ( compress,
    decompress,
    sha1Hash,
    byteStringToText,
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Crypto.Hash.SHA1 qualified as SHA1
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)

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

-- Computes the SHA-1 hash of a strict ByteString
sha1Hash :: BS.ByteString -> String
sha1Hash bs = concatMap (`showHex` "") $ BS.unpack $ SHA1.hash bs

-- Converts a decompressed ByteString to Text
byteStringToText :: BS.ByteString -> Either String T.Text
byteStringToText bs =
  case TE.decodeUtf8' bs of
    Right text -> Right text
    Left err -> Left $ "Text decoding error: " ++ show err