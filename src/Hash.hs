module Hash
  ( compress,
    decompress,
    sha1HashWithOpenSSL,
    byteStringToText,
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import OpenSSL.EVP.Digest qualified as OpenSSL

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

-- Computes the SHA-1 hash of a strict ByteString using OpenSSL
sha1HashWithOpenSSL :: BS.ByteString -> IO String
sha1HashWithOpenSSL bs = do
  digest <- OpenSSL.digestBS OpenSSL.sha1 bs
  return $ concatMap (`showHex` "") $ BS.unpack digest
  where
    showHex :: (Integral a) => a -> String
    showHex n
      | n < 16 = '0' : showHex' n
      | otherwise = showHex' n
    showHex' = flip showIntAtBase 16 intToDigit

-- Converts a decompressed ByteString to Text
byteStringToText :: BS.ByteString -> Either String T.Text
byteStringToText bs =
  case TE.decodeUtf8' bs of
    Right text -> Right text
    Left err -> Left $ "Text decoding error: " ++ show err