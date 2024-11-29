module FileIO (readFileAsByteString, writeFileFromByteString) where

import Data.ByteString qualified as BS

-- alias for BS.readFile
readFileAsByteString :: FilePath -> IO BS.ByteString
readFileAsByteString = BS.readFile

-- alias for BS.writeFile
writeFileFromByteString :: FilePath -> BS.ByteString -> IO ()
writeFileFromByteString = BS.writeFile

