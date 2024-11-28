module FileIO (readFileAsByteString, writeFileFromByteString) where

import Data.ByteString qualified as BS

-- Reads a file from the given file path into a strict ByteString
readFileAsByteString :: FilePath -> IO BS.ByteString
readFileAsByteString filePath = BS.readFile filePath

-- Writes a strict ByteString to a file at the given file path
writeFileFromByteString :: FilePath -> BS.ByteString -> IO ()
writeFileFromByteString filePath content = BS.writeFile filePath content

