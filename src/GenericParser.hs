{-# LANGUAGE LambdaCase #-}

module GenericParser where

newtype Parser a = P {doParse :: String -> Maybe (a, String)}

-- get one char, if present, otherwise return Nothing
get :: Parser Char
get = P $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- >>> doParse get "hello"
-- Just ('h',"ello")


