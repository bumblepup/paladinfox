{-# LANGUAGE OverloadedStrings #-}

module Section where

import Object
import Parser
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

-- toplevel: ordered list
-- Sections:
-- + Header:
--     PDF: <version string>
-- + Trailer:
--     trailer: <object>
--   + trailers are all combined into one
--   + xref sheets are combined to allow cleanup, then ignored
-- + Body:
--     obj: &ref<refidx>-<genidx> <object>
--   + may contain `data:' entry
--
-- Objects:
-- + null: null
-- + int: <integer>
-- + real: <float>
-- + bool: <bool>
-- + name: <string that starts with `/'>
-- + string: <string>
-- + hex: <hex string that starts with `0x'>
-- + array: <list>
-- + dict: <map>
-- + ref: *ref<refidx>-<genidx>
-- + data: <file name containing raw decoded stream data>

data Section
  = HeaderSect BS.ByteString
  | TrailerSect Int Object
  | XRefSect [XRefEntry]
  | BodySect Int Int Object (Maybe StreamData)
  deriving (Show)

data XRefEntry
  = XRefAlloc
    { refidx :: Int
    , refoffset :: Int
    , generation :: Int
    }
  | XRefFree
    { refidx :: Int
    , nextfree :: Int
    , generation :: Int
    }
  deriving (Show)

type StreamData = BS.ByteString

readPDF :: Parser [Section]
readPDF = do
  h <- readHeader
  cs <- takeMany $ readBody <|> xref <|> readTrailer
  eof
  pure $ h:cs
  where xref = XRefSect <$> readXRef <* space

readHeader :: Parser Section
readHeader = do
  string "%PDF-"
  maj <- takeWhile1 isDec
  string "."
  min <- takeWhile1 isDec
  eol >> space
  pure $ HeaderSect $ BS.concat [maj, ".", min]

readTrailer :: Parser Section
readTrailer = do
  keyword "trailer" >> space
  x <- readObject
  string "startxref" >> eol
  y <- read . C8.unpack <$> takeWhile1 isDec
  eol
  string "%%EOF"
  eol <|> eof
  space
  pure $ TrailerSect y x

readBody :: Parser Section
readBody = do
  x <- read . C8.unpack <$> takeWhile1 isDec
  space
  y <- read . C8.unpack <$> takeWhile1 isDec
  space
  keyword "obj" >> space
  d <- readObject
  s <- fmap Just (readStream d) <|> pure Nothing
  keyword "endobj" >> space
  pure $ BodySect x y d s

-- readStream :: Object -> Parser StreamData
-- readStream d = do
--   string "stream"
--   string "\n" <|> string "\r\n"
--   x <- -- TODO read the stream
--   eol <|> pure ()
--   string "endstream" >> space
--   pure x

readStream :: Object -> Parser StreamData
readStream d = do
  string "stream"
  string "\n" <|> string "\r\n"
  x <- takeUntil "endstream"
  eol <|> pure ()
  string "endstream" >> space
  pure x

takeUntil :: BS.ByteString -> Parser StreamData
takeUntil s = do
  b <- buffer
  case BS.breakSubstring s b of
    (_, "") -> continue >> takeUntil s
    (x, _) -> x <$ advance_ (BS.length x)

readXRef :: Parser [XRefEntry]
readXRef = do
  string "xref" >> eol
  fmap concat $ takeMany $ do
    idx <- read . C8.unpack <$> takeWhile1 isDec
    string " "
    ct <- read . C8.unpack <$> takeWhile1 isDec
    eol
    readEntries idx ct

readEntries :: Int -> Int -> Parser [XRefEntry]
readEntries _ 0 = pure []
readEntries idx ct = do
  offs <- read . C8.unpack <$> takeN isDec 10
  string " "
  gen <- read . C8.unpack <$> takeN isDec 5
  string " "
  typ <- takeString "n" <|> takeString "f"
  string " \n" <|> string " \r" <|> string "\r\n"
  let res = (if typ == "n" then XRefAlloc else XRefFree) idx offs gen
  (res:) <$> readEntries (idx + 1) (ct - 1)
