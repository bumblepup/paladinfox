{-# LANGUAGE OverloadedStrings #-}

module Object where

import Parser
import Control.Applicative
import Data.Scientific
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import qualified Data.Text as TX
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml.Builder as YB

data Object
  = NullObj
  | RealObj Scientific
  | BoolObj Bool
  | NameObj BS.ByteString
  | StringObj BS.ByteString
  | HexObj BS.ByteString
  | ArrayObj [Object]
  | DictObj [(BS.ByteString, Object)]
  | RefObj Int Int
  deriving (Show)

serialize :: Object -> YB.YamlBuilder
serialize NullObj = YB.null
serialize (RealObj n) = YB.scientific n
serialize (BoolObj b) = YB.bool b
serialize (NameObj n) = YB.mapping [("n", YB.string $ decodeUtf8 n)]
serialize (StringObj s) = YB.mapping [("s", YB.string $ decodeUtf8 s)]
-- serialize (HexObj h) =
serialize (ArrayObj a) = YB.array $ map serialize a
serialize (DictObj d) = YB.mapping $ map ((k, v) -> (decodeUtf8 k, serialize v)) d
serialize (RefObj r g) = YB.alias $ TX.pack $ "ref" ++ show r ++ "-" ++ show g

isRegular, isDelimiter, isWhitespace :: Word8 -> Bool
isRegular x = BS.notElem x "\0\t\n\f\r ()<>[]{}/%"
isDelimiter x = BS.elem x "()<>[]{}/%"
isWhitespace x = BS.elem x "\0\t\n\f\r "

isHex, isDec, isOct :: Word8 -> Bool
isHex x = BS.elem x "0123456789ABCDEFabcdef"
isDec x = BS.elem x "0123456789"
isOct x = BS.elem x "01234567"

space :: Parser ()
space = skipMany $ skipWhile1 isWhitespace <|> comment
  where comment = string "%" >> skipWhile0 (flip BS.notElem "\n\r")

eol :: Parser ()
eol = string "\n" <|> string "\r\n" <|> string "\r"

keyword :: BS.ByteString -> Parser ()
keyword x = do
  string x
  y <- takeWhile0 isRegular
  if BS.null y then pure () else fail "expected keyword"

hexPair :: Word8 -> Word8 -> Word8
hexPair x y = fromIntegral (read ['0', 'x', BS.w2c x, BS.w2c y] :: Int)

octTrip :: Word8 -> Word8 -> Word8 -> Word8
octTrip x y z = fromIntegral (read ['0', 'o', BS.w2c x, BS.w2c y, BS.w2c z] :: Int)

readObject :: Parser Object
readObject = readObj <* space
  where
    readObj = readNestable <|> readStringlike <|> readKeyword <|> readNumeric
    readNestable = readDict <|> readArray
    readStringlike = readHex <|> readString <|> readName
    readKeyword = readNull <|> readTrue <|> readFalse
    readNumeric = readRef <|> readNumber
    readNull = NullObj <$ keyword "null"
    readTrue = BoolObj True <$ keyword "true"
    readFalse = BoolObj False <$ keyword "false"

readDict :: Parser Object
readDict = do
  string "<<" >> space
  elems <- takeMany $ do
    NameObj k <- readName <* space
    v <- readObject
    pure (k, v)
  string ">>"
  pure $ DictObj elems

readArray :: Parser Object
readArray = do
  string "[" >> space
  objs <- takeMany readObject
  string "]"
  pure $ ArrayObj objs

readName :: Parser Object
readName = string "/" >> NameObj . BS.concat <$> g
  where
    g = takeMany $ fmap f esc <|> takeWhile1 nonesc
    esc = string "#" >> takeN isHex 2
    nonesc x = x /= BS.c2w '#' && isRegular x
    f xs = let [x, y] = BS.unpack xs in BS.singleton $ hexPair x y

readString :: Parser Object
readString = string "(" >> StringObj . BS.concat <$> f 0
  where
    f x = do
      vs <- takeMany $ e <|> n <|> k
      (vs ++) <$> op x <|> cp x
    e = string "\\" >> esc
    n = "\n" <$ eol
    k = takeWhile1 $ flip BS.notElem "()\\\r\n"
    cp 0 = [] <$ string ")"
    cp n = (:) <$> takeString ")" <*> f (n - 1)
    op n = (:) <$> takeString "(" <*> f (n + 1)
    esc = foldr1 (<|>) [en, er, et, eb, ef, elp, erp, ebs, enl, eo, pure ""]
    en = "\n" <$ string "n"
    er = "\r" <$ string "r"
    et = "\t" <$ string "t"
    eb = "\b" <$ string "b"
    ef = "\f" <$ string "f"
    elp = "(" <$ string "("
    erp = ")" <$ string ")"
    ebs = "\\" <$ string "\\"
    enl = "" <$ eol
    eo = fmap (eo'.BS.unpack) $ foldr1 (<|>) $ map (takeN isOct) [3, 2, 1]
    eo' [z] = BS.singleton $ octTrip (BS.c2w '0') (BS.c2w '0') z
    eo' [y, z] = BS.singleton $ octTrip (BS.c2w '0') y z
    eo' [x, y, z] = BS.singleton $ octTrip x y z
    eo' _ = error "wrong number of octal digits"

readHex :: Parser Object
readHex = do
  k <- string "<" *> takeWhile0 g <* string ">"
  pure $ HexObj $ BS.pack $ f $ BS.unpack $ BS.filter isHex k
  where
    f [] = []
    f [x] = [hexPair x $ BS.c2w '0']
    f (x:y:xs) = hexPair x y:f xs
    g x = isHex x || isWhitespace x

readRef :: Parser Object
readRef = try $ do
  fstr <- read . C8.unpack <$> takeWhile1 isDec
  space
  sndr <- read . C8.unpack <$> takeWhile1 isDec
  space
  keyword "R"
  pure $ RefObj fstr sndr

readNumber :: Parser Object
readNumber = do
  b <- buffer
  sign <- takeString "-" <|> ("" <$ string "+") <|> pure ""
  fstn <- takeWhile0 isDec
  decp <- takeString "." <|> pure ""
  sndn <- takeWhile0 isDec
  case (BS.null fstn, BS.null decp, BS.null sndn) of
    (False, _, True) -> pure ()
    (_, False, False) -> pure ()
    _ -> seterr $ Error ["readNumber"] $ C8.unpack b
  pure $ RealObj $ read $ f [sign, g fstn, decp, g sndn]
  where
    f = C8.unpack . BS.concat
    g x = if BS.null x then "0" else x
