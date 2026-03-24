{-# LANGUAGE OverloadedStrings #-}

module Filter where

import Object
import Parser
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

data Filter
  = Failed BS.ByteString Error
  | Consume (BS.ByteString -> Filter)
  | Supply BS.ByteString Filter
  | Completed BS.ByteString

composeFilter :: Filter -> Filter -> Filter
composeFilter _ y@(Failed _ _) = y
composeFilter (Failed _ e) (Completed c) = Failed c e
composeFilter (Completed _) y@(Completed _) = y
composeFilter (Consume f) y@(Completed _) = composeFilter (f BS.empty) y
composeFilter (Consume f) (Supply c y) = composeFilter (f c) y
composeFilter x@(Consume _) (Consume g) = Consume (\c -> composeFilter x $ g c)
composeFilter (Supply c x) y = Supply c $ composeFilter x y
composeFilter (Failed _ e) _ = Failed BS.empty e
composeFilter _ _ = Failed BS.empty $ Error [] "filter stream has trailing bytes"

applyFilter :: Filter -> Parser a -> Parser a
applyFilter filt k = go True (parse k) filt
  where
    go n _ (Failed c e) = prep n c >> seterr e
    go n (Failure _ e) (Completed c) = prep n c >> seterr e
    go n (Success _ r) (Completed c) = prep n c >> pure r
    go n (Continue g) t@(Completed _) = go n (g BS.empty) t
    go n (Continue g) (Supply c t) = go n (g c) t
    go True p@(Continue _) (Consume h) = buffer >>= go False p.h
    go False p@(Continue _) (Consume h) = do
      b <- advanceChunk >> continue >> buffer
      go False p $ h b
    go n (Failure _ e) _ = eat n >> seterr e
    go n _ _ = eat n >> fail "filter stream has trailing bytes"
    eat True = pure ()
    eat False = advanceChunk_
    prep True "" = pure ()
    prep True _ = error "unfed filter has buffer"
    prep False c = do
      b <- buffer
      case BS.isSuffixOf c b of
        True -> advance_ $ BS.length b - BS.length c
        False -> error "filter buffer differs from parse buffer"

applyFilters :: [Filter] -> Parser a -> Parser a
applyFilters [] x = x
applyFilters fs x = applyFilter (foldr1 composeFilter fs) x

-- filter definitions

data Easy a = FFail String | FMore [Word8] a | FDone [Word8]

easyFilter :: (Maybe Word8 -> a -> Easy a) -> a -> Filter
easyFilter f accum = go 0 BS.empty [] accum
  where
    go l is os ac
      | BS.null is = Consume $ c l os ac
      | length os >= l = h os $ go l is [] ac
      | otherwise = case f (Just $ BS.head is) ac of
          FFail e -> g os $ Failed is $ Error [] e
          FDone x -> g (i x os) $ Completed is
          FMore x a -> go l (BS.tail is) (i x os) a
    c l os ac is
      | BS.null is = case f Nothing ac of
          FFail e -> g os $ Failed is $ Error [] e
          FDone x -> g (i x os) $ Completed is
          FMore x a -> c l (i x os) a is
      | l <= 0 = go (BS.length is) is os ac
      | otherwise = go (div (l + BS.length is) 2) is os ac
    g x = if null x then id else h x
    h x = Supply $ BS.pack $ reverse x
    i [] k = k
    i (x:xs) k = i xs (x:k)

hexFilter :: Maybe Word8 -> Maybe Word8 -> Easy (Maybe Word8)
hexFilter Nothing _ = FFail "unexpected end of stream"
hexFilter (Just y) x
  | isWhitespace y = FMore [] x
  | y == BS.c2w '>' = case x of
      Nothing -> FDone []
      Just x' -> FDone [hexPair x' $ BS.c2w '0']
hexFilter (Just y) _ | not $ isHex y = FFail "unexpected character in stream"
hexFilter (Just y) Nothing = FMore [] $ Just y
hexFilter (Just y) (Just x) = FMore [hexPair x y] Nothing

base85Filter :: Maybe Word8 -> [Word8] -> Easy [Word8]
base85Filter Nothing _ = FFail "unexpected end of stream"
base85Filter (Just y) (x:xs)
  | x == BS.c2w '~' && y == BS.c2w '>' = case base85 xs of
      Nothing -> FFail "improper base85 encoding"
      Just k -> FDone k
  | x == BS.c2w '~' = FFail "unexpected character in stream"
base85Filter (Just y) xs
  | isWhitespace y = FMore [] xs
  | y == BS.c2w '~' = FMore [] $ y:xs
  | y == BS.c2w 'z' && null xs = FMore [0, 0, 0, 0] xs
  | y >= BS.c2w '!' && y <= BS.c2w 'u' = if length xs < 4
    then FMore [] $ y:xs
    else case base85 $ y:xs of
      Nothing -> FFail "improper base85 encoding"
      Just k -> FMore k []
  | otherwise = FFail "unexpected character in stream"

base85 :: [Word8] -> Maybe [Word8]
base85 [] = Just []
base85 [a, b] = take 1 <$> base85 [0, 0, 0, a, b]
base85 [a, b, c] = take 2 <$> base85 [0, 0, a, b, c]
base85 [a, b, c, d] = take 3 <$> base85 [0, a, b, c, d]
base85 xs@[_, _, _, _, _] = if k >= 2^32 then Nothing else Just [w, x, y, z]
  where
    k = sum $ zipWith (\n m -> (85^m)*(fromIntegral n - 33)) xs [0..] :: Int
    w = fromIntegral $ mod x' 256
    (x, x') = (fromIntegral $ mod y' 256, div y' 256)
    (y, y') = (fromIntegral $ mod z' 256, div z' 256)
    (z, z') = (fromIntegral $ mod k 256, div k 256)
base85 _ = Nothing
