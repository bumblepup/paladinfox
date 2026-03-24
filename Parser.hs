{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Word
import qualified Control.Monad.Fail as MF
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS

newtype Parser a
  = Parser
    { runParser :: forall r. Buffer
                -> (Buffer -> Error -> Result r)
                -> (Buffer -> a -> Result r)
                -> Result r
    }

data Result a
  = Failure Buffer Error
  | Continue (BS.ByteString -> Result a)
  | Success Buffer a

data Buffer
  = Buffer
    { pos :: Pos
    , buf :: BS.ByteString
    , bak :: Maybe Buffer
    }

data Pos
  = Pos
    { offset :: Int
    } deriving (Eq)

data Error
  = Error
    { expected :: [String]
    , unexpected :: String
    } deriving (Show)

instance Semigroup a => Semigroup (Parser a) where
  (<>) = lift2 (<>)

instance Monoid a => Monoid (Parser a) where
  mempty = wrap mempty

instance Functor Parser where
  fmap = lift1

instance Applicative Parser where
  pure = wrap
  liftA2 = lift2

instance Alternative Parser where
  empty = err "empty"
  (<|>) = alt

instance Monad Parser where
  (>>=) = bind

instance MF.MonadFail Parser where
  fail = err

instance MonadPlus Parser

showerr :: Error -> String
showerr (Error es u) = unexp u ++ foldr1 (++) (map exp es)
  where
    unexp s = "Unexpected '" ++ s ++ "', expected:\n"
    exp s = "  - " ++ s ++ "\n"

parse :: Parser a -> Result a
parse k = runParser k buf Failure Success
  where buf = Buffer (Pos 0) BS.empty Nothing

alt :: Parser a -> Parser a -> Parser a
alt j k = Parser go
  where
    go b f s = runParser j b (go' (pos b) f s) s
    go' p' f s b e
      | pos b == p' = runParser k b f s
      | otherwise = f b e

try :: Parser a -> Parser a
try k = Parser go
  where
    go b f s = runParser k (push b) (gof f) (gos s)
    gof f b e = f (pop b) e
    gos s b r = s (drop b) r
    push b@(Buffer p bs _) = Buffer p bs $ Just b
    drop (Buffer _ _ Nothing) = error "try stack exceeded"
    drop (Buffer p bs (Just (Buffer _ _ bk))) = Buffer p bs bk
    pop (Buffer _ _ Nothing) = error "try stack exceeded"
    pop (Buffer _ _ (Just bk)) = bk

continue :: Parser Bool
continue = Parser go
  where
    go b _ s = Continue $ go' b s
    go' b s "" = s b False
    go' b s c = s (app c b) True
    app c (Buffer p bs bk) = Buffer p (app' c bs) $ fmap (app c) bk
    app' c "" = c
    app' c bs = BS.append bs c

ensure :: Int -> Parser Int
ensure n = do
  bs <- buffer
  case BS.length bs >= n of
    True -> pure n
    False -> do
      more <- continue
      case more of
        True -> ensure n
        False -> pure $ BS.length bs

advance :: Int -> Parser Int
advance n = Parser go
  where go b _ s = (uncurry.flip) s $ advanceBuf n b

advance_ :: Int -> Parser ()
advance_ n = () <$ advance n

advanceChunk :: Parser Int
advanceChunk = buffer >>= advance.BS.length

advanceChunk_ :: Parser ()
advanceChunk_ = () <$ advanceChunk

advanceBuf :: Int -> Buffer -> (Int, Buffer)
advanceBuf n (Buffer p bs bk)
  | l > n = (n, Buffer (advancePos n bs p) (BS.drop n bs) bk)
  | otherwise = (l, Buffer (advancePos l bs p) BS.empty bk)
  where l = BS.length bs

advancePos :: Int -> BS.ByteString -> Pos -> Pos
advancePos n _ (Pos k) = Pos $ k + n

wrap :: a -> Parser a
wrap x = Parser go
  where go b _ s = s b x

err :: String -> Parser a
err x = Parser go
  where go b f _ = f b $ Error [] x

seterr :: Error -> Parser a
seterr x = Parser go
  where go b f _ = f b x

position :: Parser Pos
position = Parser go
  where go b _ s = s b $ pos b

buffer :: Parser BS.ByteString
buffer = Parser go
  where go b _ s = s b $ buf b

bind :: Parser a -> (a -> Parser b) -> Parser b
bind k g = Parser go
  where
    go b f s = runParser k b f $ go' f s
    go' f s b r = runParser (g r) b f s

lift1 :: (a -> b) -> Parser a -> Parser b
lift1 g k = Parser go
  where
    go b f s = runParser k b f $ go' s
    go' s b r = s b $ g r

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 g k l = Parser go
  where
    go b f s = runParser k b f $ go' f s
    go' f s b r = runParser l b f $ go'' r s
    go'' r1 s b r2 = s b $ g r1 r2

-- primitive parser definitions

isEof :: Parser Bool
isEof = (0 ==) <$> ensure 1

eof :: Parser ()
eof = ensure 1 >>= g
  where
    g 0 = pure ()
    g _ = buffer >>= seterr . Error ["EOF"] . C8.unpack

takeString :: BS.ByteString -> Parser BS.ByteString
takeString st = do
  let l = BS.length st
  n <- ensure l
  case n < l of
    True -> seterr $ Error [] "EOF"
    False -> do
      b <- buffer
      case BS.isPrefixOf st b of
        False -> seterr $ Error [C8.unpack st] $ C8.unpack b
        True -> st <$ advance l

string :: BS.ByteString -> Parser ()
string st = () <$ takeString st

takeN :: (Word8 -> Bool) -> Int -> Parser BS.ByteString
takeN g n = do
  k <- ensure n
  case k < n of
    True -> seterr $ Error [] "EOF"
    False -> do
      b <- BS.take n <$> buffer
      case BS.all g b of
        False -> seterr $ Error ["takeN " ++ show n ++ " bytes"] $ C8.unpack b
        True -> b <$ advance n

takeWhile0 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile0 g = ensure 1 >> BS.concat <$> go
  where
    go = do
      b <- buffer
      case b of
        "" -> pure []
        _ -> case BS.findIndex (not.g) b of
          Nothing -> advance (BS.length b) >> continue >> (b:) <$> go
          Just n -> [BS.take n b] <$ advance n

takeWhile1 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile1 g = do
  (p, r, q) <- (,,) <$> position <*> takeWhile0 g <*> position
  if p == q then buffer >>= seterr . Error ["takeWhile1"] . C8.unpack else pure r

skipWhile0 :: (Word8 -> Bool) -> Parser ()
skipWhile0 g = ensure 1 >> go
  where
    go = do
      b <- buffer
      case b of
        "" -> pure ()
        _ -> case BS.findIndex (not.g) b of
          Nothing -> advance (BS.length b) >> continue >> go
          Just n -> advance_ n

skipWhile1 :: (Word8 -> Bool) -> Parser ()
skipWhile1 g = do
  (p, _, q) <- (,,) <$> position <*> skipWhile0 g <*> position
  if p == q then buffer >>= seterr . Error ["takeWhile1"] . C8.unpack else pure ()

-- basic combinators

takeMany :: Parser a -> Parser [a]
takeMany k = do
  x <- fmap Just k <|> pure Nothing
  case x of
    Nothing -> pure []
    Just y -> (y:) <$> takeMany k

skipMany :: Parser a -> Parser ()
skipMany k = do
  x <- fmap Just k <|> pure Nothing
  case x of
    Nothing -> pure ()
    Just _ -> skipMany k
