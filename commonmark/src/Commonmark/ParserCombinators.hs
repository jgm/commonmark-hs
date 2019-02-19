{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Commonmark.ParserCombinators
  ( SourcePos(..)
  , initialPos
  , incSourceLine
  , incSourceColumn
  , HasSourcePos(..)
  , ParseError(..)
  , renderParseError
  , renderParseErrors
  , ParserT
  , runParserT
  , Parser
  , runParser
  , satisfy
  , getPosition
  , tokensWhile
  , tokensWhile1
  , skipWhile
  , count
  , manyTill
  , between
  , option
  , notFollowedBy
  , lookAhead
  , peekBehind
  , eof
  , withRaw
  , (<?>)
  , raiseParseErrors
  , setInput
  , getInput
  , putState
  , getState
  , updateState
  , module Control.Applicative
  ) where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Functor.Identity (Identity, runIdentity)

data SourcePos = SourcePos
     { sourceName   :: String
     , sourceLine   :: !Int
     , sourceColumn :: !Int }
     deriving (Eq, Ord, Data, Typeable)

instance Show SourcePos where
  show pos = sourceName pos ++ " line " ++ show (sourceLine pos) ++
              " column " ++ show (sourceColumn pos)

initialPos :: String -> SourcePos
initialPos name = SourcePos name 1 1
{-# INLINABLE initialPos #-}

incSourceLine :: SourcePos -> Int -> SourcePos
incSourceLine (SourcePos name line col) n = SourcePos name (line + n) col
{-# INLINABLE incSourceLine #-}

incSourceColumn :: SourcePos -> Int -> SourcePos
incSourceColumn (SourcePos name line col) n = SourcePos name line (col + n)
{-# INLINABLE incSourceColumn #-}

class HasSourcePos t where
  tokenSourcePos :: t -> SourcePos

data ParseError t =
    UnexpectedToken t
  | UnexpectedEOF
  | ParseFailure String
  | UnexpectedPrecedingToken t
  | ExpectedPrecedingToken
  | ExpectedEOF
  | Expected String
  deriving Show

renderParseError :: Show t => ParseError t -> String
renderParseError e =
  case e of
    UnexpectedToken t -> "Unexpected token: " ++ show t
    UnexpectedEOF -> "Unexpected end of input"
    ParseFailure s -> s
    UnexpectedPrecedingToken t -> "Unexpected preceding token: " ++ show t
    ExpectedPrecedingToken -> "Expected a preceding token"
    ExpectedEOF -> "Expected end of input"
    Expected s -> "Expected " ++ s

renderParseErrors :: Show t => [ParseError t] -> String
renderParseErrors = unlines . map renderParseError

data PState u t = PState
     { tokens    :: [t]
     , lastToken :: Maybe t
     , userState :: !u
     }

newtype ParserT t u m a = ParserT
  { unParserT :: PState u t -> m (Either [ParseError t] (a, PState u t)) }

instance MonadTrans (ParserT t u) where
    lift amb = ParserT $ \st -> do
                 a <- amb
                 return $ Right (a, st)

instance Monad m => Functor (ParserT t u m) where
  fmap f (ParserT parser) =
    ParserT $ \st -> do
      res <- parser st
      case res of
        Right (x, newst) -> return $ Right (f x, newst)
        Left err         -> return $ Left err
  {-# INLINE fmap #-}

instance Monad m => Applicative (ParserT t u m) where
  pure x = ParserT $ \st -> return $ Right (x, st)
  fp <*> xp = ParserT $ \st -> do
    res <- unParserT fp st
    case res of
      Right (f, newst) -> unParserT (f <$> xp) newst
      Left err         -> return $ Left err
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (ParserT t u m) where
  empty = ParserT $ \_ -> return $ Left mempty
  ParserT a <|> ParserT b = ParserT $ \st -> do
    res <- a st
    case res of
      Right (x, newst) -> return $ Right (x, newst)
      Left err -> do
        res' <- b st
        case res' of
          Left err'          -> return $ Left (err <> err')
          Right (x', newst') -> return $ Right (x', newst')
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ParserT t u m) where
  return = pure
  ParserT a >>= f = ParserT $ \st -> do
    res <- a st
    case res of
      Left err -> return $ Left err
      Right (x, newst) -> unParserT (f x) newst
  fail = Control.Monad.Fail.fail
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  {-# INLINE fail #-}

instance Monad m => MonadPlus (ParserT t u m)

instance Monad m => MonadFail (ParserT t u m) where
  fail msg = ParserT $ \_ -> return $ Left [ParseFailure msg]
  {-# INLINE fail #-}

runParserT :: Monad m
           => ParserT t u m a -> u -> [t] -> m (Either [ParseError t] a)
runParserT (ParserT parser) uState toks =
  fmap fst <$> parser PState{ tokens = toks
                            , lastToken = Nothing
                            , userState = uState }

type Parser t u a = ParserT t u Identity a

runParser :: Parser t u a -> u -> [t] -> Either [ParseError t] a
runParser p u toks = runIdentity (runParserT p u toks)

satisfy :: Monad m => (t -> Bool) -> ParserT t u m t
satisfy f = ParserT $ \st ->
  return $
    case tokens st of
      t:ts | f t  -> Right (t, st{ tokens = ts, lastToken = Just t })
      t:_         -> Left [UnexpectedToken t]
      []          -> Left [UnexpectedEOF]
{-# INLINABLE satisfy #-}

getPosition :: (Monad m, HasSourcePos t) => ParserT t u m SourcePos
getPosition = ParserT $ \st ->
  return $
    case tokens st of
      t:_ -> Right (tokenSourcePos t, st)
      []  -> case lastToken st of
               Just t  -> let pos = tokenSourcePos t
                          in  Right (pos{ sourceLine = sourceLine pos + 1
                                        , sourceColumn = 1 }, st)
               Nothing -> Right (initialPos "", st)
{-# INLINABLE getPosition #-}

tokensWhile :: Monad m => (t -> Bool) -> ParserT t u m [t]
tokensWhile f = ParserT $ \st ->
  let (xs, ys) = span f (tokens st)
  in  return $
        Right (xs, st{ tokens = ys, lastToken = if null ys
                                                   then Nothing
                                                   else Just (last ys) })
{-# INLINABLE tokensWhile #-}

tokensWhile1 :: Monad m => (t -> Bool) -> ParserT t u m [t]
tokensWhile1 f = ParserT $ \st ->
  case tokens st of
    []           -> return $ Left [UnexpectedEOF]
    (t:ts) | f t -> return $ Right (t:xs, st{ tokens = ys
                                            , lastToken =
                                                if null ys
                                                   then Nothing
                                                   else Just (last ys) })
                        where (xs, ys) = span f ts
    (t:_)        ->  return $ Left [UnexpectedToken t]
{-# INLINABLE tokensWhile1 #-}

skipWhile :: Monad m => (t -> Bool) -> ParserT t u m ()
skipWhile f = ParserT $ \st ->
  let (_, ys) = span f (tokens st)
  in  return $
        Right ((), st{ tokens = ys, lastToken = if null ys
                                                   then Nothing
                                                   else Just (last ys) })
{-# INLINABLE skipWhile #-}

count :: Monad m => Int -> ParserT t u m a -> ParserT t u m [a]
count n pa = sequence $ replicate n pa
{-# INLINABLE count #-}

manyTill :: Monad m
         => ParserT t u m a -> ParserT t u m b -> ParserT t u m [a]
manyTill pa pb = many (notFollowedBy pb *> pa) <* pb
{-# INLINABLE manyTill #-}

between :: Monad m
        => ParserT t u m a -> ParserT t u m b -> ParserT t u m c
        -> ParserT t u m c
between popen pclose pin = popen *> pin <* pclose
{-# INLINABLE between #-}

option :: Monad m => a -> ParserT t u m a -> ParserT t u m a
option defaultValue (ParserT parser) = ParserT $ \st -> do
  res <- parser st
  case res of
    Right (x, newst) -> return $ Right (x, newst)
    Left _           -> return $ Right (defaultValue, st)
{-# INLINABLE option #-}

notFollowedBy :: Monad m => ParserT t u m a -> ParserT t u m ()
notFollowedBy (ParserT parser) = ParserT $ \st -> do
  res <- parser st
  case res of
    Left _  -> return $ Right ((), st)
    Right _ ->
      case tokens st of
        t:_ -> return $ Left [UnexpectedToken t]
        []  -> return $ Left [UnexpectedEOF]
{-# INLINABLE notFollowedBy #-}

lookAhead :: Monad m => ParserT t u m a -> ParserT t u m a
lookAhead (ParserT parser) = ParserT $ \st -> do
  res <- parser st
  case res of
    Left errs    -> return $ Left errs
    Right (x, _) -> return $ Right (x, st)
{-# INLINABLE lookAhead #-}

peekBehind :: Monad m => ParserT t u m (Maybe t)
peekBehind = ParserT $ \st -> return (Right (lastToken st, st))
{-# INLINABLE peekBehind #-}

eof :: Monad m => ParserT t u m ()
eof = ParserT $ \st ->
  return $
    case tokens st of
      [] -> Right ((), st)
      _  -> Left [ExpectedEOF]
{-# INLINABLE eof #-}

withRaw :: Monad m => ParserT t u m a -> ParserT t u m (a, [t])
withRaw (ParserT parser) = ParserT $ \st -> do
  res <- parser st
  case res of
    Left errs       -> return $ Left errs
    Right (x,newst) -> return $ Right ((x, toksParsed), newst)
      where toksParsed = take (length (tokens st) - length (tokens newst))
                             (tokens st)
{-# INLINABLE withRaw #-}

(<?>) :: Monad m => ParserT t u m a -> String -> ParserT t u m a
(ParserT parser) <?> expected =
  ParserT $ \st -> do
    res <- parser st
    case res of
      Left _          -> return $ Left [Expected expected]
      Right (x,newst) -> return $ Right (x, newst)
{-# INLINE (<?>) #-}
infixl 5 <?>

raiseParseErrors :: Monad m => [ParseError t] -> ParserT t u m a
raiseParseErrors errs = ParserT $ \_ -> return $ Left errs

getInput :: Monad m => ParserT t u m [t]
getInput = ParserT $ \st -> return $ Right (tokens st, st)
{-# INLINABLE getInput #-}

setInput :: Monad m => [t] -> ParserT t u m ()
setInput ts = ParserT $ \st -> return $ Right ((), st{ tokens = ts })
{-# INLINABLE setInput #-}

getState :: Monad m => ParserT t u m u
getState = ParserT $ \st -> return $ Right (userState st, st)
{-# INLINABLE getState #-}

putState :: Monad m => u -> ParserT t u m ()
putState uState = ParserT $ \st ->
  return $ Right ((), st{ userState = uState })
{-# INLINABLE putState #-}

updateState :: Monad m => (u -> u) -> ParserT t u m ()
updateState f = ParserT $ \st ->
  return $ Right ((), st{ userState = f (userState st) })
{-# INLINABLE updateState #-}
