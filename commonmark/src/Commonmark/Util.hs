{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Util
  ( satisfyTok
  , satisfyWord
  , anyTok
  , anySymbol
  , symbol
  , whitespace
  , lineEnd
  , spaceTok
  , oneOfToks
  , noneOfToks
  , gobbleSpaces
  , gobbleUpToSpaces
  , withRaw
  , hasType
  , textIs
  , blankLine
  , restOfLine
  , isOneOfCI
  , nonindentSpaces
  , skipManyTill
  , skipWhile
  , tokensWhile1
  )
  where
import           Control.Monad   (mzero, void)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Parsec
import           Text.Parsec.Pos (updatePosString)
import           Commonmark.Tokens
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | Parses a single 'Tok' satisfying a predicate.
satisfyTok :: Monad m => (Tok -> Bool) -> ParsecT Toks s m Tok
satisfyTok f = tokenPrim (T.unpack . tokContents) updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> Toks -> SourcePos
        updatePos _spos _ (Toks n v)
          | n < V.length v
          , Just (Tok _ !pos _) <- V.indexM v n = pos
        updatePos !spos (Tok _ _ t) _ = updatePosString spos (T.unpack t)
{-# INLINEABLE satisfyTok #-}

-- | Parses any 'Tok'.
anyTok :: Monad m => ParsecT Toks s m Tok
anyTok = satisfyTok (const True)
{-# INLINEABLE anyTok #-}

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: Monad m => ParsecT Toks s m Tok
anySymbol = satisfyTok (\t -> case tokType t of
                                    Symbol _ -> True
                                    _        -> False)
{-# INLINEABLE anySymbol #-}

-- | Parses a 'Symbol' with character @c@.
symbol ::  Monad m => Char -> ParsecT Toks s m Tok
symbol c = satisfyTok (hasType (Symbol c))
{-# INLINEABLE symbol #-}

-- | Parses a 'Tok' with one of the listed types.
oneOfToks ::  Monad m => [TokType] -> ParsecT Toks s m Tok
oneOfToks toktypes = satisfyTok (hasTypeIn toktypes)
{-# INLINEABLE oneOfToks #-}

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParsecT Toks s m Tok
noneOfToks toktypes = satisfyTok (not . hasTypeIn toktypes)
{-# INLINEABLE noneOfToks #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  Monad m => ParsecT Toks s m [Tok]
whitespace = many1 $ satisfyTok (\t -> case tokType t of
                                         Spaces  -> True
                                         LineEnd -> True
                                         _       -> False)
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT Toks s m Tok
lineEnd = satisfyTok (hasType LineEnd)
{-# INLINEABLE lineEnd #-}

-- | Parses a 'Spaces' token.
spaceTok :: Monad m => ParsecT Toks s m Tok
spaceTok = satisfyTok (hasType Spaces)
{-# INLINEABLE spaceTok #-}

-- | Parses a 'WordChars' token matching a predicate.
satisfyWord ::  Monad m => (Text -> Bool) -> ParsecT Toks s m Tok
satisfyWord f = satisfyTok (\t -> hasType WordChars t && textIs f t)
{-# INLINEABLE satisfyWord #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m => Int -> ParsecT Toks u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m => Int -> ParsecT Toks u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: Monad m => Bool -> Int -> ParsecT Toks u m Int
gobble' requireAll numspaces
  | numspaces >= 1 = (do
    Tok Spaces pos _ <- satisfyTok (hasType Spaces)
    pos' <- getPosition
    case sourceColumn pos' - sourceColumn pos of
         n | n < numspaces  -> (+ n) <$> gobble' requireAll (numspaces - n)
           | n == numspaces -> return $! n
           | otherwise      -> do
               let newtok = Tok Spaces
                      (incSourceColumn pos numspaces)
                      (T.replicate (n - numspaces) " ")
               Toks i v <- getInput
               let f mv = VM.unsafeWrite mv (i - 1) newtok
               setInput $ Toks (i-1) $ V.modify f v
               return $! numspaces)
    <|> if requireAll
           then mzero
           else return 0
  | otherwise     = return 0
{-# INLINEABLE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: Monad m => ParsecT Toks s m a -> ParsecT Toks s m (a, [Tok])
withRaw parser = do
  Toks n v <- getInput
  res <- parser
  Toks n' _ <- getInput
  let rawtoks = V.toList $ V.slice n (n' - n) v
  return $! (res, rawtoks)
{-# INLINEABLE withRaw #-}

-- | Filters tokens of a certain type.
hasType :: TokType -> Tok -> Bool
hasType ty (Tok ty' _ _) = ty == ty'
{-# INLINEABLE hasType #-}

hasTypeIn :: [TokType] -> Tok -> Bool
hasTypeIn tys (Tok ty' _ _) = ty' `elem` tys

-- | Filters tokens with certain contents.
textIs :: (Text -> Bool) -> Tok -> Bool
textIs f (Tok _ _ t) = f t
{-# INLINEABLE textIs #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParsecT Toks u m ()
nonindentSpaces = void $ gobbleUpToSpaces 3
{-# INLINEABLE nonindentSpaces #-}

-- | Case-insensitive membership in a list of 'Text's.
isOneOfCI :: [Text] -> Text -> Bool
isOneOfCI ts t = T.toLower t `elem` ts
{-# INLINEABLE isOneOfCI #-}

-- | Apply @p@ many times until @stop@ succeeds, discarding results.
skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
skipManyTill p stop = scan
    where scan = (() <$ stop) <|> (p >> scan)
{-# INLINEABLE skipManyTill #-}

-- | Efficiently skip 'Tok's satisfying a certain condition.
skipWhile :: Monad m => (Tok -> Bool) -> ParsecT Toks u m ()
skipWhile f = skipMany (satisfyTok f)
{-# INLINEABLE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParsecT Toks s m ()
blankLine = try $ do
  skipWhile (hasType Spaces)
  void lineEnd
{-# INLINEABLE blankLine #-}

-- | Efficiently parse one or more tokens meeting a
-- condition.
tokensWhile1 :: Monad m => (Tok -> Bool) -> ParsecT Toks s m [Tok]
tokensWhile1 f = do
  Toks n v <- getInput
  let v' = V.takeWhile f $ V.drop n v
  let len = V.length v'
  if len > 0
     then do
       setInput $ Toks (n + (len - 1)) v
       satisfyTok (const True) -- to make parsec know it's not empty
                               -- and set position
       return $! V.toList v'
     else mzero

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT Toks s m ([Tok], SourcePos)
restOfLine =
   (do ts <- tokensWhile1 (not . hasType LineEnd)
       pos <- getPosition
       le <- option [] $ (:[]) <$> lineEnd
       return $! (ts ++ le, pos))
  <|>
   (do le@(Tok _ pos _) <- lineEnd
       return $! ([le], pos))
{-# INLINEABLE restOfLine #-}
