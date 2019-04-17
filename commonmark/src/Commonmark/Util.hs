{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Util
  ( satisfyTok
  , getOffset
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
  )
  where
import           Control.Monad   (mzero, void, guard)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Parsec
import           Commonmark.Tokens

-- | Parses a single 'Tok' satisfying a predicate.
satisfyTok :: Monad m => (Tok -> Bool) -> ParsecT [Tok] s m Tok
satisfyTok f = tokenPrim tokToString updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> [Tok] -> SourcePos
        updatePos spos (Tok LineEnd _ _ _) _ =
          setSourceColumn (incSourceLine spos 1) 1
        updatePos spos (Tok Tab _ _ _) _ =
          incSourceColumn spos (4 - ((sourceColumn spos - 1) `mod` 4))
        updatePos spos (Tok _ _ len _) _ =
          incSourceColumn spos len
{-# INLINEABLE satisfyTok #-}

-- | Return offset of next token if any.
getOffset :: Monad m => ParsecT [Tok] s m (Maybe Int)
getOffset = (do
  t <- lookAhead anyTok
  return $ Just $ tokOffset t) <|> return Nothing

-- | Parses any 'Tok'.
anyTok :: Monad m => ParsecT [Tok] s m Tok
anyTok = satisfyTok (const True)
{-# INLINEABLE anyTok #-}

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: Monad m => ParsecT [Tok] s m Tok
anySymbol = satisfyTok (\t -> case tokType t of
                                    Symbol _ -> True
                                    _        -> False)
{-# INLINEABLE anySymbol #-}

-- | Parses a 'Symbol' with character @c@.
symbol ::  Monad m => Char -> ParsecT [Tok] s m Tok
symbol c = satisfyTok (hasType (Symbol c))
{-# INLINEABLE symbol #-}

-- | Parses a 'Tok' with one of the listed types.
oneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
oneOfToks toktypes = satisfyTok (\t -> any ($ t) (map hasType toktypes))
{-# INLINEABLE oneOfToks #-}

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
noneOfToks toktypes =
  satisfyTok (\t -> not $ any ($ t) (map hasType toktypes))
{-# INLINEABLE noneOfToks #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  Monad m => ParsecT [Tok] s m [Tok]
whitespace = many1 $ oneOfToks [Spaces, Tab, LineEnd]
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT [Tok] s m Tok
lineEnd = satisfyTok (hasType LineEnd)
{-# INLINEABLE lineEnd #-}

-- | Parses a 'Spaces' or 'Tab' token.
spaceTok :: Monad m => ParsecT [Tok] s m Tok
spaceTok = satisfyTok (\t -> hasType Spaces t || hasType Tab t)
{-# INLINEABLE spaceTok #-}

-- | Parses a 'WordChars' token matching a predicate.
satisfyWord ::  Monad m => (Text -> Bool) -> ParsecT [Tok] s m Tok
satisfyWord f = satisfyTok (\t -> hasType WordChars t && textIs f t)
{-# INLINEABLE satisfyWord #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: Monad m => Bool -> Int -> ParsecT [Tok] u m Int
gobble' requireAll numspaces
  | numspaces >= 1 = (do
    startpos <- getPosition
    Tok _ pos _ subj <-
           satisfyTok (\t -> hasType Spaces t || hasType Tab t)
    endpos <- getPosition
    case sourceColumn startpos - sourceColumn endpos of
         n | n < numspaces  -> (+ n) <$> gobble' requireAll (numspaces - n)
           | n == numspaces -> return n
           | otherwise      -> do
               let newtok = Tok Spaces pos numspaces subj
               getInput >>= setInput . (newtok:)
               return numspaces)
    <|> if requireAll
           then mzero
           else return 0
  | otherwise     = return 0
{-# INLINEABLE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: Monad m => ParsecT [Tok] s m a -> ParsecT [Tok] s m (a, [Tok])
withRaw parser = do
  toks <- getInput
  res <- parser
  rawtoks <- (do Tok _ pos _ _ <- lookAhead anyTok
                 return $ takeWhile ((< pos) . tokOffset) toks)
             <|> return toks
  return (res, rawtoks)
{-# INLINEABLE withRaw #-}

-- | Filters tokens of a certain type.
hasType :: TokType -> Tok -> Bool
hasType ty t = tokType t == ty
{-# INLINEABLE hasType #-}

-- | Filters tokens with certain contents.
textIs :: (Text -> Bool) -> Tok -> Bool
textIs f t = f (T.pack (tokToString t))
{-# INLINEABLE textIs #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParsecT [Tok] u m ()
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
skipWhile :: Monad m => (Tok -> Bool) -> ParsecT [Tok] u m ()
skipWhile f = skipMany (satisfyTok f)
{-# INLINEABLE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParsecT [Tok] s m ()
blankLine = try $ do
  skipWhile (\t -> hasType Spaces t || hasType Tab t)
  void lineEnd
{-# INLINEABLE blankLine #-}

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT [Tok] s m ([Tok], SourcePos)
restOfLine = do
  ts <- many (satisfyTok (not . hasType LineEnd))
  pos <- getPosition
  (do le <- lineEnd
      return (ts ++ [le], pos)) <|> (do eof
                                        guard (not (null ts))
                                        return (ts, pos))
{-# INLINEABLE restOfLine #-}
