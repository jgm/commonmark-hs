{-# LANGUAGE OverloadedStrings #-}
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
  )
  where
import           Control.Monad   (mzero, void)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Parsec
import           Text.Parsec.Pos (updatePosString)
import           Commonmark.Tokens

-- | Parses a single 'Tok' satisfying a predicate.
satisfyTok :: Monad m => (Tok -> Bool) -> ParsecT [Tok] s m Tok
satisfyTok f = tokenPrim (T.unpack . tokContents) updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> [Tok] -> SourcePos
        updatePos _spos _ (Tok _ pos _ : _) = pos
        updatePos spos (Tok _ _pos t) []    =
          updatePosString spos (T.unpack t)
{-# INLINEABLE satisfyTok #-}

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
oneOfToks toktypes = satisfyTok (hasTypeIn toktypes)
{-# INLINEABLE oneOfToks #-}

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
noneOfToks toktypes = satisfyTok (not . hasTypeIn toktypes)
{-# INLINEABLE noneOfToks #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  Monad m => ParsecT [Tok] s m [Tok]
whitespace = many1 $ satisfyTok (\t -> case tokType t of
                                         Spaces  -> True
                                         LineEnd -> True
                                         _       -> False)
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT [Tok] s m Tok
lineEnd = satisfyTok (hasType LineEnd)
{-# INLINEABLE lineEnd #-}

-- | Parses a 'Spaces' token.
spaceTok :: Monad m => ParsecT [Tok] s m Tok
spaceTok = satisfyTok (hasType Spaces)
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
    Tok Spaces pos _ <- satisfyTok (hasType Spaces)
    pos' <- getPosition
    case sourceColumn pos' - sourceColumn pos of
         n | n < numspaces  -> (+ n) <$> gobble' requireAll (numspaces - n)
           | n == numspaces -> return n
           | otherwise      -> do
               let newtok = Tok Spaces
                      (incSourceColumn pos numspaces)
                      (T.replicate (n - numspaces) " ")
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
  newpos <- getPosition
  let rawtoks = takeWhile ((< newpos) . tokPos) toks
  return (res, rawtoks)
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
  skipWhile (hasType Spaces)
  void lineEnd
{-# INLINEABLE blankLine #-}

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT [Tok] s m ([Tok], SourcePos)
restOfLine = do
  toks <- getInput
  case break (hasType LineEnd) toks of
    (_,[]) -> do
      ts <- many1 anyTok
      pos <- getPosition
      return (ts, pos)
    (ts,le@(Tok _ pos _):rest) -> do
      setInput (le:rest)
      lineEnd -- gobble le, so parsec doesn't think nothing consumed
      return (ts ++ [le], pos)
{-# INLINEABLE restOfLine #-}
