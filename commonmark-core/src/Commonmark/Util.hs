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
  , whileM_
  , blankLine
  , restOfLine
  , isOneOfCI
  , nonindentSpaces
  , skipManyTill
  , skipWhile
  )
  where
import           Control.Monad   (mzero, when, void)
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

-- | Parses any 'Tok'.
anyTok :: Monad m => ParsecT [Tok] s m Tok
anyTok = satisfyTok (const True)

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: Monad m => ParsecT [Tok] s m Tok
anySymbol = satisfyTok (\t -> case tokType t of
                                    Symbol _ -> True
                                    _        -> False)

-- | Parses a 'Symbol' with character @c@.
symbol ::  Monad m => Char -> ParsecT [Tok] s m Tok
symbol c = satisfyTok (hasType (Symbol c))

-- | Parses a 'Tok' with one of the listed types.
oneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
oneOfToks toktypes = satisfyTok (\t -> any ($ t) (map hasType toktypes))

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
noneOfToks toktypes =
  satisfyTok (\t -> not $ any ($ t) (map hasType toktypes))

-- | Parses a 'Tok' with none of the listed types.
whitespace ::  Monad m => ParsecT [Tok] s m [Tok]
whitespace = many1 $ oneOfToks [Spaces, LineEnd]

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT [Tok] s m Tok
lineEnd = satisfyTok (hasType LineEnd)

-- | Parses a 'Spaces' token.
spaceTok :: Monad m => ParsecT [Tok] s m Tok
spaceTok = satisfyTok (hasType Spaces)

-- | Parses a 'WordChars' token matching a predicate.
satisfyWord ::  Monad m => (Text -> Bool) -> ParsecT [Tok] s m Tok
satisfyWord f = satisfyTok (\t -> hasType WordChars t && textIs f t)

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n

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

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: Monad m => ParsecT [Tok] s m a -> ParsecT [Tok] s m (a, [Tok])
withRaw parser = do
  toks <- getInput
  res <- parser
  newpos <- getPosition
  let rawtoks = takeWhile ((< newpos) . tokPos) toks
  return $ rawtoks `seq` res `seq` (res, rawtoks)

-- | Filters tokens of a certain type.
hasType :: TokType -> Tok -> Bool
hasType ty (Tok ty' _ _) = ty == ty'

-- | Filters tokens with certain contents.
textIs :: (Text -> Bool) -> Tok -> Bool
textIs f (Tok _ _ t) = f t

-- from monad-loops:
-- | Execute an action repeatedly as long as the given boolean expression
-- returns True.  The condition is evaluated before the loop body.
-- Discards results.
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x $ f >> go

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParsecT [Tok] u m ()
nonindentSpaces = void $ gobbleUpToSpaces 3

-- | Case-insensitive membership in a list of 'Text's.
isOneOfCI :: [Text] -> Text -> Bool
isOneOfCI ts t = T.toLower t `elem` ts

-- | Apply @p@ many times until @stop@ succeeds, discarding results.
skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
skipManyTill p stop = scan
    where scan = (() <$ stop) <|> (p >> scan)

-- | Efficiently skip 'Tok's satisfying a certain condition.
skipWhile :: Monad m => (Tok -> Bool) -> ParsecT [Tok] u m ()
skipWhile f = void $ updateParserState $
  \(State inp pos user) ->
     case dropWhile f inp of
         rest@(Tok _ newpos _ : _) ->
            State rest newpos user
         [] ->
            case reverse inp of
                 (Tok _ lastpos t : _) ->
                   State [] (incSourceColumn lastpos (T.length t)) user
                 [] -> State inp pos user

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParsecT [Tok] s m ()
blankLine = try $ do
  skipWhile (hasType Spaces)
  void lineEnd

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT [Tok] s m ([Tok], SourcePos)
restOfLine = do
  inp <- getInput
  case break (hasType LineEnd) inp of
       (_, [])       -> (,) <$> many1 anyTok <*> getPosition
       (ts, le@(Tok _ pos _):rest) -> do
         setPosition pos
         setInput (le:rest)
         lineEnd
         return (ts ++ [le], pos)
