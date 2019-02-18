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
  , hasType
  , textIs
  , blankLine
  , restOfLine
  , isOneOfCI
  , nonindentSpaces
  , skipManyTill
  )
  where
import           Control.Monad   (mzero, void)
import           Data.Maybe                (maybeToList)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Commonmark.Tokens
import           Commonmark.ParserCombinators

-- | Parses a single 'Tok' satisfying a predicate.
satisfyTok :: Monad m => (Tok -> Bool) -> ParserT Tok s m Tok
satisfyTok = satisfy

-- | Parses any 'Tok'.
anyTok :: Monad m => ParserT Tok s m Tok
anyTok = satisfyTok (const True)

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: Monad m => ParserT Tok s m Tok
anySymbol = satisfyTok (\t -> case tokType t of
                                    Symbol _ -> True
                                    _        -> False)

-- | Parses a 'Symbol' with character @c@.
symbol ::  Monad m => Char -> ParserT Tok s m Tok
symbol c = satisfyTok (hasType (Symbol c))

-- | Parses a 'Tok' with one of the listed types.
oneOfToks ::  Monad m => [TokType] -> ParserT Tok s m Tok
oneOfToks toktypes = satisfyTok (\t -> any ($ t) (map hasType toktypes))

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParserT Tok s m Tok
noneOfToks toktypes =
  satisfyTok (\t -> not $ any ($ t) (map hasType toktypes))

-- | Parses one or more whitespace 'Tok's.
whitespace ::  Monad m => ParserT Tok s m [Tok]
whitespace = some $ oneOfToks [Spaces, LineEnd]

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParserT Tok s m Tok
lineEnd = satisfyTok (hasType LineEnd)

-- | Parses a 'Spaces' token.
spaceTok :: Monad m => ParserT Tok s m Tok
spaceTok = satisfyTok (hasType Spaces)

-- | Parses a 'WordChars' token matching a predicate.
satisfyWord ::  Monad m => (Text -> Bool) -> ParserT Tok s m Tok
satisfyWord f = satisfyTok (\t -> hasType WordChars t && textIs f t)

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m => Int -> ParserT Tok u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n =  gobble' True n

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m => Int -> ParserT Tok u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n

gobble' :: Monad m => Bool -> Int -> ParserT Tok u m Int
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

-- | Filters tokens of a certain type.
hasType :: TokType -> Tok -> Bool
hasType ty (Tok ty' _ _) = ty == ty'

-- | Filters tokens with certain contents.
textIs :: (Text -> Bool) -> Tok -> Bool
textIs f (Tok _ _ t) = f t

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParserT Tok u m ()
nonindentSpaces = void $ gobbleUpToSpaces 3

-- | Case-insensitive membership in a list of 'Text's.
isOneOfCI :: [Text] -> Text -> Bool
isOneOfCI ts t = T.toLower t `elem` ts

-- | Apply @p@ many times until @stop@ succeeds, discarding results.
skipManyTill :: Monad m
             => ParserT t u m a -> ParserT t u m b -> ParserT t u m ()
skipManyTill p stop = scan
    where scan = (() <$ stop) <|> (p >> scan)

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParserT Tok s m ()
blankLine = do
  skipWhile (hasType Spaces)
  void lineEnd

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParserT Tok s m ([Tok], SourcePos)
restOfLine = do
  ts <- tokensWhile (not . hasType LineEnd)
  pos <- getPosition
  le <- optional lineEnd
  return (ts ++ maybeToList le, pos)

