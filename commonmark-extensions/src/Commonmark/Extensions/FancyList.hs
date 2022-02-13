{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Commonmark.Extensions.FancyList
  ( fancyListSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.TokParsers
import Commonmark.Blocks
import qualified Data.Text as T
import Control.Monad (mzero, guard, when)
import Text.Parsec
import qualified Data.Text.Read as TR
import Data.Char (isAlpha, isDigit, isLower, isUpper, ord, toLower)

fancyListSpec :: (Monad m, IsBlock il bl, IsInline il)
               => SyntaxSpec m il bl
fancyListSpec = mempty
  { syntaxBlockSpecs =
     [ listItemSpec (bulletListMarker <|> fancyOrderedListMarker) ]
  }

fancyOrderedListMarker :: Monad m => BlockParser m il bl ListType
fancyOrderedListMarker = do
  mbListType <- getParentListType
  -- first try to parse an item like the parent
  let pInSeries = case mbListType of
                     Just (OrderedList _ e d) -> try (pMarker e d)
                     _ -> mzero
  pInSeries <|>
      do initialParen <- option False $ True <$ symbol '('
         (start, enumtype) <- pDecimal <|>
                              (case mbListType of
                                 Nothing -> pLowerRomanOne <|> pUpperRomanOne
                                 _ -> mzero) <|>
                              pLowerAlpha <|> pUpperAlpha <|>
                              pLowerRoman <|> pUpperRoman
         delimtype <- if initialParen
                         then TwoParens <$ symbol ')'
                         else Period <$ symbol '.' <|> OneParen <$ symbol ')'
         when (delimtype == Period &&
              (enumtype == UpperRoman || enumtype == UpperAlpha)) $ checkSpace
         return $! OrderedList start enumtype delimtype

  where
    checkSpace = do
        Tok tt _ t <- lookAhead anyTok
        guard $ case tt of
                  Spaces  -> T.length t > 1
                  LineEnd -> True
                  _       -> False
    pMarker e d = do
      when (d == TwoParens) $ () <$ symbol '('
      (start, enumtype) <- case e of
        Decimal -> pDecimal
        LowerRoman -> pLowerRoman
        UpperRoman -> pUpperRoman
        LowerAlpha -> pLowerAlpha
        UpperAlpha -> pUpperAlpha
      delimtype <- case d of
        TwoParens -> TwoParens <$ symbol ')'
        OneParen  -> OneParen <$ symbol ')'
        Period    -> Period <$ symbol '.'
      when (delimtype == Period &&
           (enumtype == UpperRoman || enumtype == UpperAlpha)) $ checkSpace
      return $! OrderedList start enumtype delimtype
    pDecimal = do
      Tok WordChars _ ds <- satisfyWord (\t ->
                              T.all isDigit t && T.length t < 10)
      case TR.decimal ds of
        Left e -> fail e
        Right (x,_) -> return $! (x, Decimal)

    pLowerAlpha = do
      Tok WordChars _ ds <- satisfyWord (\t ->
                              T.length t == 1 &&
                              T.all isAlpha t &&
                              T.all isLower t)
      case T.uncons ds of
        Nothing    -> mzero
        Just (c,_) -> return $! (1 + ord c - ord 'a', LowerAlpha)

    pUpperAlpha = do
      Tok WordChars _ ds <- satisfyWord (\t ->
                              T.length t == 1 &&
                              T.all isAlpha t &&
                              T.all isUpper t)
      case T.uncons ds of
        Nothing    -> mzero
        Just (c,_) -> return $! (1 + ord c - ord 'A', UpperAlpha)

    pLowerRomanOne = (1, LowerRoman) <$ satisfyWord (== "i")
    pUpperRomanOne = (1, UpperRoman) <$ satisfyWord (== "I")

    pLowerRoman = do
      Tok WordChars _ ds <- satisfyWord (\t ->
                              T.length t < 10 &&
                              T.all isLowerRoman t)
      case parse (romanNumeral False) "" ds of
        Left _     -> mzero
        Right x    -> return $! (x, LowerRoman)

    pUpperRoman = do
      Tok WordChars _ ds <- satisfyWord (\t ->
                              T.length t < 10 &&
                              T.all isUpperRoman t)
      case parse (romanNumeral True) "" ds of
        Left _     -> mzero
        Right x    -> return $! (x, UpperRoman)

isLowerRoman :: Char -> Bool
isLowerRoman c = c `elem` ['i','v','x','l','c','d','m']

isUpperRoman :: Char -> Bool
isUpperRoman c = c `elem` ['I','V','X','L','C','D','M']

-- from pandoc:
romanNumeral :: Stream s m Char
             => Bool                  -- ^ Uppercase if true
             -> ParsecT s st m Int
romanNumeral upperCase = do
    let rchar uc = char $ if upperCase then uc else toLower uc
    let one         = rchar 'I'
    let five        = rchar 'V'
    let ten         = rchar 'X'
    let fifty       = rchar 'L'
    let hundred     = rchar 'C'
    let fivehundred = rchar 'D'
    let thousand    = rchar 'M'
    lookAhead $ choice [one, five, ten, fifty, hundred, fivehundred, thousand]
    thousands <- ((1000 *) . length) <$> many thousand
    ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
    fivehundreds <- option 0 $ 500 <$ fivehundred
    fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
    hundreds <- ((100 *) . length) <$> many hundred
    nineties <- option 0 $ try $ ten >> hundred >> return 90
    fifties <- option 0 (50 <$ fifty)
    forties <- option 0 $ try $ ten >> fifty >> return 40
    tens <- ((10 *) . length) <$> many ten
    nines <- option 0 $ try $ one >> ten >> return 9
    fives <- option 0 (5 <$ five)
    fours <- option 0 $ try $ one >> five >> return 4
    ones <- length <$> many one
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then fail "not a roman numeral"
       else return $! total
