{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Tag
  ( htmlTag
  , htmlOpenTag
  , htmlClosingTag
  , htmlAttributeName
  , htmlAttributeValue
  , htmlDoubleQuotedAttributeValue )
where
import           Commonmark.Tokens
import           Commonmark.Util
import           Control.Monad     (liftM2)
import           Data.Char         (isAscii, isLetter)
import qualified Data.ByteString.Char8  as B8
import           Text.Parsec       hiding (State)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

-- A tag name consists of an ASCII letter followed by zero or more ASCII
-- letters, digits, or hyphens (-).
htmlTagName :: Monad m => ParsecT [Tok] s m [Tok]
htmlTagName = try $ do
  let isTagText t' = B8.all isAscii t'
  let startsWithLetter t' = not (B8.null t') && isLetter (B8.head t')
  t <- satisfyWord (isTagText .&&. startsWithLetter)
  rest <- many (symbol '-' <|> satisfyWord isTagText)
  return (t:rest)

-- An attribute name consists of an ASCII letter, _, or :, followed by
-- zero or more ASCII letters, digits, _, ., :, or -. (Note: This is
-- the XML specification restricted to ASCII. HTML5 is laxer.)
htmlAttributeName :: Monad m => ParsecT [Tok] s m [Tok]
htmlAttributeName = try $ do
  let isTagText t' = B8.all isAscii t'
  let startsWithLetter t' = not (B8.null t') && isLetter (B8.head t')
  t <- satisfyWord (startsWithLetter .&&. isTagText) <|>
        symbol '_' <|>
        symbol ':'
  rest <- many $ satisfyWord isTagText
             <|> symbol '_'
             <|> symbol '-'
             <|> symbol '.'
             <|> symbol ':'
  return (t:rest)

-- An attribute value specification consists of optional whitespace,
-- a = character, optional whitespace, and an attribute value.
htmlAttributeValueSpec :: Monad m => ParsecT [Tok] s m [Tok]
htmlAttributeValueSpec = try $ do
  sps1 <- option [] whitespace
  eq <- symbol '='
  sps2 <- option [] whitespace
  val <- htmlAttributeValue
  return $ sps1 ++ [eq] ++ sps2 ++ val

-- An attribute value consists of an unquoted attribute value,
-- a single-quoted attribute value, or a double-quoted attribute value.
htmlAttributeValue :: Monad m => ParsecT [Tok] s m [Tok]
htmlAttributeValue =
  htmlUnquotedAttributeValue <|>
  htmlSingleQuotedAttributeValue <|>
  htmlDoubleQuotedAttributeValue

-- An attribute consists of whitespace, an attribute name, and an optional
-- attribute value specification.
htmlAttribute :: Monad m => ParsecT [Tok] s m [Tok]
htmlAttribute = try $ do
  sps <- whitespace
  n <- htmlAttributeName
  val <- option [] htmlAttributeValueSpec
  return $ sps ++ n ++ val

-- An unquoted attribute value is a nonempty string of characters not
-- including spaces, ", ', =, <, >, or `.
htmlUnquotedAttributeValue :: Monad m => ParsecT [Tok] s m [Tok]
htmlUnquotedAttributeValue =
  many1 $ noneOfToks [Spaces, LineEnd, Symbol '<', Symbol '>',
                      Symbol '=', Symbol '`', Symbol '\'', Symbol '"']

-- A single-quoted attribute value consists of ', zero or more characters
-- not including ', and a final '.
htmlSingleQuotedAttributeValue :: Monad m => ParsecT [Tok] s m [Tok]
htmlSingleQuotedAttributeValue = try $ do
  op <- symbol '\''
  contents <- many (satisfyTok (not . hasType (Symbol '\'')))
  cl <- symbol '\''
  return $ op : contents ++ [cl]

-- A double-quoted attribute value consists of ", zero or more characters
-- not including ", and a final ".
htmlDoubleQuotedAttributeValue :: Monad m => ParsecT [Tok] s m [Tok]
htmlDoubleQuotedAttributeValue = try $ do
  op <- symbol '"'
  contents <- many (satisfyTok (not . hasType (Symbol '"')))
  cl <- symbol '"'
  return $ op : contents ++ [cl]

-- | An open tag consists of a @<@ character, a tag name, zero or more
-- attributes, optional whitespace, an optional @/@ character, and a
-- @>@ character.  This parses assumes that the opening @<@ has already
-- been parsed.
htmlOpenTag :: Monad m => ParsecT [Tok] s m [Tok]
htmlOpenTag = try $ do
  -- assume < has already been parsed
  n <- htmlTagName
  attrs <- concat <$> many htmlAttribute
  sps <- option [] whitespace
  sl <- option [] $ (:[]) <$> symbol '/'
  cl <- symbol '>'
  return $ n ++ attrs ++ sps ++ sl ++ [cl]

-- | A closing tag consists of the string @</@, a tag name, optional
-- whitespace, and the character @>@.  This parser assumes that the
-- opening @<@ has already been parsed.
htmlClosingTag :: Monad m => ParsecT [Tok] s m [Tok]
htmlClosingTag = try $ do
  -- assume < has already been parsed
  op <- symbol '/'
  n <- htmlTagName
  sps <- option [] whitespace
  cl <- symbol '>'
  return $ op : n ++ sps ++ [cl]

-- An HTML comment consists of <!-- + text + -->, where text does not
-- start with > or ->, does not end with -, and does not contain --.
-- (See the HTML5 spec.)
htmlComment :: Monad m => ParsecT [Tok] s m [Tok]
htmlComment = try $ do
  -- assume < has already been parsed
  op <- sequence [ symbol '!'
                 , symbol '-'
                 , symbol '-' ]
  notFollowedBy $ do
    optional $ symbol '-'
    symbol '>'
  contents <- many $ satisfyTok (not . hasType (Symbol '-'))
                 <|> try (symbol '-' <* notFollowedBy (symbol '-'))
  cl <- sequence [ symbol '-'
                 , symbol '-'
                 , symbol '>' ]
  return $ op ++ contents ++ cl

-- A processing instruction consists of the string <?, a string of
-- characters not including the string ?>, and the string ?>.
htmlProcessingInstruction :: Monad m => ParsecT [Tok] s m [Tok]
htmlProcessingInstruction = try $ do
  -- assume < has already been parsed
  let questionmark = symbol '?'
  op <- questionmark
  contents <- many $ satisfyTok (not . hasType (Symbol '?'))
                 <|> try (questionmark <*
                           notFollowedBy (symbol '>'))
  cl <- sequence [ questionmark
                 , symbol '>' ]
  return $ op : contents ++ cl

-- A declaration consists of the string <!, a name consisting of one or
-- more uppercase ASCII letters, whitespace, a string of characters not
-- including the character >, and the character >.
htmlDeclaration :: Monad m => ParsecT [Tok] s m [Tok]
htmlDeclaration = try $ do
  -- assume < has already been parsed
  op <- symbol '!'
  let isDeclName t = not (B8.null t) && B8.all (isAscii .&&. isLetter) t
  name <- satisfyWord isDeclName
  ws <- whitespace
  contents <- many (satisfyTok (not . hasType (Symbol '>')))
  cl <- symbol '>'
  return $ op : name : ws ++ contents ++ [cl]

-- A CDATA section consists of the string <![CDATA[, a string of characters
-- not including the string ]]>, and the string ]]>.
htmlCDATASection :: Monad m => ParsecT [Tok] s m [Tok]
htmlCDATASection = try $ do
  -- assume < has already been parsed
  op <- sequence [ symbol '!'
                 , symbol '['
                 , satisfyWord (== "CDATA")
                 , symbol '[' ]
  let ender = try $ sequence [ symbol ']'
                             , symbol ']'
                             , symbol '>' ]
  contents <- many $ do
                notFollowedBy ender
                anyTok
  cl <- ender
  return $ op ++ contents ++ cl

-- An HTML tag consists of an open tag, a closing tag, an HTML comment,
-- a processing instruction, a declaration, or a CDATA section.
-- Assumes @<@ has already been parsed.
htmlTag :: Monad m => ParsecT [Tok] s m [Tok]
htmlTag = htmlOpenTag <|> htmlClosingTag <|> htmlComment <|>
          htmlProcessingInstruction <|> htmlDeclaration <|> htmlCDATASection
