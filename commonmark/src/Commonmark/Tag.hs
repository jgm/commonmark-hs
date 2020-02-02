{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Tag
  ( htmlTag
  , htmlOpenTag
  , htmlClosingTag
  , htmlAttributeName
  , htmlAttributeValue
  , htmlDoubleQuotedAttributeValue )
where
import           Commonmark.Parsec
import           Control.Monad     (liftM2)
import           Data.Char         (isAscii, isLetter, isAlphaNum)
import           Data.Text         (Text)
import qualified Data.Text         as T

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

-- A tag name consists of an ASCII letter followed by zero or more ASCII
-- letters, digits, or hyphens (-).
htmlTagName :: Monad m => ParsecT Text s m Text
htmlTagName = do
  lookAhead $ satisfy (\c -> isAscii c && isLetter c)
  textWhile1 (\c -> c == '-' || (isAscii c && isAlphaNum c))

-- An attribute name consists of an ASCII letter, _, or :, followed by
-- zero or more ASCII letters, digits, _, ., :, or -. (Note: This is
-- the XML specification restricted to ASCII. HTML5 is laxer.)
htmlAttributeName :: Monad m => ParsecT Text s m Text
htmlAttributeName = try $ do
  lookAhead $ satisfy (\c -> c == '_' || c == ':' || (isAscii c && isLetter c))
  textWhile1 (\c -> c == '_' || c == ':' || c == '-' || c == '.' ||
                    (isAscii c && isAlphaNum c))

-- An attribute value specification consists of optional whitespace,
-- a = character, optional whitespace, and an attribute value.
htmlAttributeValueSpec :: Monad m => ParsecT Text s m Text
htmlAttributeValueSpec = try $ do
  sps1 <- option "" whitespace
  _ <- char '='
  sps2 <- option "" whitespace
  val <- htmlAttributeValue
  return $ sps1 <> "=" <> sps2 <> val

-- An attribute value consists of an unquoted attribute value,
-- a single-quoted attribute value, or a double-quoted attribute value.
htmlAttributeValue :: Monad m => ParsecT Text s m Text
htmlAttributeValue =
  htmlUnquotedAttributeValue <|>
  htmlSingleQuotedAttributeValue <|>
  htmlDoubleQuotedAttributeValue

-- An attribute consists of whitespace, an attribute name, and an optional
-- attribute value specification.
htmlAttribute :: Monad m => ParsecT Text s m Text
htmlAttribute = try $ do
  sps <- whitespace
  n <- htmlAttributeName
  val <- option "" htmlAttributeValueSpec
  return $ sps <> n <> val

-- An unquoted attribute value is a nonempty string of characters not
-- including spaces, ", ', =, <, >, or `.
htmlUnquotedAttributeValue :: Monad m => ParsecT Text s m Text
htmlUnquotedAttributeValue =
  textWhile1 (\c -> not (c == ' ' || c == '\t' || c == '\r' || c == '\n' ||
                         c == '<' || c == '>' || c == '=' || c == '`' ||
                         c == '\'' || c == '"'))

-- A single-quoted attribute value consists of ', zero or more characters
-- not including ', and a final '.
htmlSingleQuotedAttributeValue :: Monad m => ParsecT Text s m Text
htmlSingleQuotedAttributeValue = try $ do
  _ <- char '\''
  contents <- textWhile1 (/= '\'')
  _ <- char '\''
  return $ "'" <> contents <> "'"

-- A double-quoted attribute value consists of ", zero or more characters
-- not including ", and a final ".
htmlDoubleQuotedAttributeValue :: Monad m => ParsecT Text s m Text
htmlDoubleQuotedAttributeValue = try $ do
  _ <- char '"'
  contents <- textWhile1 (/= '"')
  _ <- char '"'
  return $ "\"" <> contents <> "\""

-- | An open tag consists of a @<@ character, a tag name, zero or more
-- attributes, optional whitespace, an optional @/@ character, and a
-- @>@ character.  This parses assumes that the opening @<@ has already
-- been parsed.
htmlOpenTag :: Monad m => ParsecT Text s m Text
htmlOpenTag = try $ do
  -- assume < has already been parsed
  n <- htmlTagName
  attrs <- mconcat <$> many htmlAttribute
  sps <- option mempty whitespace
  sl <- option mempty $ T.singleton <$> char '/'
  _ <- char '>'
  return $ n <> attrs <> sps <> sl <> ">"

-- | A closing tag consists of the string @</@, a tag name, optional
-- whitespace, and the character @>@.  This parser assumes that the
-- opening @<@ has already been parsed.
htmlClosingTag :: Monad m => ParsecT Text s m Text
htmlClosingTag = try $ do
  -- assume < has already been parsed
  _ <- char '/'
  n <- htmlTagName
  sps <- option mempty whitespace
  _ <- char '>'
  return $ "/" <> n <> sps <> ">"

-- An HTML comment consists of <!-- + text + -->, where text does not
-- start with > or ->, does not end with -, and does not contain --.
-- (See the HTML5 spec.)
htmlComment :: Monad m => ParsecT Text s m Text
htmlComment = try $ do
  -- assume < has already been parsed
  _ <- string "!--"
  notFollowedBy $ string "->"
  contents <- many $ satisfy (/= '-')
                 <|> try (char '-' <* notFollowedBy (char '-'))
  _ <- string "-->"
  return $ "!--" <> T.pack contents <> "-->"

-- A processing instruction consists of the string <?, a string of
-- characters not including the string ?>, and the string ?>.
htmlProcessingInstruction :: Monad m => ParsecT Text s m Text
htmlProcessingInstruction = try $ do
  -- assume < has already been parsed
  _ <- char '?'
  contents <- many $ satisfy (/= '?')
                 <|> try (char '?' <* notFollowedBy (char '>'))
  _ <- string "?>"
  return $ "?" <> T.pack contents <> "?>"

-- A declaration consists of the string <!, a name consisting of one or
-- more uppercase ASCII letters, whitespace, a string of characters not
-- including the character >, and the character >.
htmlDeclaration :: Monad m => ParsecT Text s m Text
htmlDeclaration = try $ do
  -- assume < has already been parsed
  _ <- char '!'
  name <- textWhile1 (isAscii .&&. isLetter)
  ws <- whitespace
  contents <- textWhile1 (\c -> c /= '>')
  _ <- char '>'
  return $ "!" <> name <> ws <> contents <> ">"

-- A CDATA section consists of the string <![CDATA[, a string of characters
-- not including the string ]]>, and the string ]]>.
htmlCDATASection :: Monad m => ParsecT Text s m Text
htmlCDATASection = try $ do
  -- assume < has already been parsed
  _ <- string "![CDATA["
  let ender = string "]]>"
  contents <- many $ notFollowedBy ender >> anyChar
  _ <- ender
  return $ "![CDATA[" <> T.pack contents <> "]]>"

-- An HTML tag consists of an open tag, a closing tag, an HTML comment,
-- a processing instruction, a declaration, or a CDATA section.
-- Assumes @<@ has already been parsed.
htmlTag :: Monad m => ParsecT Text s m Text
htmlTag = htmlOpenTag <|> htmlClosingTag <|> htmlComment <|>
          htmlProcessingInstruction <|> htmlDeclaration <|> htmlCDATASection
