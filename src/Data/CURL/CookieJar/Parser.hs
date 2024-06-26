{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Attoparsec 'Parser' for a cookie jar, and convenience function to run it |-}
module Data.CURL.CookieJar.Parser
  ( cookieJarParser
  , cookieParser
  , parseCookieJar
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Attoparsec.ByteString.Char8
  ( many'
  , endOfLine
  , endOfLine
  , isEndOfLine
  , takeWhile1
  , decimal
  , skipSpace
  , skipWhile
  , char
  , parseOnly
  , try
  , Parser
  )
import Network.HTTP.Client (Cookie(..), CookieJar, createCookieJar)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Parse a cookie jar in the Netscape/Mozilla format
parseCookieJar :: ByteString -> Either String CookieJar
parseCookieJar = parseOnly cookieJarParser

-- | Parser a cookie jar in the Netscape/Mozilla format
cookieJarParser :: Parser CookieJar
cookieJarParser = createCookieJar <$> many' cookieParser

-- | Parser for one cookie/line in a cookie jar in the Netscape/Mozilla format
-- This will also consume any comment lines preceding the cookie line.
--
-- This parser recognizes the magic prefix @#HttpOnly_# and sets the appropriate
-- field in the Cookie datatype
cookieParser :: Parser Cookie
cookieParser = 
  skipSpace *> (httpOnlyLine <|> commentLine <|> cookieLine)
  where
    httpOnlyLine = try $ "#HttpOnly_" *> cookieLineParser True
    commentLine = "#" *> skipWhile notEndOfLine *> endOfLine *> cookieParser
    cookieLine = cookieLineParser False
    cookieLineParser cookie_http_only = do
      let -- these are the fields not represented by the cookie jar format
        cookie_creation_time = epoch
        cookie_last_access_time = epoch
        cookie_persistent = True
      cookie_domain <- stringField
      tab
      cookie_host_only <- boolField
      tab
      cookie_path <- stringField
      tab
      cookie_secure_only <- boolField
      tab
      cookie_expiry_time <- timeField
      tab
      cookie_name <- stringField
      tab
      cookie_value <- lastField
      (endOfLine <|> pure ())
      pure $ Cookie {..}
      where
        tab = void $ char '\t'
        stringField = takeWhile1 (/= '\t')
        boolField = (True <$ "TRUE") <|> (False <$ "FALSE")
        timeField = posixSecondsToUTCTime <$> fromInteger <$> decimal
        lastField = takeWhile1 (notEndOfLine)
        epoch = posixSecondsToUTCTime 0
      

notEndOfLine :: Char -> Bool
notEndOfLine = not . isEndOfLine . fromIntegral . ord
