{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Pretty-printer to cookie jar format for the 'Cookie' and 'CookieJar' types
from http-types.

__WARNING__: This is not a full serialization of the 'Cookie' and 'CookieJar'
types. The Netscape cookie jar format does not support all of the fields.

In particular, the following fields of 'Cookie' are not represented:

  * 'cookie_creation_time'
  * 'cookie_last_access_time'
  * 'cookie_persistent'
  * 'cookie_http_only'
|-}   
module Data.CURL.CookieJar.PrettyPrinter
  ( prettyCookieJar
  , CookieJarHeader(..)
  , prettyCookieJarHeader
  , prettyCookie
  -- |
  -- * Re-export
  -- This is re-exported from 'Data.ByteString.Builder' for convenience
  , toLazyByteString
  ) where

import Data.Semigroup ((<>))
import Network.HTTP.Client (Cookie(..), CookieJar, destroyCookieJar)
import Data.ByteString.Builder
  ( Builder
  , byteString
  , integerDec
  , char7
  , toLazyByteString
  )
import Data.Foldable (foldMap)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- | Specify what header, if any, to print at the top of the cookie jar
--
-- Some other parsers (like Perl's) require the header to be present
data CookieJarHeader
  = NoHeader
  | HTTPHeader
  | NetscapeHeader

-- | Print a cookie jar in the Netscape/Mozilla format, with optional
-- leading header
prettyCookieJar :: CookieJarHeader -> CookieJar -> Builder
prettyCookieJar header jar =
     prettyCookieJarHeader header
  <> foldMap ((<> "\n") . prettyCookie) (destroyCookieJar jar)

-- | Print a cookie jar header
prettyCookieJarHeader :: CookieJarHeader -> Builder
prettyCookieJarHeader NoHeader = mempty
prettyCookieJarHeader HTTPHeader = "# HTTP Cookie File\n"
prettyCookieJarHeader NetscapeHeader = "# Netscape HTTP Cookie File\n"

-- | Print an individual cookie on a single line
prettyCookie :: Cookie -> Builder
prettyCookie (Cookie {..}) =
     byteString cookie_domain
  <> tab
  <> bool cookie_host_only
  <> tab
  <> byteString cookie_path
  <> tab
  <> bool cookie_secure_only
  <> tab
  <> unixTime cookie_expiry_time
  <> tab
  <> byteString cookie_name
  <> tab
  <> byteString cookie_value
  where
    bool True = "TRUE"
    bool False = "FALSE"
    unixTime = integerDec . round . utcTimeToPOSIXSeconds
    tab = char7 '\t'


