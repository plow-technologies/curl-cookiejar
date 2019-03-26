{-|
Conduit sources/sinks for Netscape/Mozilla cookie jar format.
|-}
module Data.CURL.CookieJar.Conduit
  (
  -- * Parsing cookie jars
    parseCookiesC
  , parseCookiesEitherC
  , sinkCookieJar
  , sinkCookieJarEither
  , sourceCookieJarCookies
  -- * Pretty-printing cookie jars
  , prettyCookieC
  , prettyCookiesC
  , prettyCookieJarC
  -- * Handle and file IO
  , readCookieJarFileC
  , readCookieJarHandleC
  , sourceCookiesFile
  , sourceCookiesHandle
  , sinkCookiesFile
  , sinkCookiesHandle
  , writeCookieJarFileC
  , writeCookieJarHandleC
  ) where

import Data.ByteString (ByteString)
import Conduit
  ( MonadThrow
  , PrimMonad
  , MonadIO
  , MonadResource
  , mapC
  , ConduitT
  , (.|)
  , yield
  , sourceFile
  , sourceHandle
  , sinkFile
  , sinkHandle
  )
import System.IO (Handle)
import Data.Conduit.List (sourceList, consume)
import Data.Conduit.ByteString.Builder (builderToByteString)
import Data.Conduit.Attoparsec
  ( ParseError, conduitParser, conduitParserEither, sinkParser, sinkParserEither)
import Data.CURL.CookieJar.Parser (cookieParser, cookieJarParser)
import Data.CURL.CookieJar.PrettyPrinter (CookieJarHeader, prettyCookie, prettyCookieJarHeader)
import Network.HTTP.Client (Cookie, CookieJar, createCookieJar, destroyCookieJar)

-- | Parse cookies from a cookie jar
--
-- Note that this does not skip the header
parseCookiesC :: MonadThrow m => ConduitT ByteString Cookie m ()
parseCookiesC = conduitParser cookieParser .| mapC snd

-- | Parse cookies from a cookie jar, returning errors as values
parseCookiesEitherC :: Monad m => ConduitT ByteString (Either ParseError Cookie) m ()
parseCookiesEitherC =  conduitParserEither cookieParser .| mapC (fmap snd)

-- | Parse cookies into a cookie jar, and return the value
sinkCookieJar :: MonadThrow m => ConduitT ByteString o m CookieJar
sinkCookieJar = sinkParser cookieJarParser

-- | Parse cookies into a cookie jar, and return the value, or a parse error
sinkCookieJarEither :: Monad m => ConduitT ByteString o m (Either ParseError CookieJar)
sinkCookieJarEither = sinkParserEither cookieJarParser

-- | Pretty print a single cookie and stream out a chunked ByteString
prettyCookieC
  :: PrimMonad m
  => Cookie
  -> ConduitT i ByteString m ()
prettyCookieC cookie =
  (yield $ prettyCookie cookie) .| builderToByteString

-- | Stream in cookies to pretty-print, along with a header
prettyCookiesC
  :: PrimMonad m
  => CookieJarHeader
  -> ConduitT Cookie ByteString m ()
prettyCookiesC header =
  cookiesAndHeader .| builderToByteString
  where
    cookiesAndHeader = do
      yield $ prettyCookieJarHeader header
      mapC prettyCookie

-- | Pretty-print a cookie jar
prettyCookieJarC
  :: PrimMonad m
  => CookieJarHeader
  -> CookieJar
  -> ConduitT i ByteString m ()
prettyCookieJarC header jar =
  sourceCookieJarCookies jar .| prettyCookiesC header

-- | Read a cookie jar from a file
readCookieJarFileC
  :: MonadThrow m
  => MonadResource m
  => FilePath
  -> ConduitT i o m CookieJar
readCookieJarFileC path = sourceCookiesFile path .| sinkCookieJarCookies

-- | Read a cookie jar from a handle
readCookieJarHandleC
  :: MonadIO m
  => MonadThrow m
  => Handle
  -> ConduitT i o m CookieJar
readCookieJarHandleC handle = sourceCookiesHandle handle .| sinkCookieJarCookies

-- | Source cookies from a cookie jar
sourceCookieJarCookies
  :: Monad m
  => CookieJar
  -> ConduitT i Cookie m ()
sourceCookieJarCookies =
  sourceList . destroyCookieJar

-- | Make a cookie jar from a finite stream of cookies
sinkCookieJarCookies
  :: Monad m
  => ConduitT Cookie o m CookieJar
sinkCookieJarCookies = createCookieJar <$> consume

-- | Source cookies from a cookie jar file
sourceCookiesFile
  :: MonadThrow m
  => MonadResource m
  => FilePath
  -> ConduitT i Cookie m ()
sourceCookiesFile path =
  sourceFile path .| parseCookiesC

-- | Source cookies from a handle
sourceCookiesHandle
  :: MonadThrow m
  => MonadIO m
  => Handle
  -> ConduitT i Cookie m ()
sourceCookiesHandle handle =
  sourceHandle handle .| parseCookiesC

-- | Write cookies to a file
sinkCookiesFile
  :: MonadResource m
  => PrimMonad m
  => CookieJarHeader
  -> FilePath
  -> ConduitT Cookie o m ()
sinkCookiesFile header path =
  prettyCookiesC header .| sinkFile path

-- | Write cookies to a handle
sinkCookiesHandle
  :: MonadIO m
  => PrimMonad m
  => CookieJarHeader
  -> Handle
  -> ConduitT Cookie o m ()
sinkCookiesHandle header handle =
  prettyCookiesC header .| sinkHandle handle

-- | Write a cookie jar to a file
writeCookieJarFileC
  :: MonadResource m
  => PrimMonad m
  => CookieJarHeader
  -> CookieJar
  -> FilePath
  -> ConduitT i o m ()
writeCookieJarFileC header jar path =
  sourceCookieJarCookies jar .| sinkCookiesFile header path

writeCookieJarHandleC
  :: MonadIO m
  => PrimMonad m
  => CookieJarHeader
  -> CookieJar
  -> Handle
  -> ConduitT i o m ()
writeCookieJarHandleC header jar handle =
  sourceCookieJarCookies jar .| sinkCookiesHandle header handle

