module Data.CURL.CookieJar 
  ( readCookieJarFile
  , writeCookieJarFile
  , CookieJarHeader(..)
  ) where

import Conduit (runResourceT, runConduit)
import Data.CURL.CookieJar.Conduit
import Data.CURL.CookieJar.PrettyPrinter
import Network.HTTP.Client (CookieJar)

readCookieJarFile :: FilePath -> IO CookieJar
readCookieJarFile path = runResourceT $ runConduit $ readCookieJarFileC path

writeCookieJarFile :: CookieJarHeader -> FilePath -> CookieJar -> IO ()
writeCookieJarFile header path jar = runResourceT $ runConduit $ writeCookieJarFileC header jar path


