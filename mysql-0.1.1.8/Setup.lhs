#!/usr/bin/env runhaskell

\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{- OPTIONS_GHC -Wall #-}

import Control.Monad (liftM2, mplus)
import Data.List (isPrefixOf)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity

-- A Cabal 1.16 vs 1.18 compatibility hack, as in 1.18
-- findProgramLocation has a new (unused in this case) parameter.
-- ConstOrId adds this parameter when types say it is mandatory.
class ConstOrId a b where
    constOrId :: a -> b

instance ConstOrId a a where
    constOrId = id

instance ConstOrId a (b -> a) where
    constOrId = const


main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [mysqlConfigProgram],

  confHook = \pkg flags -> do
    lbi <- confHook simpleUserHooks pkg flags
    bi  <- mysqlBuildInfo lbi
    return lbi {
      localPkgDescr = updatePackageDescription (Just bi, []) (localPkgDescr lbi)
    }
}

mysqlConfigProgram = (simpleProgram "mysql_config") {
    programFindLocation = \verbosity -> constOrId $ liftM2 mplus
      (findProgramLocation verbosity "mysql_config")
      (findProgramLocation verbosity "mysql_config5")
  }

mysqlBuildInfo :: LocalBuildInfo -> IO BuildInfo
mysqlBuildInfo lbi = do
  let mysqlConfig = fmap words . rawSystemProgramStdoutConf normal
                    mysqlConfigProgram (withPrograms lbi)

  --include <- mysqlConfig ["--include"]
  let include = ["-I/usr/msys64/home/m/libmysql/include"]
  --libs <- mysqlConfig ["--libs"]
  -- let libs = ["-L/c/Program Files/MySQL/MySQL Server 5.7/lib", "-lmysqlclient", "ws2_32", "Secur32"]
  let libs = ["-L/usr/msys64/home/m/libmysql/lib", "-lmysqlclient"]

  return emptyBuildInfo {
    extraLibDirs = map (drop 2) . filter ("-L" `isPrefixOf`) $ libs
  , extraLibs = map (drop 2) . filter ("-l" `isPrefixOf`) .
                filter (/= "-lmygcc") $ libs
  , includeDirs = map (drop 2) include
  }
\end{code}
