-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import qualified Distribution.Client.PackageIndex()
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache )
import qualified Paths_cabal_install()

import Distribution.Package()
import Distribution.Version()
import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import qualified Data.Map()
import System.FilePath (dropExtension)
import Data.Maybe()
import Control.Monad()

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity [] =
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update verbosity repos = do
  mapM_ (updateRepo verbosity) repos

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    notice verbosity $ "Downloading the latest package list from "
                    ++ remoteRepoName remoteRepo
    indexPath <- downloadIndex verbosity remoteRepo (repoLocalDir repo)
    writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                            =<< BS.readFile indexPath
    updateRepoIndexCache verbosity repo