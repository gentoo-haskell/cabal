-- | Provides the version number of @cabal-install@.

module Distribution.Client.Version
  ( cabalInstallVersion
  ) where

import Distribution.Version

-- |
-- This value determines the output of `cabal-install --version`.
cabalInstallVersion :: Version
cabalInstallVersion = mkVersion [3,10,3,0]
