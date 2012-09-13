-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Client.IndexUtils (
  getInstalledPackages,
  getSourcePackages,
  convert,

  readPackageIndexFile,
  parsePackageIndex,
  readRepoIndex,
  updateRepoIndexCache,
  ) where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types

import Distribution.Package
         ( PackageId, PackageIdentifier(..), PackageName(..)
         , Package(..), packageVersion, packageName
         , Dependency(Dependency), InstalledPackageId(..) )
import Distribution.Client.PackageIndex (PackageIndex)
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import qualified Distribution.PackageDescription.Parse as PackageDesc.Parse
import Distribution.PackageDescription
         ( GenericPackageDescription )
import Distribution.PackageDescription.Parse
         ( parsePackageDescription )
import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import qualified Distribution.Simple.Configure as Configure
         ( getInstalledPackages )
import Distribution.ParseUtils
         ( ParseResult(..) )
import Distribution.Version
         ( Version(Version), intersectVersionRanges )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( die, warn, info, fromUTF8, findPackageDesc )

import Data.Char   (isAlphaNum)
import Data.Maybe  (catMaybes, fromMaybe)
import Data.List   (isPrefixOf)
import Data.Monoid (Monoid(..))
import qualified Data.Map as Map
import Control.Monad (MonadPlus(mplus), when, unless, liftM)
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import Distribution.Client.GZipUtils (maybeDecompress)
import Distribution.Client.Utils (byteStringToFilePath)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)
import Distribution.Compat.Exception (catchIO)
import System.Directory
         ( getModificationTime, doesFileExist )
import Distribution.Compat.Time


getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex.PackageIndex
getInstalledPackages verbosity comp packageDbs conf =
    Configure.getInstalledPackages verbosity' comp packageDbs conf
  where
    --FIXME: make getInstalledPackages use sensible verbosity in the first place
    verbosity'  = lessVerbose verbosity

convert :: InstalledPackageIndex.PackageIndex -> PackageIndex InstalledPackage
convert index' = PackageIndex.fromList
    -- There can be multiple installed instances of each package version,
    -- like when the same package is installed in the global & user dbs.
    -- InstalledPackageIndex.allPackagesBySourcePackageId gives us the
    -- installed packages with the most preferred instances first, so by
    -- picking the first we should get the user one. This is almost but not
    -- quite the same as what ghc does.
    [ InstalledPackage ipkg (sourceDeps index' ipkg)
    | (_,ipkg:_) <- InstalledPackageIndex.allPackagesBySourcePackageId index' ]
  where
    -- The InstalledPackageInfo only lists dependencies by the
    -- InstalledPackageId, which means we do not directly know the corresponding
    -- source dependency. The only way to find out is to lookup the
    -- InstalledPackageId to get the InstalledPackageInfo and look at its
    -- source PackageId. But if the package is broken because it depends on
    -- other packages that do not exist then we have a problem we cannot find
    -- the original source package id. Instead we make up a bogus package id.
    -- This should have the same effect since it should be a dependency on a
    -- non-existant package.
    sourceDeps index ipkg =
      [ maybe (brokenPackageId depid) packageId mdep
      | let depids = InstalledPackageInfo.depends ipkg
            getpkg = InstalledPackageIndex.lookupInstalledPackageId index
      , (depid, mdep) <- zip depids (map getpkg depids) ]

    brokenPackageId (InstalledPackageId str) =
      PackageIdentifier (PackageName (str ++ "-broken")) (Version [] [])

------------------------------------------------------------------------
-- Reading the source package index
--

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'SourcePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
getSourcePackages :: Verbosity -> [Repo] -> IO SourcePackageDb
getSourcePackages verbosity [] = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
  return SourcePackageDb {
    packageIndex       = mempty,
    packagePreferences = mempty
  }
getSourcePackages verbosity repos = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (readRepoIndex verbosity) repos
  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith intersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  _ <- evaluate pkgs
  _ <- evaluate prefs'
  return SourcePackageDb {
    packageIndex       = pkgs,
    packagePreferences = prefs'
  }

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> Repo
              -> IO (PackageIndex SourcePackage, [Dependency])
readRepoIndex verbosity repo =
  let indexFile = repoLocalDir repo </> "00-index.tar"
      cacheFile = repoLocalDir repo </> "00-index.cache"
  in handleNotFound $ do
    warnIfIndexIsOld indexFile
    whenCacheOutOfDate indexFile cacheFile $ do
      info verbosity $ "Updating the index cache file..."
      updatePackageIndexCacheFile indexFile cacheFile
    readPackageIndexCacheFile mkAvailablePackage indexFile cacheFile

  where
    mkAvailablePackage pkgEntry =
      SourcePackage {
        packageInfoId      = pkgid,
        packageDescription = packageDesc pkgEntry,
        packageSource      = case pkgEntry of
          NormalPackage _ _ _      -> RepoTarballPackage repo pkgid Nothing
          BuildTreeRef  _ path _ _ -> LocalUnpackedPackage path
      }
      where
        pkgid = packageId pkgEntry

    handleNotFound action = catchIO action $ \e -> if isDoesNotExistError e
      then do
        case repoKind repo of
          Left  remoteRepo -> warn verbosity $
               "The package list for '" ++ remoteRepoName remoteRepo
            ++ "' does not exist. Run 'hackport update' to download it."
          Right _localRepo -> warn verbosity $
               "The package list for the local repo '" ++ repoLocalDir repo
            ++ "' is missing. The repo is invalid."
        return mempty
      else ioError e

    isOldThreshold = 15 --days
    warnIfIndexIsOld indexFile = do
      dt <- getFileAge indexFile
      when (dt >= isOldThreshold) $ case repoKind repo of
        Left  remoteRepo -> warn verbosity $
             "The package list for '" ++ remoteRepoName remoteRepo
          ++ "' is " ++ show (tdDay diff)  ++ " days old.\nRun "
          ++ "'hackport update' to get the latest list of available packages."
        Right _localRepo -> return ()

-- | It is not necessary to call this, as the cache will be updated when the
-- index is read normally. However you can do the work earlier if you like.
--
updateRepoIndexCache :: Verbosity -> Repo -> IO ()
updateRepoIndexCache verbosity repo =
    whenCacheOutOfDate indexFile cacheFile $ do
      info verbosity $ "Updating the index cache file..."
      updatePackageIndexCacheFile indexFile cacheFile
  where
    indexFile = repoLocalDir repo </> "00-index.tar"
    cacheFile = repoLocalDir repo </> "00-index.cache"

whenCacheOutOfDate :: FilePath-> FilePath -> IO () -> IO ()
whenCacheOutOfDate origFile cacheFile action = do
  exists <- doesFileExist cacheFile
  if not exists
    then action
    else do
      origTime  <- getModificationTime origFile
      cacheTime <- getModificationTime cacheFile
      unless (cacheTime >= origTime) action


------------------------------------------------------------------------
-- Reading the index file
--

-- | An index entry is either a normal package, or a local build tree reference.
data PackageEntry = NormalPackage PackageId GenericPackageDescription BlockNo
                  | BuildTreeRef PackageId FilePath GenericPackageDescription
                    BlockNo

type MkPackageEntry = IO PackageEntry

instance Package PackageEntry where
  packageId (NormalPackage pkgid _ _   ) = pkgid
  packageId (BuildTreeRef  pkgid _ _ _ ) = pkgid

packageDesc :: PackageEntry -> GenericPackageDescription
packageDesc (NormalPackage _   descr _ ) = descr
packageDesc (BuildTreeRef  _ _ descr _ ) = descr

-- | Read a compressed \"00-index.tar.gz\" file into a 'PackageIndex'.
--
-- This is supposed to be an \"all in one\" way to easily get at the info in
-- the hackage package index.
--
-- It takes a function to map a 'GenericPackageDescription' into any more
-- specific instance of 'Package' that you might want to use. In the simple
-- case you can just use @\_ p -> p@ here.
--
readPackageIndexFile :: Package pkg
                     => (PackageEntry -> pkg)
                     -> FilePath
                     -> IO (PackageIndex pkg, [Dependency])
readPackageIndexFile mkPkg indexFile = do
  (mkPkgs, prefs) <- either fail return
                     . parsePackageIndex
                     . maybeDecompress
                     =<< BS.readFile indexFile

  pkgEntries  <- sequence mkPkgs
  pkgs <- evaluate $ PackageIndex.fromList (map mkPkg pkgEntries)
  return (pkgs, prefs)

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--
parsePackageIndex :: ByteString
                  -> Either String ([MkPackageEntry], [Dependency])
parsePackageIndex = accum 0 [] [] . Tar.read
  where
    accum blockNo pkgs prefs es = case es of
      Tar.Fail err   -> Left  err
      Tar.Done       -> Right (reverse pkgs, reverse prefs)
      Tar.Next e es' -> accum blockNo' pkgs' prefs' es'
        where
          (pkgs', prefs') = extract blockNo pkgs prefs e
          blockNo'        = blockNo + Tar.entrySizeInBlocks e

    extract blockNo pkgs prefs entry =
       fromMaybe (pkgs, prefs) $
                 tryExtractPkg
         `mplus` tryExtractPrefs
      where
        tryExtractPkg = do
          mkPkgEntry <- extractPkg entry blockNo
          return (mkPkgEntry:pkgs, prefs)

        tryExtractPrefs = do
          prefs' <- extractPrefs entry
          return (pkgs, prefs'++prefs)

extractPkg :: Tar.Entry -> BlockNo -> Maybe MkPackageEntry
extractPkg entry blockNo = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeExtension fileName == ".cabal"
    -> case splitDirectories (normalise fileName) of
        [pkgname,vers,_] -> case simpleParse vers of
          Just ver -> Just $ return (NormalPackage pkgid descr blockNo)
            where
              pkgid  = PackageIdentifier (PackageName pkgname) ver
              parsed = parsePackageDescription . fromUTF8 . BS.Char8.unpack
                                               $ content
              descr  = case parsed of
                ParseOk _ d -> d
                _           -> error $ "Couldn't read cabal file "
                                    ++ show fileName
          _ -> Nothing
        _ -> Nothing

  Tar.OtherEntryType typeCode content _
    | typeCode == Tar.buildTreeRefTypeCode ->
      Just $ do
        let path   = byteStringToFilePath content
        cabalFile <- findPackageDesc path
        descr     <- PackageDesc.Parse.readPackageDescription normal cabalFile
        return $ BuildTreeRef (packageId descr) path descr blockNo

  _ -> Nothing

  where
    fileName = Tar.entryPath entry

extractPrefs :: Tar.Entry -> Maybe [Dependency]
extractPrefs entry = case Tar.entryContent entry of
{-
 -- get rid of hackage's preferred-versions
 -- I'd like to have bleeding-edge packages in system and I don't fear of
 -- broken packages with improper depends
  Tar.NormalFile content _
     | takeFileName (Tar.entryPath entry) == "preferred-versions"
    -> Just . parsePreferredVersions
     . BS.Char8.unpack $ content
-}
  _ -> Nothing

parsePreferredVersions :: String -> [Dependency]
parsePreferredVersions = catMaybes
                       . map simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines

------------------------------------------------------------------------
-- Reading and updating the index cache
--

updatePackageIndexCacheFile :: FilePath -> FilePath -> IO ()
updatePackageIndexCacheFile indexFile cacheFile = do
    (mkPkgs, prefs) <- either fail return
                       . parsePackageIndex
                       . maybeDecompress
                       =<< BS.readFile indexFile
    pkgEntries <- sequence mkPkgs
    let cache = mkCache pkgEntries prefs
    writeFile cacheFile (showIndexCache cache)
  where
    mkCache pkgs prefs =
        [ CachePreference pref          | pref <- prefs ]
     ++ [ CachePackageId pkgid blockNo
        | (NormalPackage pkgid _ blockNo) <- pkgs ]
     ++ [ CacheBuildTreeRef blockNo
        | (BuildTreeRef _ _ _ blockNo) <- pkgs]

readPackageIndexCacheFile :: Package pkg
                          => (PackageEntry -> pkg)
                          -> FilePath
                          -> FilePath
                          -> IO (PackageIndex pkg, [Dependency])
readPackageIndexCacheFile mkPkg indexFile cacheFile = do
  indexHnd <- openFile indexFile ReadMode
  cache    <- liftM readIndexCache (BSS.readFile cacheFile)
  packageIndexFromCache mkPkg indexHnd cache


packageIndexFromCache :: Package pkg
                      => (PackageEntry -> pkg)
                      -> Handle
                      -> [IndexCacheEntry]
                      -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache mkPkg hnd = accum mempty []
  where
    accum srcpkgs prefs [] = do
      -- Have to reverse entries, since in a tar file, later entries mask
      -- earlier ones, and PackageIndex.fromList does the same, but we
      -- accumulate the list of entries in reverse order, so need to reverse.
      pkgIndex <- evaluate $ PackageIndex.fromList (reverse srcpkgs)
      return (pkgIndex, prefs)

    accum srcpkgs prefs (CachePackageId pkgid blockno : entries) = do
      -- Given the cache entry, make a package index entry.
      -- The magic here is that we use lazy IO to read the .cabal file
      -- from the index tarball if it turns out that we need it.
      -- Most of the time we only need the package id.
      pkg <- unsafeInterleaveIO $ do
        getEntryContent blockno >>= readPackageDescription
      let srcpkg = mkPkg (NormalPackage pkgid pkg blockno)
      accum (srcpkg:srcpkgs) prefs entries

    accum srcpkgs prefs (CacheBuildTreeRef blockno : entries) = do
      -- We have to read the .cabal file eagerly here because we can't cache the
      -- package id for build tree references - the user might edit the .cabal
      -- file after the reference was added to the index.
      path <- liftM byteStringToFilePath . getEntryContent $ blockno
      pkg  <- do cabalFile <- findPackageDesc path
                 PackageDesc.Parse.readPackageDescription normal cabalFile
      let srcpkg = mkPkg (BuildTreeRef (packageId pkg) path pkg blockno)
      accum (srcpkg:srcpkgs) prefs entries

    accum srcpkgs prefs (CachePreference pref : entries) =
      accum srcpkgs (pref:prefs) entries

    getEntryContent :: BlockNo -> IO ByteString
    getEntryContent blockno = do
      hSeek hnd AbsoluteSeek (fromIntegral (blockno * 512))
      header  <- BS.hGet hnd 512
      size    <- getEntrySize header
      BS.hGet hnd (fromIntegral size)

    getEntrySize :: ByteString -> IO Tar.FileSize
    getEntrySize header =
      case Tar.read header of
        Tar.Next e _ ->
          case Tar.entryContent e of
            Tar.NormalFile _ size -> return size
            Tar.OtherEntryType typecode _ size
              | typecode == Tar.buildTreeRefTypeCode
                                  -> return size
            _                     -> interror "unexpected tar entry type"
        _ -> interror "could not read tar file entry"

    readPackageDescription :: ByteString -> IO GenericPackageDescription
    readPackageDescription content =
      case parsePackageDescription . fromUTF8 . BS.Char8.unpack $ content of
        ParseOk _ d -> return d
        _           -> interror "failed to parse .cabal file"

    interror msg = die $ "internal error when reading package index: " ++ msg
                      ++ "The package index or index cache is probably "
                      ++ "corrupt. Running 'hackport update' might fix it."

------------------------------------------------------------------------
-- Index cache data structure
--

-- | Tar files are block structured with 512 byte blocks. Every header and file
-- content starts on a block boundary.
--
type BlockNo = Int

data IndexCacheEntry = CachePackageId PackageId BlockNo
                     | CacheBuildTreeRef BlockNo
                     | CachePreference Dependency
  deriving (Eq, Show)

readIndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
readIndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == packageKey && sep == blocknoKey ->
      case (parseName pkgnamestr, parseVer pkgverstr [],
            parseBlockNo blocknostr) of
        (Just pkgname, Just pkgver, Just blockno)
          -> Just (CachePackageId (PackageIdentifier pkgname pkgver) blockno)
        _ -> Nothing
    [key, blocknostr] | key == buildTreeRefKey ->
      case parseBlockNo blocknostr of
        Just blockno -> Just (CacheBuildTreeRef blockno)
        _            -> Nothing
    (key: remainder) | key == preferredVersionKey ->
      fmap CachePreference (simpleParse (BSS.unpack (BSS.unwords remainder)))
    _  -> Nothing
  where
    packageKey = BSS.pack "pkg:"
    blocknoKey = BSS.pack "b#"
    buildTreeRefKey     = BSS.pack "build-tree-ref:"
    preferredVersionKey = BSS.pack "pref-ver:"

    parseName str
      | BSS.all (\c -> isAlphaNum c || c == '-') str
                  = Just (PackageName (BSS.unpack str))
      | otherwise = Nothing

    parseVer str vs =
      case BSS.readInt str of
        Nothing        -> Nothing
        Just (v, str') -> case BSS.uncons str' of
          Just ('.', str'') -> parseVer str'' (v:vs)
          Just _            -> Nothing
          Nothing           -> Just (Version (reverse (v:vs)) [])

    parseBlockNo str =
      case BSS.readInt str of
        Just (blockno, remainder) | BSS.null remainder -> Just blockno
        _                                              -> Nothing

showIndexCacheEntry :: IndexCacheEntry -> String
showIndexCacheEntry entry = case entry of
   CachePackageId pkgid b -> "pkg: " ++ display (packageName pkgid)
                                  ++ " " ++ display (packageVersion pkgid)
                          ++ " b# " ++ show b
   CacheBuildTreeRef b    -> "build-tree-ref: " ++ show b
   CachePreference dep    -> "pref-ver: " ++ display dep

readIndexCache :: BSS.ByteString -> [IndexCacheEntry]
readIndexCache = catMaybes . map readIndexCacheEntry . BSS.lines

showIndexCache :: [IndexCacheEntry] -> String
showIndexCache = unlines . map showIndexCacheEntry
