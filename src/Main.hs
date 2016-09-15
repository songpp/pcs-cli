{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Api
import           Control.Applicative
import           Control.Monad          (liftM, void)
import           Data.Aeson
import           Data.List              (intercalate, isPrefixOf, isSuffixOf,
                                         stripPrefix)
import           PCS
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf            (printf)
import           Token
import           Util

data Args where
  Info   :: Args
  Auth   :: Args
  Quota  :: Args
  SimpleSearch :: { keyword :: String } -> Args
  Search :: { basePath  :: String
            , keyword   :: String
            , recursive :: Bool } -> Args
  Upload :: { file      :: FilePath
            , path      :: FilePath
            , overwrite :: Bool } -> Args
  Download :: { file      :: FilePath
              , path      :: FilePath
              , overwrite :: Bool } -> Args

deriving instance Show Args
deriving instance Data Args
deriving instance Typeable Args

info = Info &= name "info"
auth = Auth &= name "auth" &= help "申请Token，采用DeviceCode方式。"
quota = Quota &= name "quota" &= help "查询空间使用情况"
search = Search { basePath  = def &= opt ("/" :: String)
                                  &= typ "PATH"
                                  &= help "在这个路径下搜索，默认 / (APP的根目录)",
                  keyword   = def &= typ "keyword" &= help "搜索关键字",
                  recursive = def &= help "是否递归搜索子文件夹"
              } &= name "search"  &= help "搜索文件"

find = SimpleSearch {
  keyword = def &= typ "keyword" &= argPos 0
  } &= name "find" &= help "在整个app目录下搜索文件"

download = Download { file      = def &= name "file" &= typ "FILE"
                                      &= help "要下载的远程云盘文件路径，/表示APP根路径"
                    , path      = def &= typ "FILE" &= help "要下载本地磁盘路径"
                    , overwrite = def
                               &= help "如果本地文件已经存在是否覆盖，默认不覆盖"
                    } &= name "download" &= help "下载文件到指定目录"

upload = Upload { file = def &= typ "FILE" &= help "要上传的文件"
                , path = def &= typ "PATH"
                             &= help "上传到云盘的路径，不需要带应用的路径前缀"
                , overwrite   = def
                             &= help "是否覆盖云盘上已经存在的文件"
                } &= name "upload" &= help "上传文件"


main :: IO ()
main = do
    args <- getArgs
    cmd <- (if null args then withArgs ["--help"] else id) . cmdArgs
              $ modes [ auth, quota , find
                      , Main.search, Main.download , Main.upload ]
                  &= program "pcs-cli"

    config <- readAppConfig
    run cmd config


class HandleCommand a where
  run :: a -> PcsConfig -> IO ()

instance HandleCommand Args where
  -- | 查询空间
  run Quota c = runPcsT c Api.quotaInfo >>= print

  -- | 搜索文件
  run (SimpleSearch k) c = run (Search "/" k True) c
  run Search{..} c = runPcsT c (Api.search basePath keyword recursive)
                 >>= printSearchResult
    where
      printSearchResult :: Either Err SearchResult -> IO ()
      printSearchResult (Right (SearchResult lists)) = do
        let appPath = extractAppPath c
        putStrLn $ printf "共找到: %d" (length lists)
        let result = intercalate "\n" . map (dropAppPathAndToString appPath) $ lists
        putStrLn result

      printSearchResult (Left err) = print err

      dropAppPathAndToString :: String -> SearchResultItem -> String
      dropAppPathAndToString appPath SearchResultItem{ abPath, .. }
        | Just relativePath <- stripPrefix appPath abPath = relativePath
        | otherwise = abPath

      extractAppPath (PcsConfig (Vars AppConfig {appPath, ..} _) _) = appPath



  -- | 下载
  run Download{..} c = do
      let remote = if checkRemotePath
                    then error ("要下载的路径必须是一个绝对路径文件：" ++ file)
                    else file
      -- 如果指定本地路径是一个目录
      local <- absFilePath path
      let local' = rp remote local
      ex <- doesFileExist local'
      isDir <- doesDirectoryExist local'
      let realLocal = if isDir then joinPath [local, takeFileName remote] else local'
      if | not overwrite && ex ->
            printf ("%s 已经存在" ++
                        "使用-o or --overwrite 可以覆盖本地文件\n") realLocal
         | otherwise -> do
              result <- runPcsT c (Api.download remote realLocal)
              print result
    where
      checkRemotePath = "/" `isSuffixOf` file || not ("/" `isPrefixOf` file)
      rp remote local= if "/" `isSuffixOf` local
                          then joinPath [local, takeFileName remote]
                          else local

  -- | 上传
  run Upload{..}  c = do
      local <- absFilePath file
      ex <- doesFileExist local
      if not ex
        then error $ printf "本地文件%s不存在！" local
        else do
              result <- runPcsT c (Api.upload (rp path local) local overwrite)
              print result
    where
      rp remotePath localFile =
            if "/" `isSuffixOf` remotePath
                then joinPath [remotePath, takeFileName localFile]
                else remotePath

  -- | 申请新token
  run Auth c = void askNewAccessToken

