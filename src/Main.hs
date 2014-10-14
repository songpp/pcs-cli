{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Main where

import           Api
import           Control.Applicative
import           Control.Monad          (liftM, void)
import           Data.Aeson
import           Data.List              (isPrefixOf, isSuffixOf)
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment     
import           System.FilePath
import           Text.Printf            (printf)
import           Token
import           Util


data Args = Info
          | Auth
          | Quota
          | Search { basePath  :: String
                   , keyword   :: String
                   , recursive :: Bool
                   }
          | Upload { file      :: FilePath
                   , path      :: FilePath
                   , overwrite :: Bool
                   }
          | Download { file      :: FilePath
                     , path      :: FilePath
                     , overwrite :: Bool
                     }
    deriving (Show, Data, Typeable)

info = Info &= name "info"
auth = Auth &= name "auth" &= help "申请Token，采用DeviceCode方式。"
quota = Quota &= name "quota" &= help "查询空间使用情况"
search = Search { basePath  = def &= opt "/" &= typ "PATH"
                                  &= help "在这个路径下搜索，默认 / (APP的根目录)",
                  keyword   = def &= help "关键字" ,
                  recursive = def &= help "是否递归搜索子文件夹"
              } &= name "search"  &= help "搜索文件"

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
    cmd <- (if null args then withArgs ["--help"] else id) . cmdArgs $ modes  [ auth, quota
                            , Main.search, Main.download , Main.upload
                            ] &= program "pcs-cli"
    case cmd of
        --Info  -> currentTokenConfig >>= handleInfo
        Quota -> quotaInfo
        Auth  -> void askNewAccessToken
        Search{..} -> Api.search basePath keyword recursive
        d @ Download{} -> handleDownload d
        u @ Upload {} -> handleUpload u



handleDownload :: Args -> IO ()
handleDownload Download{..} = do
    let remote = if checkRemotePath
                then error ("要下载的路径必须是一个绝对路径文件：" ++ file)
                else file

    local <- absFilePath path
    let local' = rp remote local
    ex <- doesFileExist local'
    isDir <- doesDirectoryExist local'
    if not overwrite && (ex || isDir)
      then printf ("%s 已经存在或者路径是一个目录, " ++
                      "使用-o or --overwrite 可以覆盖本地文件\n") local'
      else Api.download remote local'
  where
    checkRemotePath = "/" `isSuffixOf` file || not ("/" `isPrefixOf` file)
    rp remote local= if "/" `isSuffixOf` local
                        then joinPath [local, takeFileName remote]
                        else local


handleUpload :: Args -> IO ()
handleUpload Upload{..} = do
    local <- absFilePath file
    ex <- doesFileExist local
    if not ex
      then error $ printf "本地文件%s不存在！" local
      else Api.upload (rp path local) local overwrite
  where
    rp remotePath localFile =
          if "/" `isSuffixOf` remotePath
              then joinPath [remotePath, takeFileName localFile]
              else remotePath



