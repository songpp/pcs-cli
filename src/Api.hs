{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Api
        ( quotaInfo
        , download
        , search
        , upload
        , Quota
        , Err
        , SearchResult(..)
        , SearchResultItem(..)
        , DownloadResult(..)
        , Resp
        ) where

import           Control.Applicative
import           Control.Monad              (liftM)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.UTF8       as U
import qualified Data.Conduit               as C
import           Data.Conduit.Binary        (sinkFile)
import           Data.List.Utils            (replace)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         as E
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method  (StdMethod (GET, POST))
import           PCS
import           System.FilePath
import           Text.Printf                (printf)
import           Token
import           Util


type Resp a = Either Err a
type RawResponse = Response L.ByteString


quotaInfo :: PcsT m (Resp Quota)
quotaInfo = do
    PcsConfig {vars, pcsManager} <- ask
    liftIO $ sendRequest pcsManager baseUrl (mkParams vars) GET
  where
    baseUrl = pcsUrl ++ "quota"
    mkParams v = Map.fromList [("method", "info"), accessTokenParam v]


search :: FilePath -> String -> Bool -> PcsT m (Resp SearchResult)
search path keyword recursive = do
    PcsConfig {vars, pcsManager} <- ask
    let params = Map.fromList [("wd", keyword),
            ("path", prependAppPath path vars),
            ("access_token", extractAccToken vars),
            ("method", "search"),
            ("re", if recursive then "1" else "0")]
    liftIO $ sendRequest pcsManager url params GET
  where
    url = pcsUrl ++ "file"


upload :: String -> String -> Bool -> PcsT m (Resp SearchResultItem)
upload path file overwrite = do
    PcsConfig {vars, pcsManager} <- ask
    let url = mkUrl vars
    liftIO $ do
        r <- uploadFile pcsManager url Map.empty file :: IO RawResponse
        return $ handleJSONResponse r
  where
    mkUrl :: Vars -> String
    mkUrl v = buildUrl baseUrl (mkParams v)
    baseUrl :: String
    baseUrl = cPcsUrl ++ "file"
    mkParams v = Map.fromList [accessTokenParam v,
                    ("ondup", if overwrite then "overwrite" else "newcopy"),
                    ("path", prependAppPath path v),
                    ("method", "upload") ]

download :: String -> String -> PcsT m (Resp DownloadResult)
download path targetPath = do
    PcsConfig {vars, pcsManager} <- ask
    liftIO $ doRequest pcsManager (mkUrl vars) Map.empty GET >>= \r ->
                if responseSuccess r
                then liftM Right (handleResponse r)
                else return $ Left (parseResponseJSON $ responseBody r)
  where
    handleResponse :: RawResponse -> IO DownloadResult
    handleResponse response =
        L.writeFile targetPath (responseBody response) >>
        return (DownloadResult path targetPath)


    parseResponseJSON body = fromMaybe
        (error $ "parse JSON result error: " ++ LC.unpack body)
        (decode body)

    mkUrl v = buildUrl baseUrl (mkParams v)

    baseUrl :: String
    baseUrl = dPcsUrl ++ "file"
    mkParams v = Map.fromList [accessTokenParam v,
                ("method", "download"),
                ("path", prependAppPath path v)]


prependAppPath :: String -> Vars -> String
prependAppPath p = replace "\\" "/" . normalise . (++ p) . appPath . appConfig



pcsUrl, cPcsUrl, dPcsUrl :: String
pcsUrl = "https://pcs.baidu.com/rest/2.0/pcs/"
cPcsUrl = "https://c.pcs.baidu.com/rest/2.0/pcs/"
dPcsUrl = "https://d.pcs.baidu.com/rest/2.0/pcs/"

extractAppPath (Vars AppConfig {appPath, ..} _) = appPath

extractAccToken (Vars _ (Just (TokenResp {accessToken, ..}))) = accessToken

accessTokenParam = (,) "access_token" . extractAccToken


handleResult :: Show a => Resp a -> IO ()
handleResult (Right q) = print q
handleResult (Left e) = handleErr e


handleErr (Err c m r) = error $ printf "请求出错了：[ %d ] => [ %s ]\n" c m


data DownloadResult = DownloadResult {
        path   :: FilePath,
        target :: FilePath
    } deriving Show


data SearchResultItem = SearchResultItem {
        fsId   :: Integer,
        abPath :: String,
        ctime  :: Integer,
        mtime  :: Integer,
        md5    :: String,
        size   :: Integer,
        isDir  :: Bool
    }

instance Show SearchResultItem where
    show SearchResultItem{fsId, abPath, ctime, mtime, md5, size, isDir} =
        printf "%s" abPath


data SearchResult = SearchResult {
        lists :: [SearchResultItem]
    }

instance Show SearchResult where
    show SearchResult { lists }
        | null lists = "没有找到。\n"
        | otherwise  = printf "=> 搜索结果: %d\n%s" (length lists) (join lists "")

        where
            prefix = " => "
            join xs acc =
                foldl (\ acc x -> acc ++ prefix ++ show x ++ "\n") acc xs


instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$> v .: "list"


instance FromJSON SearchResultItem where
    parseJSON (Object v) = SearchResultItem <$> v .: "fs_id"
          <*> liftM (T.unpack . decodeUtf8 . U.fromString) (v .: "path")
          <*> v .: "ctime"
          <*> v .: "mtime"
          <*> v .: "md5"
          <*> v .: "size"
          <*> liftM int2bool (v .:? "isdir" .!= 1)
        where
            int2bool :: Int -> Bool
            int2bool = (> 0)

-- error
data Err = Err {
        code   :: Integer,
        msg    :: String,
        eReqId :: Integer
    } deriving (Show)


instance FromJSON Err where
    parseJSON (Object v) = Err <$>
                v .: "error_code" <*>
                v .: "error_msg" <*>
                v .: "request_id"


data Quota = Quota {  quota :: Integer, used  :: Integer} deriving (Eq)

instance Show Quota where
    show Quota { quota, used } =
            printf "当前配额: %.2f G, 已经使用: %.2f G" total used'
        where
            total, used' :: Double
            total = fromIntegral quota / toG
            used' = fromIntegral used / toG
            toG =  1024 * 1024 * 1024


instance FromJSON Quota where
    parseJSON (Object v) = Quota <$>
                v .: "quota" <*>
                v .: "used"
