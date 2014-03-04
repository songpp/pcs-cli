{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Api
        ( quotaInfo
        , download
        , search
        , upload
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
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         as E
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method  (StdMethod (GET, POST))
import           System.FilePath
import           Text.Printf                (printf)
import           Token
import           Util



type Resp a = Either a Err
type RawResponse = Response L.ByteString



-- | 查询配额和使用情况
quotaInfo =
        currentTokenConfig >>= runReaderT quotaInfoT >>= liftIO . handleResult
    where
        quotaInfoT :: ReaderT Vars IO (Resp Quota)
        quotaInfoT = do
            vs <- ask
            let params = mkParams vs
            liftIO $ sendRequest baseUrl params GET

        baseUrl = pcsUrl ++ "quota"
        mkParams v = Map.fromList [("method", "info"), accessTokenParam v]


search path wd recur =
        currentTokenConfig >>= runReaderT searchT >>= liftIO . handleResult
    where
        searchT :: ReaderT Vars IO (Resp SearchResult)
        searchT = do
            vs <- ask
            let params = Map.fromList [("wd", wd),
                    ("path", prependAppPath path vs),
                    ("access_token", extractAccToken vs),
                    ("method", "search"),
                    ("re", if recur then "1" else "0")]
            liftIO $ sendRequest url params GET
        url = pcsUrl ++ "file"


-- |
upload path file ovr =
        currentTokenConfig >>= runReaderT uploadT >>= liftIO . handleResult
    where
        uploadT :: ReaderT Vars IO (Resp (Maybe Value))
        uploadT = do
            vs <- ask
            let url = mkUrl vs
            liftIO $ do
                r <- uploadFile url Map.empty file :: IO RawResponse
                return $ handleJSONResponse r


        mkUrl :: Vars -> String
        mkUrl v = buildUrl baseUrl (mkParams v)

        baseUrl :: String
        baseUrl = cPcsUrl ++ "file"
        mkParams v = Map.fromList [accessTokenParam v,
                        ("ondup", if ovr then "overwrite" else "newcopy"),
                        ("path", prependAppPath path v),
                        ("method", "upload") ]


-- |
download :: String -> String -> IO ()
download path targetPath =
        currentTokenConfig >>= runReaderT downloadT >>= liftIO . handleResult
    where
        downloadT :: ReaderT Vars IO (Resp DownloadResult)
        downloadT = do
            vs <- ask
            liftIO $ doRequest (mkUrl vs) Map.empty GET >>= \r ->
                        if responseSuccess r
                        then liftM Left (handleResponse r)
                        else return $ Right (parseResponseJSON $ responseBody r)

        handleResponse :: RawResponse -> IO DownloadResult
        handleResponse response =
            L.writeFile targetPath (responseBody response) >>
            return (DownloadResult targetPath)


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
prependAppPath p v = normalise . (++ p) . appPath . appConfig $ v


pcsUrl, cPcsUrl, dPcsUrl :: String
pcsUrl = "https://pcs.baidu.com/rest/2.0/pcs/"
cPcsUrl = "https://c.pcs.baidu.com/rest/2.0/pcs/"
dPcsUrl = "https://d.pcs.baidu.com/rest/2.0/pcs/"

extractAppPath (Vars AppConfig {appPath, ..} _) = appPath

extractAccToken (Vars _ (Just (TokenResp {accessToken, ..}))) = accessToken

accessTokenParam = (,) "access_token" . extractAccToken


handleResult :: Show a => Resp a -> IO ()
handleResult (Left q) = print q
handleResult (Right e) = handleErr e


handleErr (Err c m r) = error $ printf "请求出错了：[ %d ] => [ %s ]\n" c m


data DownloadResult = DownloadResult {
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
          <*> liftM int2bool (v .: "isdir")
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
