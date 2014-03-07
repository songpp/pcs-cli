{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Util where

import           Control.Applicative
import qualified Control.Exception                     as E
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Attoparsec
import           Data.Attoparsec.Number
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as LC
import           Data.Conduit
import qualified Data.Conduit.Binary                   as CB
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as TE
import           Network                               (withSocketsDo)
import           Network.Connection                    (TLSSettings (..))
import           Network.HTTP.Base                     (urlEncode)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method             (Method,
                                                        StdMethod (GET, POST),
                                                        methodGet, methodPost)
import           Network.HTTP.Types.Status
import           Prelude                               as P
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf                           (printf)



type Params = Map String String



uploadFile :: String -> Params -> String -> IO (Response L.ByteString)
uploadFile url params f = withSocketsDo $ do
        --printf "uploading file %s" f
        req <- parseUrl url
        let req' = req { method = methodPost }
        request <- formDataBody body req'
        withUncheckedManager $ httpLbs request
    where
        body = fpart : Map.foldlWithKey
                            (\a k v -> pair2PartBs k v : a) [] params

        pair2PartBs k v = partBS (T.pack k) (toUtf8BS v)
        fpart = partFileSource "file" f
        toUtf8BS = TE.encodeUtf8 . T.pack



sendRequest :: (FromJSON a, FromJSON b)
            => String
            -> Map String String
            -> StdMethod
            -> IO (Either a b)
sendRequest url params m = do
        resp' <- doRequest url params m
        debug <- isDebug
        when debug $
            printf " << response: %s ... \n"
                (LC.unpack $ responseBody resp')
        let resp = handleJSONResponse resp'
        return resp


doRequest :: String
          -> Map String String
          -> StdMethod
          -> IO (Response L.ByteString)
doRequest url param m = withSocketsDo $ do
        debug <- isDebug
        when debug $ do
                printf ">> %s: [ %s ] \n" (show m) url
                printf ">>>> param: [ %s ] \n" (joinParams param)
        req <- case m of
            GET -> do
                let url' = appendParamToUrl
                req <- parseUrl url'
                let req' = req { checkStatus = check, method = methodGet }
                return req'
            POST -> do
                req <- parseUrl url
                let req' = req { checkStatus = check, method = methodPost }
                return $ if Map.null param
                    then req'
                    else urlEncodedBody (conv' param) req'
        {-
        printf " >> %s: %s%s \n"
            ( LC.unpack . LC.fromStrict $ method req)
            ( LC.unpack . LC.fromStrict $ host req)
            ( LC.unpack . LC.fromStrict $ queryString req)
        -}
        withUncheckedManager $ httpLbs req
    where
        appendParamToUrl = printf "%s%c%s" url sep (joinParams param)
        sep = if '?' `elem` url then '&' else '?'

        conv' = Map.foldlWithKey (\acc k v -> (toBS' k, toBS' v):acc ) []
        toBS' = LC.toStrict . LC.pack

        check _ _ _ = Nothing



handleJSONResponse :: (FromJSON a, FromJSON b)
                   => Response L.ByteString
                   -> Either a b
handleJSONResponse resp = if responseSuccess resp
        then Right $ decodeRespBody body
        else Left $ decodeRespBody body
    where
        body = responseBody resp


withUncheckedManager = withManagerSettings sts
    where
        sts = mkManagerSettings (TLSSettingsSimple True False False) Nothing


decodeRespBody :: FromJSON b => L.ByteString -> b
decodeRespBody cont = case eitherDecode cont of
    Left err -> error $ "parse response error: \n\t" ++ err
    Right a -> a


responseSuccess :: Response L.ByteString -> Bool
responseSuccess = valid' . statusCode . responseStatus
    where
        valid' c = c == 200



buildUrl :: String -> Map String String -> String
buildUrl url params = printf "%s?%s" url (joinParams params)


joinParams :: Map String String -> String
joinParams = drop 1 . Map.foldlWithKey (\acc k v ->
    printf "%s&%s=%s" acc (urlEncode k) (urlEncode v)) ""


absFilePath :: String -> IO String
absFilePath path = do
        home <- getHomeDirectory
        current <- getCurrentDirectory
        return $ makeAbsFilePath home current path
    where
        makeAbsFilePath home current path = case normalise path of
            ('~' : '/' : ps) -> joinPath [home, ps]
            (l : ps) -> if l /= '/'
                then joinPath [current, path]
                else path


isDebug :: IO Bool
isDebug = liftM e2bool (lookupEnv "DEBUG")
    where
        e2bool (Just "1") = True
        e2bool _          = False

