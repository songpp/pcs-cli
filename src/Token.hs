
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}


module Token
        ( TokenResp(..)
        , Vars (..)
        , AppConfig (..)
        , askNewAccessToken
        , currentTokenConfig
        , tokenCachedFileName
        , newTokenIfNonLocalExisted
        ) where

import           Control.Applicative
import qualified Control.Exception          as E
import           Control.Monad              (liftM)
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Attoparsec
import           Data.Attoparsec.Number
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map                   as Map
import           Network                    (withSocketsDo)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method  (Method, StdMethod (GET, POST),
                                             methodGet, methodPost)
import           Network.HTTP.Types.Status
import           System.Directory           (getHomeDirectory, doesFileExist)
import           System.Environment         (getArgs)
import           System.FilePath
import           System.IO
import           Text.Printf                (printf)
import           Util

--
tokenCachedFileName :: String
tokenCachedFileName = ".pcs-cli.token.json"

appConfigFileName :: String
appConfigFileName =  ".pcs-cli.app.json"

appConfigFile :: IO FilePath
appConfigFile = liftM (</> appConfigFileName) getHomeDirectory
tokenCachedFileFullPath = liftM (</> tokenCachedFileName) getHomeDirectory

newTokenIfNonLocalExisted :: IO Vars
newTokenIfNonLocalExisted = currentTokenConfig >>= process
    where
        process ac@Vars {token = Nothing, ..} = askNewAccessToken
        process ac = return  ac


currentToken :: IO (Maybe TokenResp)
currentToken = liftM parse'' $ tokenCachedFileFullPath >>= readFile
    where
        parse'' :: String -> Maybe TokenResp
        parse'' = decodeStrict . LC.toStrict . LC.pack

currentTokenConfig :: IO Vars
currentTokenConfig = do
    tok <- currentToken
    conf <- loadAppConfig
    return (Vars conf tok)


askNewAccessToken :: IO Vars
askNewAccessToken = do
    conf <- loadAppConfig
    dcResp <- deviceCodeAuth conf
    tok <- requestForAccessToken conf dcResp
    return Vars { appConfig = conf, token = Just tok }
    
--
openApiUrl, openApiVersion, scope, oAuthUrl, deviceAuthUrl, tokenUrl :: String
openApiUrl = "https://openapi.baidu.com"
openApiVersion = "2.0"
scope = "basic netdisk"

oAuthUrl = openApiUrl ++ "/oauth/" ++ openApiVersion
deviceAuthUrl = oAuthUrl ++ "/device/code"
tokenUrl = oAuthUrl ++ "/token"

convertStringPairToBS = map2 (LC.toStrict . LC.pack)
    where map2 f (a,b) = (f a, f b)


requestForAccessToken :: AppConfig -> DeviceCodeResp -> IO TokenResp
requestForAccessToken (AppConfig {appKey, secret, ..}) dc = do
        resp <- doRequest tokenUrl params POST
        let resp' = handleJSONResponse resp :: Either TokenResp Err
        case resp' of
            Left t@(TokenResp { accessToken, refreshToken, ..}) -> do
                file' <- tokenCachedFileFullPath
                writeFile file' (LC.unpack . responseBody $ resp)
                return t
            Right err -> handleErr err
    where
        params = Map.fromList [("grant_type","device_token"),
                ("code", deviceCode dc),
                ("client_id", appKey),
                ("client_secret", secret)]


deviceCodeAuth AppConfig {appKey, ..} = do
        resp <- doRequest deviceAuthUrl params POST
        let r = handleJSONResponse resp :: Either DeviceCodeResp Err
        case handleJSONResponse resp of
            Left dcResp -> do
                printf ">> 去这个地址[ %s ]，\n输入Code: [ %s ]\n"
                    (verifyUrl dcResp) (userCode dcResp)
                printf ">> 或者去这个地址直接扫描二维码授权：%s\n" (qrCodeUrl dcResp)
                printf ">> 完成授权后按输入任意字符后回车继续..\n >> "
                _ <- getLine
                return dcResp
            Right err ->
                error $ printf "请求出错了：[%s] [%s]\n" (errCode err) (description err)

    where
        params = Map.fromList [ ("client_id", appKey)
                              , ("scope", scope)
                              , ("response_type", "device_code")]


handleErr (Err {errCode, description}) =
    error $ printf "请求出错了：[%s] [%s]\n" errCode description



loadAppConfig :: IO AppConfig
loadAppConfig = do
        f <- appConfigFile
        e <- doesFileExist f
        cont <- if not e 
            then error ("需要文件: " ++ f ++ 
                "\n JSON格式：{ \n\t appKey : \"...\", \n\t secretKey: \"...\", \n\t appPath: \"/apps/...\" }") 
            else L.readFile f 
        case eitherDecode cont of
            Left err -> error $ "读取App配置出错：" ++ err
            Right conf -> return conf



data Err = Err {
        errCode     :: String,
        description :: String
    } deriving Show

instance FromJSON Err where
    parseJSON (Object v) = Err 
                <$> v .: "error"
                <*> v .: "error_description"

data DeviceCodeResp = DeviceCodeResp {
        deviceCode     :: String,
        userCode       :: String,
        verifyUrl      :: String,
        qrCodeUrl      :: String,
        dCodeExpiresIn :: Int,
        interval       :: Int
    } deriving Show


instance FromJSON DeviceCodeResp where
    parseJSON (Object v) = DeviceCodeResp 
                <$> v .: "device_code"
                <*> v .: "user_code" 
                <*> v .: "verification_url" 
                <*> v .: "qrcode_url" 
                <*> v .: "expires_in" 
                <*> v .: "interval"


data TokenResp = TokenResp {
        accessToken    :: String,
        tokenExpiresIn :: Int,
        refreshToken   :: String,
        tokenScope     :: String,
        sessionKey     :: String,
        sessionSecret  :: String
    } deriving (Show, Eq)

instance FromJSON TokenResp where
    parseJSON (Object v) = TokenResp <$>
            v .: "access_token" <*>
            v .: "expires_in" <*>
            v .: "refresh_token" <*>
            v .: "scope" <*>
            v .: "session_key" <*>
            v .: "session_secret"


data AppConfig = AppConfig { 
        appKey :: String,
        secret :: String,
        appPath :: FilePath 
      } deriving (Show, Eq)


instance FromJSON AppConfig where
    parseJSON (Object v) = 
        AppConfig <$> v .: "appKey"
                  <*> v .: "secretKey"
                  <*> v .: "appPath"


data Vars = Vars {
    appConfig :: AppConfig,
    token :: Maybe TokenResp
} deriving Show 







