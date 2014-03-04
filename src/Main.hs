
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Main
        ( main
        , quotaInfo
        ) where

import           Control.Applicative
import           Control.Monad          (liftM)
import           Data.Aeson
import           Data.Map               ()
import qualified Data.Map               as Map
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf            (printf)
import           Token
import           Util

import           Api

pcsUrl, cPcsUrl, dPcsUrl :: String
pcsUrl = "https://pcs.baidu.com/rest/2.0/pcs/"
cPcsUrl = "https://c.pcs.baidu.com/rest/2.0/pcs/"
dPcsUrl = "https://d.pcs.baidu.com/rest/2.0/pcs/"


data Args = Info
          | Auth { appKey :: FilePath }
          | Quota
          | Search { basePath  :: FilePath
                   , keywrod   :: String
                   , recursive :: Bool
                   }
          | Upload { file       :: FilePath
                   , targetPath :: FilePath
                   }
          | Download { path :: FilePath }
    deriving (Eq, Show, Typeable, Data)


auth = Auth def
quota = Quota &= help "查询空间使用情况"



main :: IO ()
main = do
    cmd <- cmdArgs (modes  [auth, quota])
    case cmd of
        Quota -> quotaInfo
        otherwise -> return ()
    args <- getArgs
    print args
    home <- getHomeDirectory
    current <- getCurrentDirectory
    let path = makeAbsFilePath home current (head args)
    putStrLn "Welcome to Command Line PCS"
    putStrLn $ "get path: " ++ path


makeAbsFilePath home current path = case normalise path of
    ('~' : '/' : ps) -> joinPath [home, ps]
    (l : ps) -> if l /= '/'
        then joinPath [current, path]
        else path
