{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hinatra where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.ByteString as B
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

newtype Handler a
  = Handler { unHandler :: StateT HandlerState IO a }
  deriving (Monad, MonadIO, MonadPlus, MonadState HandlerState)

data HandlerState
  = HandlerState
    { stateReq :: Request
    }

toApp :: Handler String -> Application
toApp h = \req -> do
  result <- liftIO $ evalStateT (unHandler h) (HandlerState req)
  return $ ResponseBuilder (Status 200 "OK") [] (fromString result)

hinatra :: Handler String -> IO ()
hinatra h = do
  run 3000 $ toApp (h `mplus` return "not found")

get :: String -> Handler a -> Handler a
get route h = do
  path <- getPath
  when (not $ match route path) $ fail "hoge"
  h

getPath :: Handler String
getPath = do
  req <- Control.Monad.State.get
  return $ Prelude.concatMap ("/"++) $ Prelude.map (Prelude.map (toEnum.fromEnum). B.unpack . encodeUtf8) $ pathInfo $ stateReq req

match :: String -> String -> Bool
match = (==)
