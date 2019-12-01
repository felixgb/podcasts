{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.IO.Class
import Web.Scotty

import Model

route :: IO ()
route = scotty 3000 $ do

  get "/all" $ do
    eps <- liftIO listEpNums
    json eps

  get "/start/:type" $ do
    ty <- param "type"
    pod <- liftIO $ podcast ty
    json pod

  get "/inc/:type" $ do
    ty <- param "type"
    liftIO $ incEpNum ty
