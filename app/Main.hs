{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE NoImplicitPrelude #-}
module Main where

import Praglude

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Web.Resourcerer.Resource ( Resource (..)
                                )
import Web.Resourcerer.Serve (resourceToApplication)

makeRootResource :: IO Resource
makeRootResource = undefined

main :: IO ()
main = do
    resource <- makeRootResource
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (resourceToApplication resource)
