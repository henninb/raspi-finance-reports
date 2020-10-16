{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ServerApi(restServer) where

import           HelloApi
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import Data.Text.Internal

server :: Handler Data.Text.Internal.Text :<|> (Data.Text.Internal.Text -> Int -> Handler User)
server = hello :<|> user
    where
        hello = return "Hello world"
        user n a = return $ User n a

app :: Application
app = serve helloApi server

restServer :: IO ()
restServer = do
    run 8080 app
