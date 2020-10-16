{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Example where


import Control.Lens (makeLenses, (^.), (%~), (.~))
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson (FromJSON, ToJSON)
import Data.List ((\\), nub)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles



-- * config

dbPath :: FilePath
dbPath = "./.state"

htmlPath :: FilePath
htmlPath = "./public_html"


-- * data types

data DB = DB { _colors :: [String], _user :: String }
  deriving (Eq, Show, Typeable, Generic)

type St = AcidState DB

instance FromJSON DB
instance ToJSON DB

makeLenses ''DB
$(deriveSafeCopy 0 'base ''DB)

openDB :: IO St
openDB = openLocalStateFrom dbPath (DB ["red", "green", "smelly"] "")

getUser :: Query DB String
getUser = asks (^. user)

changeUser :: String -> Update DB ()
changeUser u = modify $ user .~ u

getColors :: Query DB [String]
getColors = asks (^. colors)

addColor :: String -> Update DB [String]
addColor c = do
  modify $ colors %~ (nub . (c:))
  gets (^. colors)

dropColor :: String -> Update DB [String]
dropColor c = do
  modify $ colors %~ (\\ [c])
  gets (^. colors)

$(makeAcidic ''DB ['getUser, 'changeUser, 'getColors, 'addColor, 'dropColor])


-- * rest

type Rest =
       "user"   :> RestUser
  :<|> "colors" :> RestColors
  :<|> Raw

type RestUser =
       Get '[JSON] String
  :<|> Capture "uname" String :> Put '[JSON] ()

type RestColors =
       Get '[JSON] [String]
  :<|> Capture "addcolor" String :> Post '[JSON] [String]
  :<|> Capture "dropcolor" String :> Delete '[JSON] [String]

rest :: St -> Server Rest
rest state = restUser state :<|> restColor state :<|> serveDirectory htmlPath

restUser :: St -> Server RestUser
restUser state =
       query'  state GetUser
  :<|> update' state . ChangeUser

restColor :: St -> Server RestColors
restColor state =
       query'  state GetColors
  :<|> update' state . AddColor
  :<|> update' state . DropColor

myMain :: IO ()
myMain = openDB >>= runRest

runRest :: St -> IO ()
runRest = runSettings settings . serveRest
  where
    settings = setPort 7000 . setHost "127.0.0.1" $ defaultSettings

serveRest :: St -> Application
serveRest = serve (Proxy :: Proxy Rest) . rest