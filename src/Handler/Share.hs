{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Share where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import qualified Prelude as P

getShareR :: Handler Html
getShareR = do
    sess <- lookupSession "_USR"
    --talvezUsuario <- return $ fmap (P.read . unpack) sess :: Handler (Maybe Usuario)
    defaultLayout $ do
        setTitle . fromString $ "Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"
        toWidget $ $(luciusFile "templates/share.lucius")
        $(whamletFile "templates/menu.hamlet")
        $(whamletFile "templates/share.hamlet")
        
getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        setTitle . fromString $ "Sobre Nós | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/sobre.lucius")
        $(whamletFile "templates/menu.hamlet")
        $(whamletFile "templates/sobre.hamlet")
