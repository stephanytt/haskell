{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Solicitacao where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import  Data.Maybe
import  qualified Prelude as P

getSolicitR :: LivroId -> Handler Html
getSolicitR livrosolicit = do
    (Just user) <- lookupSession "IdUser"
    Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
    e <- runDB $ selectFirst [EstanteLivro ==. livrosolicit ] []
    date <- fmap utctDay (lift $ liftIO getCurrentTime)
    runDB $ insert (Solicitacao (estanteUsuario $ entityVal $ fromJust e) userId livrosolicit date)
    redirect PesquisaR
