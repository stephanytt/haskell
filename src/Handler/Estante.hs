{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Estante where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import  Data.Maybe
import  qualified Prelude as P

getVerestanteR :: LivroId -> Handler Html
getVerestanteR livroscad = do
    resultlivro <- runDB $ get404 livroscad
    (Just user) <- lookupSession "IdUser"
    Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
    defaultLayout $ do
        setTitle . fromString $ "Livro | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/cadUser.lucius")
        $(whamletFile "templates/menuinterno.hamlet")
        [whamlet|
            <div .container-fluid>
                <div .row>
                    <div .col-md-4>
                        <p>
                    <div .col-md-4>
                        <h1>
                            Livro: #{livroNome resultlivro}
                        <p> 
                            Autor: #{livroAutor resultlivro}
                        <p> 
                            ISBN: #{livroIsbn resultlivro}
                        <p> 
                            Capa Livro: <img src=#{livroCapalivro resultlivro} style="max-width:100$">
                        <p> 
                            Banner Livro: <img src=#{livroBannerlivro resultlivro} style="max-width:100$">
                        <p> 
                            Descrição: #{livroDescricao resultlivro}
                        <p>
                            Categoria: #{livroCategoria resultlivro}
        |]