{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pesquisa where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import  qualified Prelude as P


formPesquisa :: Form (Text,Text)
formPesquisa = renderDivs $ (,)
        <$> areq textField  "Nome do livro: " Nothing
        <*> areq hiddenField "" (Just "Tete")

getPesquisaR :: Handler Html
getPesquisaR = do
        (widget,enctype) <- generateFormPost formPesquisa
        (Just user) <- lookupSession "IdUser"
        Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
        defaultLayout $ do 
                setTitle . fromString $ "Pesquisar Livro | Sharebooks - Compartilhando histórias"
                addStylesheet $ StaticR css_bootstrap_css
                toWidget $ $(luciusFile "templates/cadUser.lucius")
                $(whamletFile "templates/menuinterno.hamlet")
                [whamlet|
                        <main>
                                <div .container-fluid>
                                        <div .row>
                                                <div .col-md-4>
                                                        <p>
                                                <div .col-md-4>
                                                        <form action=@{ResultR} method=post enctype=#{enctype}>
                                                                ^{widget}
                                                                <input type="submit" value="Pesquisar">
                |]
                