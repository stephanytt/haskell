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
                        <main .cont>
                                <div .container-fluid>
                                        <div .row>
                                                <div .col-md-3>
                                                        <p>
                                                <div .col-md-6 .form-pesquisa>
                                                        <h1>
                                                                Pesquisa
                                                        <p>
                                                                Adicione um livro à sua estante
                                                        <br>
                                                        <form action=@{ResultR} method=post enctype=#{enctype}>
                                                                ^{widget}
                                                                <input type="submit" value="Pesquisar">
                        <footer>
                                <nav .navbar .navbar-inverse .navbar-fixed-bottom>
                                        <div .container-fluid>
                                                <p .navbar-text .navbar-right>Sharebooks 2017. Todos os direitos reservados.&nbsp;</p>
                |]
                