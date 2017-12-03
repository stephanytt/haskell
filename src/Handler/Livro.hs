{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Livro where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql


formLivro :: Form Livro
formLivro = renderDivs $ Livro
        <$> areq textField  "Codigo: " Nothing
        <*> areq textField  "Nome: " Nothing
        <*> areq textField  "Autor: " Nothing
        <*> areq intField  "ISBN: " Nothing
        <*> areq textField  "CapaLivro: " Nothing
        <*> areq textField  "BannerLivro: " Nothing
        <*> areq textField  "Descrição: " Nothing
        <*> areq intField  "Classe: " Nothing
        <*> areq textField  "Categoria: " Nothing

getCadLivroR :: Handler Html
getCadLivroR = do
    (widget,enctype) <- generateFormPost formLivro 
    defaultLayout $ do
        [whamlet|  
            <form action=@{CadLivroR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit">
        |]

postCadLivroR :: Handler Html
postCadLivroR = do
    --retornando o resultado
    ((result,_),_) <- runFormPost formLivro
    case result of 
        FormSuccess livro -> do
            idLivro<- runDB $ insert livro
            redirect ShareR
        _ -> redirect ShareR
    