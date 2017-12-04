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


formPesquisa :: Form (Text,Text)
formPesquisa = renderDivs $ (,)
        <$> areq textField  "Nome do livro: " Nothing
        <*> areq hiddenField "" (Just "Tete")

getPesquisaR :: Handler Html
getPesquisaR = do
        (widget,enctype) <- generateFormPost formPesquisa
        defaultLayout $ do 
                [whamlet|
                        <form action=@{ResultR} method=post enctype=#{enctype}>
                                ^{widget}
                                <input type="submit">
                |]
                