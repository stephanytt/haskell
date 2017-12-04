{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Resultado where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import  Handler.Pesquisa

postResultR :: Handler Html
postResultR = do 
    ((result,_),_) <- runFormPost formPesquisa
    case result of  
        FormSuccess (livropesquisado,_) -> do
            resultlivro <- runDB $ rawSql
                ("Select ??\
                \FROM livro\
                \WHERE livro.nome = '" <> (pack $ show livropesquisado) <>"'") [] :: Handler [(Entity Livro)]
            --faz o lambda pra retornar as infos do livro(id, livro e suas infos)
            livros <- sequence $ map (\livro -> return (entityKey livro, entityVal livro)) resultlivro
            defaultLayout $ do
                [whamlet|
                    $forall (id, livro) <- livros   
                        <div>
                            <p>
                                Livro : #{livroNome livro} 
                            <p>
                                Autor: #{livroAutor livro}
                            <p> 
                                Categoria : #{livroCategoria livro}
                |]
        _ -> redirect PesquisaR        