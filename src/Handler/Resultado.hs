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
            resultlivro <- selectLivros ("%"<>livropesquisado<>"%")
            --faz o lambda pra retornar as infos do livro(id, livro e suas infos)
            livros <- sequence $ map (\livro -> return (entityKey livro, entityVal livro)) resultlivro
            defaultLayout $ do
                setTitle . fromString $ "Resultado | Sharebooks - Compartilhando histórias"
                addStylesheet $ StaticR css_bootstrap_css
                toWidget $ $(luciusFile "templates/cadUser.lucius")
                $(whamletFile "templates/menuinterno.hamlet")
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
selectLivros :: Text -> Handler [Entity Livro]
selectLivros t = runDB $ rawSql s [toPersistValue t]
    where s = "SELECT ?? FROM livro WHERE nome LIKE ? "