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
import  qualified Prelude as P

postResultR :: Handler Html
postResultR = do 
    ((result,_),_) <- runFormPost formPesquisa
    (Just user) <- lookupSession "IdUser"
    Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
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
                        <div .container-fluid>
                            <div .row>
                                <div .col-md-4>
                                    <p>
                                    
                                <div .col-md-4>
                                    <p>
                                        Livro : #{livroNome livro} 
                                    <p>
                                        Autor: #{livroAutor livro}
                                    <p> 
                                        Categoria : #{livroCategoria livro}
                                    <p>
                                        
                    <p>
                        Não encontrou o livro? <a href=@{CadLivroR}>
                            Cadastre-o agora
                    <footer>
                        <nav .navbar .navbar-inverse .navbar-static-bottom>
                            <div .container-fluid>
                                <p .navbar-text .navbar-right>Sharebooks 2017. Todos os direitos reservados.&nbsp;</p>

                |]
        _ -> redirect PesquisaR        
selectLivros :: Text -> Handler [Entity Livro]
selectLivros t = runDB $ rawSql s [toPersistValue t]
    where s = "SELECT ?? FROM livro WHERE nome LIKE ? "