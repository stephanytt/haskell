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
import  Data.Maybe
import  qualified Prelude as P


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
    (Just user) <- lookupSession "IdUser"
    Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
    defaultLayout $ do
        setTitle . fromString $ "Cadastro de Livro | Sharebooks - Compartilhando histórias"
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
                            <form action=@{CadLivroR} method=post enctype=#{enctype}>
                                ^{widget}
                                <input type="submit">
            <footer>
                <nav .navbar .navbar-inverse .navbar-static-bottom>
                    <div .container-fluid>
                        <p .navbar-text .navbar-right>Sharebooks 2017. Todos os direitos reservados.&nbsp;</p>

        |]

postCadLivroR :: Handler Html
postCadLivroR = do
    --retornando o resultado
    ((result,_),_) <- runFormPost formLivro
    case result of 
        FormSuccess livro -> do
            idLivro<- runDB $ insert livro
            (Just user) <- lookupSession "IdUser"
            Just (Entity userId _) <- runDB $ selectFirst [UsuarioId ==. ( P.read . unpack $ user) ] []
            eid <- runDB $ insert (Estante userId idLivro "Bom" 0) :: Handler EstanteId
            redirect (VerLivroR idLivro)
        _ -> redirect ShareR

getVerLivroR :: LivroId -> Handler Html
getVerLivroR livroid = do -- da o erro / se der certo ele pesquisa na table livro
    resultlivro <- runDB $ get404 livroid
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
                        <a href=@{SolicitR livroid} >    
                            Doação
          
        |]

--Route:
-- /livro/buscaLivro       BuscaLivroR     GET

--getBuscaLivroR :: Handler Html
--getBuscaLivroR = do
  --  (widget, enctype) <- generateFormPost formLivroNome
   -- defaultLayout $ do
       -- [whamlet|
        --    <form action=@{ShareR} method=post enctype=#{enctype}>
          --      ^{widget}
            --    <input type="submit" value="Buscar">
       -- |]
--formLivroNome :: Form LivroNome 
--formLivroNome = renderDivs $ LivroNome
  --  <$> areq textField  "Nome: " Nothing
  
getTuaRotaR = undefined