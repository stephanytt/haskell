{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql
import qualified Prelude as P

formUser :: Form Usuario
formUser = renderDivs $ Usuario
        <$> areq emailField  "Email: " Nothing
        <*> areq passwordField  "Senha: " Nothing
        <*> areq textField  "Nome: " Nothing
        <*> areq textField  "CPF: " Nothing
        <*> areq textField  "Cidade: " Nothing
        <*> areq textField  "Estado: " Nothing

menu :: Widget
menu = [whamlet|
    <nav .navbar .navbar-default .navbar-fixed-top>
      <div .container-fluid>
        <div .navbar-header>
          <button type="button" .navbar-toggle .collapsed data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
            <span .sr-only>Toggle navigation</span>
            <span .icon-bar></span>
            <span .icon-bar></span>
            <span .icon-bar></span>
          <a .navbar-brand href=@{ShareR}>
            <img src=@{StaticR img_sharebooks_png} alt="Sharebooks" width="20%">

        <div .collapse .navbar-collapse id="bs-example-navbar-collapse-1">
            <ul .nav .navbar-nav .navbar-left>
                <li>
                    <a href=@{SobreR}>
                        Sobre Nós
            <ul .nav .navbar-nav .navbar-right>
                <li>
                    <a href=@{CadUserR}>
                        Cadastrar-se
                <li>
                    <a href=@{LoginR}>
                        Login
|]

menuInterno :: Widget
menuInterno = [whamlet|
    <nav .navbar .navbar-default .navbar-fixed-top>
      <div .container-fluid>
        <div .navbar-header>
          <button type="button" .navbar-toggle .collapsed data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
            <span .sr-only>Toggle navigation</span>
            <span .icon-bar></span>
            <span .icon-bar></span>
            <span .icon-bar></span>
          <a .navbar-brand href=@{ShareR}>
            <img src=@{StaticR img_sharebooks_png} alt="Sharebooks" width="20%">

        <div .collapse .navbar-collapse id="bs-example-navbar-collapse-1">
            <ul .nav .navbar-nav .navbar-right>
                <li>
                    <a href=@{LogoutR}>
                        Sair
|]

getCadUserR :: Handler Html
getCadUserR = do 
    (widget,enctype) <- generateFormPost formUser
    defaultLayout $ do 
        setTitle . fromString $ "Cadastre-se | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/cadUser.lucius")
        $(whamletFile "templates/cadUser.hamlet")

postCadUserR :: Handler Html
postCadUserR = do 
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess usuario -> do 
            usuarioid <- runDB $ insert usuario
            redirect (PerfilUserR usuarioid)
        _ -> redirect ShareR
        
tabelaSerie :: UsuarioId -> Widget
tabelaSerie usuarioid = do
    -- Handler ([Entity uid usuarioSerie])
    usuarioSerie <- handlerToWidget $ runDB $ selectList [UsuarioSerieUsuarioId ==. usuarioid] 
                                                         [Asc UsuarioSerieVistoEm]
    series <- handlerToWidget $ sequence $ fmap (\(Entity _ usuarioSerie) -> runDB $ get404 $ usuarioSerieSerieId usuarioSerie) usuarioSerie
    tudoJunto <- return $ zip usuarioSerie series
    [whamlet|
        <table>
            <thead>
                <tr>
                    <th>
                        Nome
                    <th>
                        Visto em 
            <tbody>
                $forall (Entity _ usuarioSerie, serie) <- tudoJunto
                    <tr>
                        <td>
                            #{serieNome serie}
                        <td>
                            #{show $ usuarioSerieVistoEm usuarioSerie}
                
    |]

getPerfilUserR :: UsuarioId -> Handler Html
getPerfilUserR usuarioid = do
    usuario <- runDB $ get404 usuarioid
    defaultLayout $ do
        setTitle . fromString $ "Perfil | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/cadUser.lucius")
        $(whamletFile "templates/perfil.hamlet")

--Select * from Usuario
getListarUserR :: Handler Html
getListarUserR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Nome
                        <th>
                            Email
                        <th>
                            Cidade
                        <th>
                            Estado
                        <th>
                <tbody>
                    $forall (Entity usuarioid usuario) <- usuarios
                        <tr>
                            <td>
                                #{usuarioNome usuario}
                            <td>
                                #{usuarioEmail usuario}
                            <td>
                                #{usuarioCidade usuario}
                            <td>
                                #{usuarioEstado usuario}
                            <td>
                                <form action=@{ApagarUserR usuarioid} method=post>
                                    <input type="submit" value="Apagar">
                                    
        |]

postApagarUserR :: UsuarioId -> Handler Html
postApagarUserR usuarioid = do
    _ <- runDB $ get404 usuarioid
    runDB $ delete usuarioid
    redirect ListarUserR
