{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import  Text.Lucius
import Database.Persist.Postgresql

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

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) 
        <$> areq emailField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing
        
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        setTitle . fromString $ "Entrar | Sharebooks - Compartilhando histórias"
        addStylesheet $ StaticR css_bootstrap_css
        toWidget $ $(luciusFile "templates/cadUser.lucius")
        [whamlet|
            ^{menu}
            <main>
                <div .container-fluid>
                    <div .row>
                        <div .col-md-4>
                            <p>
                                
                        <div .col-md-4>
                            <form action=@{LoginR} method=post enctype=#{enctype}>
                                ^{widget}
                                <input type="submit" value="Entrar">

            <footer>
                <nav .navbar .navbar-inverse .navbar-fixed-bottom>
                    <div .container-fluid>
                        <p .navbar-text .navbar-right>Sharebooks 2017. Todos os direitos reservados.&nbsp;</p>
        |]

-- BD (Maybe (Entity Usuario))
autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar email senha = runDB $ selectFirst [UsuarioEmail ==. email
                                             ,UsuarioSenha ==. senha] []

postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess (email,senha) -> do 
            talvezUsuario <- autenticar email senha 
            case talvezUsuario of 
                Nothing -> do 
                    setMessage [shamlet| 
                        <h1> 
                            Usuario nao cadastrado/Senha inválida 
                    |]
                    redirect LoginR
                Just (Entity uid (Usuario e _ n c d s)) -> do 
                    setSession "_USR" (pack (show $ Usuario e "" n c d s))
                    setSession "IdUser" (pack (show $ uid ))
                    redirect (PerfilUserR uid)
        _ -> redirect ShareR

getLogoutR :: Handler Html
getLogoutR = do 
    deleteSession "_USR"
    redirect ShareR