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
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
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