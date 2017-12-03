{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UsuarioSerie where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


formAssistir :: Form UsuarioSerie
formAssistir = renderDivs $ UsuarioSerie
    <$> areq (selectField seriesLista) "Series: " Nothing
    <*> areq (selectField usersLista) "Usuario: " Nothing
    <*> areq dayField "Visto em: " Nothing

seriesLista = do
       entidades <- runDB $ selectList [] [Asc SerieNome] 
       optionsPairs $ fmap (\ent -> (serieNome $ entityVal ent, entityKey ent)) entidades

usersLista = do
       entidades <- runDB $ selectList [] [Asc UsuarioNome] 
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades

getAssistirR :: Handler Html
getAssistirR = do
    (widget,enctype) <- generateFormPost formAssistir
    defaultLayout $ do 
        [whamlet|
            <form action=@{AssistirR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]

postAssistirR :: Handler Html
postAssistirR = do
    ((result,_),_) <- runFormPost formAssistir
    case result of
        FormSuccess assistir -> do 
            _ <- runDB $ insert assistir 
            setMessage [shamlet|
                <div> Registro inserido!
            |]
            redirect AssistirR
