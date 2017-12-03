{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Serie where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

formSerie :: Form Serie 
formSerie = renderDivs $ Serie 
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Genero: " Nothing
        <*> areq intField  "Temporadas: " Nothing
        <*> areq intField  "Ano: " Nothing

getCadSerieR :: Handler Html
getCadSerieR = do 
    (widget,enctype) <- generateFormPost formSerie
    defaultLayout $ do 
        [whamlet|
            <form action=@{CadSerieR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]

postCadSerieR :: Handler Html
postCadSerieR = do 
    ((result,_),_) <- runFormPost formSerie
    case result of
        FormSuccess serie -> do 
            serieid <- runDB $ insert serie 
            redirect (PerfilSerieR serieid)

getPerfilSerieR :: SerieId -> Handler Html
getPerfilSerieR serieid = do 
    serie <- runDB $ get404 serieid
    defaultLayout $ do 
        [whamlet|
            <h1> ]
                Serie: #{serieNome serie}
            <h2>
                Genero: #{serieGenero serie}
            <h2>
                Temporadas: #{serieQtTemp serie}
            <h2>
                Ano: #{serieAno serie}
        |]

-- SELECT * FROM SERIE
getListarSerieR :: Handler Html
getListarSerieR = do 
    series <- runDB $ selectList [] [Asc SerieNome]
    defaultLayout $ do 
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Nome
                        
                        <th> 
                            Genero
                        
                        <th>
                            Temporadas
                        
                        <th> 
                            Ano
                        
                        <th>
                            
                
                <tbody>
                    $forall (Entity serieid serie) <- series
                        <tr>
                            <td>
                                <a href=@{PerfilSerieR serieid}> 
                                    #{serieNome serie}
                            
                            <td>
                                #{serieGenero serie}
                            
                            <td>
                                #{serieQtTemp serie}
                            
                            <td>
                                #{serieAno serie}
                            
                            <td>
                                <form action=@{ApagarSerieR serieid} method=post>
                                    <input type="submit" value="Apagar">
        |]

postApagarSerieR :: SerieId -> Handler Html
postApagarSerieR serieid = do 
    _ <- runDB $ get404 serieid
    runDB $ delete serieid
    redirect ListarSerieR
    