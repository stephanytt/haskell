{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Solicitacao where

import  Import
import  Text.Lucius
import  Text.Julius
import  Database.Persist.Postgresql

postSolicitR :: LivroId -> Handler Html
postSolicitR livrosolicit = undefined
