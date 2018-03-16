{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
 
module Consumo where
import Yesod
import Foundation
import Selects
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text hiding (map)
import Data.Time.Calendar
import GHC.Generics
import Text.Julius
import Data.Time

import Database.Persist.Postgresql
import Network.HTTP.Types.Status


data ConsumoTemp = ConsumoTemp {dtConsumo :: Day,
                                qtConsumo :: Double,
                                precoId :: PrecoId,
                                ambienteId :: AmbienteId} deriving Generic

instance ToJSON ConsumoTemp
instance FromJSON ConsumoTemp


criaDouble :: Int -> Int -> Double
criaDouble a b = fromIntegral a + (fromIntegral b)/100

getArduinoConsumoR:: Int -> Int -> PrecoId -> AmbienteId -> Handler TypedContent
getArduinoConsumoR  left right pid aid = do 
        now <- liftIO getCurrentTime
        let (ano, mes, dia) = toGregorian $ utctDay now
        temCons <- runDB $ selectFirst [ConsumoDtConsumo ==. fromGregorian ano mes dia, ConsumoAmbienteId ==. aid] []
        case temCons of
            Nothing -> do
                cid <- runDB $ insert $ Consumo (fromGregorian ano mes dia) (criaDouble left right) pid aid
                sendStatusJSON created201 (object ["resp" .= (fromSqlKey cid)])
            Just _ -> do
                cid <-runDB $ updateWhere [ConsumoDtConsumo==. fromGregorian ano mes dia] [ConsumoQtConsumo+=. criaDouble left right]
                sendResponseStatus status200 ("UPDATED" :: Text)


postConsumoR :: Handler TypedContent
postConsumoR = do
    consumo <- requireJsonBody :: Handler ConsumoTemp
    temCons <- runDB $ selectFirst [ConsumoDtConsumo ==. dtConsumo consumo, ConsumoAmbienteId ==. ambienteId consumo] []
    case temCons of
        Nothing -> do
            cid <- runDB $ insert $ fazConsumo (dtConsumo consumo) (qtConsumo consumo) (precoId consumo) (ambienteId consumo)
            sendStatusJSON created201 (object ["resp" .= (fromSqlKey cid)])
        Just _ -> do
            cid <-runDB $ updateWhere [ConsumoDtConsumo==. dtConsumo consumo] [ConsumoQtConsumo+=. qtConsumo consumo]
            sendResponseStatus status200 ("UPDATED" :: Text)
            
fazConsumo :: Day -> Double -> PrecoId -> AmbienteId -> Consumo
fazConsumo dt qt pr amb = Consumo dt qt pr amb

getConsumoCasaR :: Handler Html
getConsumoCasaR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        jan <- selectConsumoCasaMesQual 1
        fev <- selectConsumoCasaMesQual 2
        mar <- selectConsumoCasaMesQual 3
        abr <- selectConsumoCasaMesQual 4
        maio <- selectConsumoCasaMesQual 5
        jun <- selectConsumoCasaMesQual 6 
        jul <- selectConsumoCasaMesQual 7
        ago <- selectConsumoCasaMesQual 8
        set <- selectConsumoCasaMesQual 9
        out <- selectConsumoCasaMesQual 10
        nov <- selectConsumoCasaMesQual 11
        dez <- selectConsumoCasaMesQual 12
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/chart.julius")
           widgetVisuConsumo casa preco consumoCasa

postConsumoAmbienteR :: AmbienteId -> Handler Html
postConsumoAmbienteR aid = do
    listaCons <- runDB $ selectList [ConsumoAmbienteId ==. aid] []
    listaCid <- return $ map (entityKey) listaCons
    del <- sequence $ map (\cid -> runDB $ delete cid) listaCid
    redirect ListarAmbienteR
    
getConsumoCasaMensalR :: Handler Html
getConsumoCasaMensalR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaMes
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/consumo/chartmensal.julius")
           widgetVisuConsumo casa preco consumoCasa
           
getConsumoCasaDiarioR :: Handler Html
getConsumoCasaDiarioR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaDia
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/consumo/chartdiario.julius")
           widgetVisuConsumo casa preco consumoCasa

getConsumoCasaSemanalR :: Handler Html
getConsumoCasaSemanalR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaSemana
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/consumo/chartsemanal.julius")
           widgetVisuConsumo casa preco consumoCasa
           
           
getConsumoCasaPickR :: Integer -> Int -> Int -> Integer -> Int -> Int -> Handler Html
getConsumoCasaPickR anoI mesI diaI anoF mesF diaF = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaPeriodo anoI mesI diaI anoF mesF diaF
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/consumo/chartpick.julius")
           widgetVisuConsumo casa preco consumoCasa
           
getRelatorioR :: Handler Html
getRelatorioR =  do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/relatorio/tableanual.julius")
           widgetVisuRelatorio casa preco consumoCasa
           
           
getRelatorioSemanalR :: Handler Html
getRelatorioSemanalR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaSemana
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/relatorio/tablesemanal.julius")
           widgetVisuRelatorioS casa preco consumoCasa consumo


getRelatorioDiarioR ::  Handler Html
getRelatorioDiarioR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaDia
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/relatorio/tablediario.julius")
           widgetVisuRelatorioD casa preco consumoCasa consumo

getRelatorioMensalR ::  Handler Html
getRelatorioMensalR = do
        consumoCasa <- selectConsumoCasa -- isso aqui é Double
        casaId <- selectCasaId -- Casa ID
        casa     <- runDB $ get404 casaId
        preco <- selectPreco -- Double
        consumo <- selectConsumoCasaMes
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
           toWidget $(juliusFile "templates/relatorio/tablemensal.julius")
           widgetVisuRelatorioM casa preco consumoCasa consumo