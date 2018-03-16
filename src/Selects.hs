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
 
module Selects where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text hiding (replace, map)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock

import Database.Persist.Postgresql
import Network.HTTP.Types.Status

selectCasaId :: Handler CasaId
selectCasaId = do
        username <- lookupSession "_USER"
        temUsu <- runDB $ selectFirst [UsuarioNmUsuario ==. fromJust(username)] []
        casa <- return $ fromJust $ fmap (usuarioCasaId . entityVal) $ temUsu
        return $ casa --RETORNA UM  KEY CASA
        
selectPreco :: Handler Double
selectPreco = do
        price <- runDB $ selectFirst [PrecoId ==. toSqlKey(0)] []
        valPrice <- return $ fromJust $ fmap (precoQtPreco . entityVal) $ price
        return $ valPrice --RETORNA O QT PRECO


{-***** SELECT CONSUMO AMBIENTE *****-}
{-***** SELECT CONSUMO AMBIENTE *****-}
{-***** SELECT CONSUMO AMBIENTE *****-}
{-***** SELECT CONSUMO AMBIENTE *****-}
{-***** SELECT CONSUMO AMBIENTE *****-}
        
selectConsumoAmbiente :: AmbienteId -> Handler [Double]
selectConsumoAmbiente aid = do
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid] []
    consumoAmbiente <- return $ map (consumoQtConsumo . entityVal) consumo
    return $ consumoAmbiente --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE
    
selectConsumoAmbienteDia :: AmbienteId -> Handler Double
selectConsumoAmbienteDia aid = do
    now <- liftIO getCurrentTime
    let (ano, mes, dia) = toGregorian $ utctDay now
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid, ConsumoDtConsumo ==. fromGregorian ano mes dia] []
    consumoAmbiente <- return $ map (consumoQtConsumo . entityVal) consumo
    let somaAmb = sum consumoAmbiente
    return $ somaAmb --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE

selectConsumoAmbienteSemana :: AmbienteId -> Handler [(Double,Day)]
selectConsumoAmbienteSemana aid = do
    now <- liftIO getCurrentTime
    let (ano, mes, dia) = toGregorian $ utctDay now
    let dataDiff = fromGregorian ano mes dia
    let dataSum = addDays (-7) dataDiff
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid, ConsumoDtConsumo >=. dataSum, ConsumoDtConsumo <=. dataDiff] []
    consumoAmbiente <- return $ map (\ent -> (consumoQtConsumo $ entityVal ent, consumoDtConsumo $ entityVal ent)) consumo
    return $ consumoAmbiente --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE

selectConsumoAmbienteMes :: AmbienteId -> Handler [(Double,Day)]
selectConsumoAmbienteMes aid = do
    now <- liftIO getCurrentTime
    let (ano, mes, _) = toGregorian $ utctDay now
    let diaF = gregorianMonthLength ano mes
    let dataI = fromGregorian ano mes 1
    let dataF = fromGregorian ano mes diaF
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []
    consumoAmbiente <- return $ map (\ent -> (consumoQtConsumo $ entityVal ent, consumoDtConsumo $ entityVal ent)) consumo
    return $ consumoAmbiente --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE
    
selectConsumoAmbienteMesQual :: AmbienteId -> Int -> Handler Double
selectConsumoAmbienteMesQual aid mes = do
    now <- liftIO getCurrentTime
    let (ano, _, _) = toGregorian $ utctDay now
    let diaF = gregorianMonthLength ano mes
    let dataI = fromGregorian ano mes 1
    let dataF = fromGregorian ano mes diaF
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []
    consumoAmbiente <- return $ map (consumoQtConsumo . entityVal) consumo
    let somaAmb = sum consumoAmbiente
    return $ somaAmb --RETORNA UMA LISTA DE CONSUMOS DO AMBIENTE

selectConsumoAmbienteAno :: AmbienteId -> Handler [(Double,Int)]
selectConsumoAmbienteAno aid = do
    now <- liftIO getCurrentTime
    let (ano, _, _) = toGregorian $ utctDay now
    let dataI = fromGregorian ano 1 1
    let dataF = fromGregorian ano 12 31
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid,ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []
    consumoAmbiente <- return $ map (\ent -> (consumoQtConsumo $ entityVal ent, getMonth(getDate(consumoDtConsumo $ entityVal ent)))) consumo
    return $ consumoAmbiente

selectConsumoAmbientePeriodo :: AmbienteId -> Integer -> Int -> Int -> Integer -> Int -> Int -> Handler Double
selectConsumoAmbientePeriodo aid anoI mesI diaI anoF mesF diaF = do
    let dataI = fromGregorian anoI mesI diaI
    let dataF = fromGregorian anoF mesF diaF
    consumo <- runDB $ selectList [ConsumoAmbienteId ==. aid,ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []
    consumoAmbiente <- return $ map (consumoQtConsumo . entityVal) consumo
    let somaAmb = sum consumoAmbiente
    return $ somaAmb

{-***** SELECT CONSUMO CASA *****-}
{-***** SELECT CONSUMO CASA *****-}
{-***** SELECT CONSUMO CASA *****-}
{-***** SELECT CONSUMO CASA *****-}
{-***** SELECT CONSUMO CASA *****-}

selectConsumoCasa :: Handler Double
selectConsumoCasa = do
    cid <- selectCasaId
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val] []) ambId
    consumoCasa <- return $ pegaConsumo consumoAmbientes
    let somaCons = sum consumoCasa
    return $ somaCons --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)
    
pegaConsumo :: [[Entity Consumo]] -> [Double]
pegaConsumo [] = []
pegaConsumo (x:xs) = map (consumoQtConsumo . entityVal) x ++ pegaConsumo xs
--(\ent -> (consumoQtConsumo $ entityVal ent, consumoDtConsumo $ entityVal ent))
--map (consumoQtConsumo . entityVal) x ++ pegaConsumo xs

selectConsumoCasaDia :: Handler Double
selectConsumoCasaDia = do
    cid <- selectCasaId
    now <- liftIO getCurrentTime
    let (ano, mes, dia) = toGregorian $ utctDay now
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo ==. fromGregorian ano mes dia] []) ambId
    consumoCasa <- return $ pegaConsumo consumoAmbientes
    let somaCons = sum consumoCasa
    return $ somaCons --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)

selectConsumoCasaSemana :: Handler [(Double,Day)]
selectConsumoCasaSemana = do
    cid <- selectCasaId
    now <- liftIO getCurrentTime
    let (ano, mes, dia) = toGregorian $ utctDay now
    let dataDiff = fromGregorian ano mes dia
    let dataSum = addDays (-7) dataDiff
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo >=. dataSum, ConsumoDtConsumo <=. dataDiff] []) ambId
    consumoCasa <- return $ pegaConsumoMes consumoAmbientes
    return $ consumoCasa --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)

selectConsumoCasaMes :: Handler [(Double,Day)]
selectConsumoCasaMes = do
    cid <- selectCasaId
    now <- liftIO getCurrentTime
    let (ano, mes, dia) = toGregorian $ utctDay now
    let diaF = gregorianMonthLength ano mes
    let dataI = fromGregorian ano mes 1
    let dataF = fromGregorian ano mes diaF
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []) ambId
    consumoCasa <- return $ pegaConsumoMes consumoAmbientes
    --let somaCons = sum consumoCasa
    return $ consumoCasa --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)
    
pegaConsumoMes :: [[Entity Consumo]] -> [(Double,Day)]
pegaConsumoMes [] = []
pegaConsumoMes (x:xs) = map (\ent -> (consumoQtConsumo $ entityVal ent, consumoDtConsumo $ entityVal ent)) x ++ pegaConsumoMes xs
--(\ent -> (consumoQtConsumo $ entityVal ent, consumoDtConsumo $ entityVal ent))

selectConsumoCasaMesQual :: Int -> Handler Double
selectConsumoCasaMesQual mes = do
    cid <- selectCasaId
    now <- liftIO getCurrentTime
    let (ano, _, _) = toGregorian $ utctDay now
    let diaF = gregorianMonthLength ano mes
    let dataI = fromGregorian ano mes 1
    let dataF = fromGregorian ano mes diaF
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []) ambId
    consumoCasa <- return $ pegaConsumo consumoAmbientes
    let somaCons = sum consumoCasa
    return $ somaCons --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)

selectConsumoCasaAno :: Integer -> Handler [(Double,Int)]
selectConsumoCasaAno ano = do
    cid <- selectCasaId
    let dataI = fromGregorian ano 1 1
    let dataF = fromGregorian ano 12 31
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] [Asc ConsumoDtConsumo]) ambId
    consumoCasa <- return $ pegaConsumoAno ano consumoAmbientes
    return $ consumoCasa
    
pegaConsumoAno :: Integer -> [[Entity Consumo]] -> [(Double,Int)]
pegaConsumoAno _ [] = []
pegaConsumoAno ano (x:xs) = map (\ent -> (consumoQtConsumo $ entityVal ent, getMonth(getDate(consumoDtConsumo $ entityVal ent)))) x ++ pegaConsumoAno ano xs

getDate :: Day -> (Integer,Int,Int)
getDate x = toGregorian x

getMonth :: (Integer,Int,Int) -> Int
getMonth (_,mes,_) = mes

selectConsumoCasaPeriodo :: Integer -> Int -> Int -> Integer -> Int -> Int -> Handler Double
selectConsumoCasaPeriodo anoI mesI diaI anoF mesF diaF = do
    cid <- selectCasaId
    let dataI = fromGregorian anoI mesI diaI
    let dataF = fromGregorian anoF mesF diaF
    ambientes <- runDB $ selectList [AmbienteCasaId ==. cid] []
    ambId <- return $ map (entityKey) ambientes
    consumoAmbientes <- sequence $ map (\val -> runDB $ selectList [ConsumoAmbienteId ==. val, ConsumoDtConsumo >=. dataI, ConsumoDtConsumo <=. dataF] []) ambId
    consumoCasa <- return $ pegaConsumo consumoAmbientes
    let somaCons = sum consumoCasa
    return $ somaCons --RETORNA SOMA DO CONSUMO DOS AMBIENTES DA CASA (DOUBLE)
    
    
{-

{-***** EXEMPLO USANDO SELECT *****-}
{-***** EXEMPLO USANDO SELECT *****-}
{-***** EXEMPLO USANDO SELECT *****-}
{-***** EXEMPLO USANDO SELECT *****-}
{-***** EXEMPLO USANDO SELECT *****-}

getPag1R :: AmbienteId -> String -> Handler Html
getPag1R s1 s2 = do

    {-***** SELECT CONSUMO AMBIENTE *****-}
    {-***** SELECT CONSUMO AMBIENTE *****-}
    consumoAmbTotal <- selectConsumoAmbiente s1
    consumoAmbDia <- selectConsumoAmbienteDia s1 2017 06 12
    consumoAmbMes <- selectConsumoAmbienteMes s1 
    consumoAmbAno <- selectConsumoAmbienteAno s1 2017
    consumoAmbPeriodo <- selectConsumoAmbientePeriodo s1 2017 06 20 2017 07 12 -- Consumo periodo
    
    now <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    
    {-***** SELECT CONSUMO CASA *****-}
    {-***** SELECT CONSUMO CASA *****-}
    
    consumoCasaTotal <- selectConsumoCasa -- Consumo Total Casa do usuario
    consumoCasaDia <- selectConsumoCasaDia 2017 06 12 -- Consumo total de um dia
    consumoCasaMes <- selectConsumoCasaMes -- Consumo total de um mes
    consumoCasaAno <- selectConsumoCasaAno 2017 -- Consumo Anual
    consumoCasaPeriodo <- selectConsumoCasaPeriodo 2017 06 12 2017 07 12 --Consumo periodo
    --let d = utctDay $ getCurrentTime
    --let (x,y,z) = toGregorian d
    defaultLayout $ do
        [whamlet|
            <h1> 
                BEM-VINDO À Página 1 #{show consumoCasaTotal} #{show consumoCasaMes} #{show s2}
            <h2>
                MEEEEP #{show year} #{show month} #{show day}
            <br>
            <br>
            <br>
            <br>
            <h3>
                sdsdsd #{show consumoAmbTotal} #{show consumoAmbDia} #{show consumoAmbMes} #{show consumoAmbAno} #{show consumoAmbPeriodo}
            
        |]
-}