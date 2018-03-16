{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Ambiente where
import Yesod
import Foundation
import Selects
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Maybe
import Text.Julius

import Database.Persist.Postgresql

data App = App

formAmb :: CasaId -> Form Ambiente
formAmb casaId = renderTable $ Ambiente 
            <$> areq nmAmbienteField "" Nothing 
            <*> pure casaId
            
nmAmbienteField :: Field Handler Text
nmAmbienteField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Nome:
            <input .form-control  placeholder="Insira o nome do ambiente:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField>
        |]
    , fieldEnctype = UrlEncoded
    }

getCriarAmbienteR :: Handler Html
getCriarAmbienteR = do
        casaId <- selectCasaId
        listaAmb <- runDB $ selectList [] [Asc AmbienteNmAmbiente]
        (widget, enctype) <- generateFormPost $ formAmb casaId
        --defaultLayout $ widgetForm CriarAmbienteR enctype widget "Criar Ambiente"
        defaultLayout $ do
           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
           widgetAmbiente listaAmb CriarAmbienteR enctype widget "Ambiente"
           
postCriarAmbienteR :: Handler Html
postCriarAmbienteR = do
        casaId <- selectCasaId
        ((res, _), _) <- runFormPost $ formAmb casaId
        case res of
                FormSuccess (formAmb) -> do
                   idx <- runDB $ insert formAmb
                   ambiente <- runDB $ get404 idx
                   setMessage $ [shamlet| Ambiente 
                                            <strong>
                                                #{ambienteNmAmbiente ambiente} 
                                            criado com sucesso |]
                   redirect $ ListarAmbienteR
                _ -> redirect CriarAmbienteR
                
getBuscarAmbienteR :: AmbienteId -> Handler Html
getBuscarAmbienteR aid = do
                        consumoAmb <- selectConsumoAmbiente aid -- isso aqui é Double
                        let consumo = sum consumoAmb -- Soma Total Ambiente
                        preco <- selectPreco -- Double
                        ambiente <- runDB $ get404 aid
                        casa     <- runDB $ get404 (ambienteCasaId ambiente)
                        jan <- selectConsumoAmbienteMesQual aid 1
                        fev <- selectConsumoAmbienteMesQual aid 2
                        mar <- selectConsumoAmbienteMesQual aid 3
                        abr <- selectConsumoAmbienteMesQual aid  4
                        maio <- selectConsumoAmbienteMesQual aid 5
                        jun <- selectConsumoAmbienteMesQual aid 6 
                        jul <- selectConsumoAmbienteMesQual aid 7
                        ago <- selectConsumoAmbienteMesQual aid 8
                        set <- selectConsumoAmbienteMesQual aid 9
                        out <- selectConsumoAmbienteMesQual aid 10
                        nov <- selectConsumoAmbienteMesQual aid 11
                        dez <- selectConsumoAmbienteMesQual aid 12
                        defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
                           toWidget $(juliusFile "templates/ambiente/view/chartanual.julius")
                           widgetVisuAmbiente aid ambiente casa consumo
                           

postApagarAmbienteR :: AmbienteId -> Handler Html
postApagarAmbienteR aid = do
                             temCons <- runDB $ selectFirst [ConsumoAmbienteId ==. aid] []
                             ambiente <- runDB $ get404 aid
                             case temCons of
                                Nothing -> do
                                    aid <- runDB $ delete aid
                                    defaultLayout $ do
                                       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                                       addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                                       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                                       setMessage $ [shamlet| Ambiente <strong>#{ambienteNmAmbiente ambiente} excluído com sucesso |]
                                       redirect $ ListarAmbienteR
                                Just _ -> do
                                   defaultLayout $ do
                                       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                                       addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                                       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                                       setMessage $ [shamlet| Não é permitido excluir o ambiente 
                                                                <strong>
                                                                    #{ambienteNmAmbiente ambiente}
                                                               , pois possui consumo |]
                                       redirect $ ListarAmbienteR
                                                  
getListarAmbienteR :: Handler Html
getListarAmbienteR = do
                        listaAmb <- runDB $ selectList [] [Asc AmbienteNmAmbiente]
                        mmsg <- getMessage
                        defaultLayout $ do 
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                            toWidget [julius| setTimeout(function() { $('#message').fadeOut('fast');}, 5000); |]
                            widgetListarAmbiente listaAmb mmsg
                            
        
                               
getAlterarAmbienteR :: AmbienteId -> Handler Html
getAlterarAmbienteR aid = do
                        casaId <- selectCasaId
                        ambiente <- runDB $ get404 aid
                        listaAmb <- runDB $ selectList [] [Asc AmbienteNmAmbiente]
                        (widget, enctype) <- generateFormPost $ formAlt casaId (ambienteNmAmbiente ambiente)
                        defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           widgetAltAmb listaAmb aid enctype widget
                               

formAlt :: CasaId -> Text -> Form Ambiente
formAlt casaId nmAmbiente = renderTable $ Ambiente 
            <$> areq (nmformAltField nmAmbiente) "" Nothing 
            <*> pure casaId
            
nmformAltField :: Text -> Field Handler Text
nmformAltField nmAmbiente = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Nome:
            <input .form-control value=#{nmAmbiente} placeholder="Insira o nome do ambiente:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField>
        |]
    , fieldEnctype = UrlEncoded
    }
    
postAlterarAmbienteR :: AmbienteId -> Handler Html
postAlterarAmbienteR aid = do
        casaId <- selectCasaId
        ambiente <- runDB $ get404 aid
        ((res, _), _) <- runFormPost $ formAlt casaId (ambienteNmAmbiente ambiente)
        case res of
            FormSuccess (formAlt) -> do
               resu <- runDB $ Database.Persist.Postgresql.replace aid formAlt
               ambnew <- runDB $ get404 aid
               defaultLayout $ do
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    setMessage $ [shamlet| Ambiente 
                                             <strong>
                                                #{ambienteNmAmbiente ambiente} 
                                            atualizado com sucesso, novo nome: #{show resu} |]
                    redirect $ ListarAmbienteR
            _ -> redirect ListarAmbienteR
    
getConsumoAmbienteDiarioR :: AmbienteId -> Handler Html
getConsumoAmbienteDiarioR aid = do
                        consumoAmb <- selectConsumoAmbiente aid -- isso aqui é Double
                        let consumo = sum consumoAmb -- Soma Total Ambiente
                        preco <- selectPreco -- Double
                        ambiente <- runDB $ get404 aid
                        casa     <- runDB $ get404 (ambienteCasaId ambiente)
                        consumo <- selectConsumoAmbienteDia aid
                        defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
                           toWidget $(juliusFile "templates/ambiente/view/chartdiario.julius")
                           widgetVisuAmbiente aid ambiente casa consumo


getConsumoAmbienteMensalR :: AmbienteId -> Handler Html
getConsumoAmbienteMensalR aid = do
                        consumoAmb <- selectConsumoAmbiente aid -- isso aqui é Double
                        let consumo = sum consumoAmb -- Soma Total Ambiente
                        preco <- selectPreco -- Double
                        ambiente <- runDB $ get404 aid
                        casa     <- runDB $ get404 (ambienteCasaId ambiente)
                        cons <- selectConsumoAmbienteMes aid
                        defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
                           toWidget $(juliusFile "templates/ambiente/view/chartmensal.julius")
                           widgetVisuAmbiente aid ambiente casa consumo

getConsumoAmbienteSemanalR :: AmbienteId -> Handler Html
getConsumoAmbienteSemanalR aid = do
                        consumoAmb <- selectConsumoAmbiente aid -- isso aqui é Double
                        let consumo = sum consumoAmb -- Soma Total Ambiente
                        preco <- selectPreco -- Double
                        ambiente <- runDB $ get404 aid
                        casa     <- runDB $ get404 (ambienteCasaId ambiente)
                        cons <- selectConsumoAmbienteSemana aid
                        defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.6.0/Chart.min.js"
                           toWidget $(juliusFile "templates/ambiente/view/chartsemanal.julius")
                           widgetVisuAmbiente aid ambiente casa consumo