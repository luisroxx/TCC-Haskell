{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Maybe

import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq nmUsuarioField "" Nothing <*>
             areq cdPasswordField "" Nothing 
             
             
nmUsuarioField :: Field Handler Text
nmUsuarioField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Email:
            <input .form-control  placeholder="Insira o email do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=textField>
        |]
    , fieldEnctype = UrlEncoded
    }
    
cdPasswordField :: Field Handler Text
cdPasswordField = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Senha:
            <input .form-control  placeholder="Insira a senha do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }
            

getLoginR :: Handler Html
getLoginR = do
            usr <- lookupSession "_USER"
            case usr of
                Nothing -> do
                    (widget, enctype) <- generateFormPost formLogin
                    defaultLayout $ do
                     addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                     addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                     addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                     widgetForm LoginR enctype widget "Login page" 
                Just _ -> do
                    usrId <- lookupSession "_ID"
                    case usrId of
                        Nothing ->redirect CriarAmbienteR
                        Just _ -> redirect CriarAmbienteR
                        
postLoginR :: Handler Html
postLoginR = do
                ((result, _), _) <- runFormPost formLogin
                case result of
                    FormSuccess (username,senha) -> do
                       temUsu <- runDB $ selectFirst [UsuarioNmUsuario ==. username,UsuarioCdPassword ==. senha] []
                       case temUsu of
                           Nothing -> redirect LoginR
                           Just _ -> do
                                tipo <- return $ fromMaybe 0 $ fmap (usuarioTpUsuario . entityVal) $ temUsu
                                case tipo of
                                    0 -> do
                                        setSession "_USER" username
                                        setSession "_ID" "admin"
                                        defaultLayout [whamlet| Admin autenticado!|]
                                        redirect ConsumoCasaR
                                    1 -> do
                                        setSession "_USER" username
                                        defaultLayout [whamlet| Usuario autenticado!|]
                                        redirect ConsumoCasaR
                    _ -> redirect LoginR
                    
postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_USER"
    deleteSession "_ID"
    redirect LoginR