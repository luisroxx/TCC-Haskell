{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Selects
import Data.Maybe

import Database.Persist.Postgresql
    
formUsu :: CasaId -> Int -> Form Usuario
formUsu casaId tpUsuario = renderDivs $ Usuario 
            <$> areq nmUsuarioField "" Nothing 
            <*> areq cdPasswordField "" Nothing
            <*> pure tpUsuario
            <*> pure casaId
            
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
            <input .form-control  placeholder="Insira o email do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=emailField>
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
            
getCriarAutorizadoR :: Handler Html
getCriarAutorizadoR = do
            casaId <- selectCasaId
            let tpUsuario = 1
            listaUsu <- runDB $ selectList [UsuarioCasaId ==. casaId] [Asc UsuarioNmUsuario]
            (widget, enctype) <- generateFormPost $ formUsu casaId tpUsuario
            defaultLayout $ do
               addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
               addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
               addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
               widgetUsuario listaUsu CriarAutorizadoR enctype widget "Autorizado"
               -- ALTERAR O WIGET
               
getListarUsuarioR :: Handler Html
getListarUsuarioR = do
                    casaId <- selectCasaId
                    nmUser <- lookupSession "_USER"
                    listaUsu <- runDB $ selectList [UsuarioCasaId ==. casaId, UsuarioNmUsuario !=. fromJust(nmUser)] []
                    mmsg <- getMessage
                    defaultLayout $ do 
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    toWidget [julius| setTimeout(function() { $('#message').fadeOut('fast');}, 5000); |]
                    widgetListarUsuario listaUsu mmsg
                    
postApagarUsuarioR :: UsuarioId -> Handler Html
postApagarUsuarioR uid = do
                usuario <- runDB $ get404 uid
                uid <- runDB $ delete uid
                defaultLayout $ do
                   addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                   addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                   addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                   setMessage $ [shamlet| Usuário 
                                            <strong>
                                                #{usuarioNmUsuario usuario} 
                                            excluído com sucesso |]
                   redirect $ ListarUsuarioR
                
postCriarAutorizadoR :: Handler Html
postCriarAutorizadoR = do
            casaId <- selectCasaId
            let tpUsuario = 1 
            ((res, _), _) <- runFormPost $ formUsu casaId tpUsuario
            case res of
                    FormSuccess (formUsu) -> do
                       idx <- runDB $ insert formUsu
                       usuario <- runDB $ get404 idx
                       defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           setMessage $ [shamlet| Usuário autorizado 
                                                    <strong>#{usuarioNmUsuario usuario} 
                                                   criado com sucesso |]
                           redirect $ ListarUsuarioR
                    _ -> redirect CriarAutorizadoR     
                    
formNewUsu :: Form (Text, Text)
formNewUsu  = renderDivs $ (,) <$> 
            areq nmUsuarioField "" Nothing <*>
            areq cdPasswordField "" Nothing        
                   
getHomeR :: Handler Html
getHomeR = do
            (widget, enctype) <- generateFormPost formNewUsu
            defaultLayout $ do
               addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
               addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
               addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
               widgeHome HomeR enctype widget "Autorizado"
               


postHomeR :: Handler Html
postHomeR = do
            ((result, _), _) <- runFormPost formNewUsu
            case result of
                FormSuccess (username,password) -> do
                    temUsu <- runDB $ selectFirst [UsuarioNmUsuario ==. username] []
                    case temUsu of
                        Nothing -> do
                            cid <- runDB $ insert (Casa "default")
                            user <- runDB $ insert $ criaUser username password cid
                            defaultLayout $ do
                                       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                                       addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                                       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                                       [whamlet| 
                                            <div .form-group>
                                            <h1> #{username} inserido com sucesso!
                                            <a href=@{LoginR}>
                                              <button .btn .btn-primary type="submit">Login
                                    |]
                        Just _ -> do
                                defaultLayout $ do
                                       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                                       addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                                       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                                       [whamlet| 
                                            <div .form-group>
                                            <h1> Nome de usuário já existe!
                                            <a href=@{LoginR}>
                                              <button .btn .btn-primary type="submit">Login
                                    |]
                _ -> redirect HomeR
                
criaUser :: Text -> Text -> CasaId -> Usuario
criaUser nm pass cid = Usuario nm pass 0 cid
                
getAlterarUsuarioR :: UsuarioId -> Handler Html
getAlterarUsuarioR uid = do
                         usuario <- runDB $ get404 uid
                         casaId <- selectCasaId
                         let tpUsuario = 1
                         listaUsu <- runDB $ selectList [UsuarioCasaId ==. casaId] [Asc UsuarioNmUsuario]
                         (widget, enctype) <- generateFormPost $ formAlt casaId tpUsuario (usuarioNmUsuario usuario)
                         defaultLayout $ do
                           addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                           addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                           addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                           widgetVisuUsuario listaUsu uid enctype widget
                        
                        
postAlterarUsuarioR :: UsuarioId -> Handler Html
postAlterarUsuarioR uid = do
                     usuario <- runDB $ get404 uid
                     casaId <- selectCasaId
                     let tpUsuario = 1
                     ((res, _), _) <- runFormPost $ formAlt casaId tpUsuario (usuarioNmUsuario usuario)
                     case res of
                            FormSuccess (formAlt) -> do
                               resu <- runDB $ Database.Persist.Postgresql.replace uid formAlt
                               defaultLayout $ do
                                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
                                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                                    setMessage $ [shamlet| Usuário autorizado 
                                                            <strong>
                                                                #{usuarioNmUsuario usuario} 
                                                            atualizado com sucesso |]
                                    redirect $ ListarUsuarioR
                            _ -> redirect ListarUsuarioR

formAlt :: CasaId -> Int -> Text -> Form Usuario
formAlt casaId tpUsuario usuario = renderDivs $ Usuario 
            <$> areq (alterNameField usuario) "" Nothing 
            <*> areq cdPasswordField "" Nothing
            <*> pure tpUsuario
            <*> pure casaId
                               
alterNameField :: Text -> Field Handler Text
alterNameField nmUsuario = Field
    { fieldParse = \rawVals _ ->
                 case rawVals of
                   [a] -> return $ Right $ Just a
                   [] -> return $ Right Nothing
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
        <div .form-group> 
            <h4>Email:
            <input .form-control value=#{nmUsuario} placeholder="Insira o email do usuário:" id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=emailField>
        |]
    , fieldEnctype = UrlEncoded
    }
    
