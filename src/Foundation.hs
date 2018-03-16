{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Text.Blaze.Html5
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Casa json
    nmCasa Text
    deriving Show

Usuario json
    nmUsuario Text
    cdPassword Text
    tpUsuario Int
    casaId CasaId
    UniqueEmail nmUsuario
    
Ambiente json
    nmAmbiente Text
    casaId CasaId
    deriving Show
    
Preco json
    qtPreco Double
    deriving Show

Consumo json
    dtConsumo Day
    qtConsumo Double
    precoId PrecoId
    ambienteId AmbienteId
    deriving Show
    UNIQUEConsumo dtConsumo ambienteId
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized ConsumoR _ = return Authorized
    isAuthorized PrecoR _ = isAdmin
    isAuthorized (ArduinoConsumoR _ _ _ _) _ = return Authorized
    isAuthorized CriarAutorizadoR _ = isAdmin
    isAuthorized CriarAmbienteR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mi <- lookupSession "_ID"
    mu <- lookupSession "_USER"
    return $ case mi of
        Nothing -> do 
            case mu of
              Nothing -> AuthenticationRequired
              Just _ -> Unauthorized "Soh o admin acessa aqui!"
        Just "admin" -> Authorized

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
    
    
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")

widgetAmbiente :: [Entity Ambiente] -> Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetAmbiente listaAmb x enctype widget y = $(whamletFile "templates/ambiente/new.hamlet")

widgetListarAmbiente :: [Entity Ambiente] -> (Maybe Html) -> Widget
widgetListarAmbiente listaAmb msgComMaybe = $(whamletFile "templates/ambiente/list.hamlet")

widgetUsuario :: [Entity Usuario] -> Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetUsuario listaUsu x enctype widget y = $(whamletFile "templates/usuario/new.hamlet")

widgeHome :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgeHome x enctype widget y = $(whamletFile "templates/index.hamlet")

widgetVisuAmbiente :: AmbienteId -> Ambiente -> Casa -> Double -> Widget
widgetVisuAmbiente aid ambiente casa  consumo = $(whamletFile "templates/ambiente/view.hamlet")

widgetListarUsuario :: [Entity Usuario] -> (Maybe Html) -> Widget
widgetListarUsuario listaUsu msgComMaybe = $(whamletFile "templates/usuario/list.hamlet")

widgetVisuUsuario :: [Entity Usuario] -> UsuarioId -> Enctype -> Widget -> Widget
widgetVisuUsuario listaUsu usuario enctype widget = $(whamletFile "templates/usuario/edit.hamlet")

widgetVisuConsumo :: Casa -> Double -> Double -> Widget
widgetVisuConsumo casa preco consumo = $(whamletFile "templates/consumo.hamlet")

widgetAltAmb :: [Entity Ambiente] -> AmbienteId -> Enctype -> Widget -> Widget
widgetAltAmb listaAmb ambiente enctype widget = $(whamletFile "templates/ambiente/edit.hamlet")


widgetVisuRelatorio :: Casa -> Double -> Double -> Widget
widgetVisuRelatorio casa preco consumo= $(whamletFile "templates/relatorio.hamlet")

widgetVisuRelatorioM :: Casa -> Double -> Double -> [(Double,Day)] -> Widget
widgetVisuRelatorioM casa preco consumo array = $(whamletFile "templates/relatorio/mensal.hamlet")

widgetVisuRelatorioD :: Casa -> Double -> Double -> Double -> Widget
widgetVisuRelatorioD casa preco consumo cons = $(whamletFile "templates/relatorio/diario.hamlet")

widgetVisuRelatorioS :: Casa -> Double -> Double -> [(Double,Day)] -> Widget
widgetVisuRelatorioS casa preco consumo array = $(whamletFile "templates/relatorio/semanal.hamlet")


