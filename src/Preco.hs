{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Preco where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Network.HTTP.Types.Status
import Database.Persist.Postgresql

formPreco :: Form Preco
formPreco = renderDivs $ Preco <$>
             areq doubleField "Valor" Nothing

getPrecoR :: Handler Html
getPrecoR = do
            (widget, enctype) <- generateFormPost formPreco
            defaultLayout $ widgetForm PrecoR enctype widget "Alterar Preco"
            
postPrecoR :: Handler Html
postPrecoR = do
    ((result, _), _) <- runFormPost formPreco
    case result of
        FormSuccess preco -> do
                cid <-runDB $ update (toSqlKey 0) [PrecoQtPreco =. precoQtPreco preco]
                defaultLayout [whamlet|
                                          <h1> PRECO ALTERADO 
                                      |]
        _ -> redirect PrecoR