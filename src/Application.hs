{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where
import Foundation
import Yesod
import Usuario
import Login
import Selects
import Preco
import Ambiente
import Consumo

-- Application
mkYesodDispatch "Sitio" resourcesSitio
