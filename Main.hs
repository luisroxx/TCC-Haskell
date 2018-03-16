{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Yesod
import Yesod.Static
import Foundation
import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d1c10pbav0d75m host=ec2-184-73-236-170.compute-1.amazonaws.com user=tleisdchqtenjg password=330dc626aa4c19015b01bd1e3e52684faf7c77864c01ae0d1d51424e95c50411 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)