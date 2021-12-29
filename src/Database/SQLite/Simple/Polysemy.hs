-- | 

module Database.SQLite.Simple.Polysemy
  ( query
  , query_
  , queryNamed
  , execute
  , execute_
  , executeNamed
  , sqliteToIO
  , runSqlite
  , sqliteMemToIO
  , Sqlite
  -- Re-exports from the normal sqlite simple package
  , Src.Query(..)
  , Src.ToRow
  , Src.FromRow
  , Src.Only(..)
  , (:.)(..)
  , Src.SQLData
  , Src.NamedParam(..)
  , Src.FormatError(..)
  , Src.ResultError(..)
  , Src.SQLError(..)
  , Src.Error(..)
  )
  where

import Polysemy
import Polysemy.Resource (Resource)
import qualified Polysemy.Resource as Resource
import Database.SQLite.Simple (ToRow, FromRow, (:.))
import qualified Database.SQLite.Simple as Src
import qualified GHC.IO as IO
import Data.Function ((&))

data Sqlite m a where
  -- Throws ConversionFailed
  Query :: (ToRow q, FromRow r) => Src.Query -> q -> Sqlite m [r]
  Query_ :: FromRow r => Src.Query -> Sqlite m [r]
  QueryNamed :: FromRow r => Src.Query -> [Src.NamedParam] -> Sqlite m [r]
  Execute :: ToRow q => Src.Query -> q -> Sqlite m ()
  Execute_ :: Src.Query -> Sqlite m ()
  ExecuteNamed :: Src.Query -> [Src.NamedParam] -> Sqlite m ()
  -- TODO: withTransaction and friends
  -- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html#v:withTransaction

makeSem ''Sqlite

-- | Interpret an SQLite Effect on the database handle
sqliteToIO :: (Final IO) `Member` r => String -- ^ The database name to open. See: <https://www.sqlite.org/c3ref/open.html>
  -> Sem (Sqlite ': r) a -> Sem r a
sqliteToIO dbName action =
  Resource.bracket openConn closeConn ((flip runSqlite) action')
     & Resource.resourceToIOFinal
  where
    openConn = raise @Resource $ embedFinal $ Src.open dbName
    closeConn = (raise @Resource) . embedFinal . Src.close
    action' = raiseUnder @Resource action

-- Technically this could be rewritten with Embed instead of Final, but the
-- type signature of sqliteToIO would get really ugly
runSqlite :: (Final IO) `Member` r => Src.Connection -> Sem (Sqlite ': r) a -> Sem r a
runSqlite conn = interpret $ \case
  Query query params -> embedFinal $ Src.query conn query params
  Query_ query -> embedFinal $ Src.query_ conn query
  QueryNamed query namedParams -> embedFinal $ Src.queryNamed conn query namedParams
  Execute query params -> embedFinal $ Src.execute conn query params
  Execute_ query -> embedFinal $ Src.execute_ conn query
  ExecuteNamed query namedParams -> embedFinal $ Src.executeNamed conn query namedParams

sqliteMemToIO :: (Final IO) `Member` r => Sem (Sqlite ': r) a -> Sem r a
sqliteMemToIO = sqliteToIO ":memory:"
