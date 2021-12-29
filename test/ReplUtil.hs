module ReplUtil where

import Test.QuickCheck
import System.IO.Unsafe
import Database.SQLite.Simple.Polysemy
import Data.Text
import Polysemy
import Control.Monad (forM_)

gimme :: Arbitrary a => a
gimme = get arbitrary

get :: Gen a -> a
get = unsafePerformIO . generate

sqlTest :: IO ()
sqlTest = runFinal . sqliteMemToIO $ do
  execute_ "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, value TEXT)"
  ins "Hee hee"
  ins "Hoo Hoo"
  ins "Look ma, I'm some text!"
  ins "weeeee"
  xs <- query_ "SELECT id, value FROM test"
  embedFinal $ forM_ xs $ \(id, txt) ->
    putStrLn $ (show @Int id) <> ", " <> (show @Text txt)
  where
    ins :: Sqlite `Member` r => Text -> Sem r ()
    ins txt = execute "INSERT INTO test (value) VALUES (?)" (Only txt)
