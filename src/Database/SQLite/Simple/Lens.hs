module Database.SQLite.Simple.Lens where

import Control.Lens
import Database.SQLite.Simple (SQLData)

makePrisms ''SQLData
