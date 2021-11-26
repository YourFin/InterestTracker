module ReplUtil where

import Test.QuickCheck
import System.IO.Unsafe

gimme :: Arbitrary a => a
gimme = get arbitrary

get :: Gen a -> a
get = unsafePerformIO . generate
