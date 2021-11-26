-- |

module Data.Example where

{--
Type that an example can be provided for
-}
class Example a where
  example1 :: a -- example value of type a
  example2 :: a
  example2 = example
  example3 :: a
  example3 = example
  example4 :: a
  example4 = example

example :: Example a => a
example = example1
