module ModelSpec where

import Model

import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import Test.QuickCheck.Instances.Time
import Generic.Random
import Network.URI (URI(..), URIAuth(..))
import qualified Network.URI as URI
import qualified Network.URI.Encode as UriE
import Data.String (IsString, fromString)
import Data.List (intersperse)
import Data.Foldable (fold, foldl')

tld :: IsString a => Gen a
tld = elements
  [ "org"
  , "com"
  , "net"
  , "io"
  , "co.in"
  , "co.uk"
  , "it"
  , "xxx"
  , "mx"
  ]

domainChar :: Gen Char
domainChar = elements $ fold
  [ ['a' .. 'z'], ['A' .. 'Z'], ['-', '0'], ['1' .. '9'] ]

intercalate :: (Foldable f, Monoid a) => a -> f a -> a
intercalate item foldable =
  case foldl' go Nothing foldable of
    Nothing -> mempty
    Just a -> a
  where
    go Nothing new = Just new
    go (Just a) new = Just (a <> item <> new)

genUriRegName :: (Monoid a, IsString a) => Gen a
genUriRegName = do
  domainNameParts <- listOf . (fmap fromString) . listOf1 $ domainChar
  tld' <- tld
  pure $ intercalate "." (domainNameParts ++ [tld'])

genPort :: (IsString a) => Gen a
genPort = frequency [(1, (fromString . show) <$> chooseInt (0, 2^16 - 1)), (5, pure "")]

instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> pure ""
                   <*> genUriRegName
                   <*> genPort
  shrink = genericShrink

-- Should not be used in general, only works for https?
instance Arbitrary URI where
  arbitrary = URI
    <$> elements [ "http:", "https:" ]
    <*> fmap Just arbitrary
    <*> withExtraEmpties ((("/" <>) . UriE.encode) <$> arbitrary)
    <*> withExtraEmpties ((("?" <>) . UriE.encode) <$> arbitrary)
    <*> withExtraEmpties ((("#" <>) . UriE.encode) <$> arbitrary)
    where
      withExtraEmpties :: Monoid a => Gen a -> Gen a
      withExtraEmpties g = frequency [(2, pure mempty), (1, g)]
  shrink = genericShrink


instance Arbitrary Citation where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary Source where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink
