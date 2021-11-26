{-# language QuasiQuotes #-}
module Model where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens as URIL
import Text.URI.QQ as URIQQ
import Network.HTTP.Req ()
import Control.Lens hiding ((#))
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Data.Aeson.Types as AesonT
import Data.Aeson
import Data.Aeson.Lens

import Data.Example

data Citation
  = CitationUri URI
  | CitationText Text
  deriving (Eq, Show, Generic)

instance Example Citation where
  example1 = CitationUri [URIQQ.uri|http://google.com/|]
  example2 = CitationText "Rowling, J.K., Harry Potter and the Goblet of Fire, Random House 1998 ed"

makePrisms ''Citation

_URI :: Prism' Text URI
_URI = prism' URI.render URI.mkURI

-- Somewhat sketchy iso, as it will strip whitespace when round-tripping
-- URIs.
_Citation :: Iso' Text Citation
_Citation = iso parseCitation renderCitation
  where
    parseCitation text =
      (text ^? (to Text.strip) . _URI . (filtered coercibleToPublicUrl) . (to CitationUri))
        & fromMaybe (CitationText text)

    coercibleToPublicUrl = elemOf (domainName . each) '.'
    domainName = URIL.uriAuthority . _Right . URIL.authHost . URIL.unRText

    renderCitation = \case
          CitationUri uri -> uri ^. (re _URI)
          CitationText text -> text


instance FromJSON Citation where
  parseJSON = withText "Citation" (views _Citation pure)

instance ToJSON Citation where
  -- needs to be provided to shut up the generic implementation
  toJSON = views (from _Citation) toJSON
  toEncoding = views (from _Citation) toEncoding

data Source
  = Source {
    -- Primary key
    _citation :: Citation,
    _title :: Text,
    _notes :: Text,
    _related :: [Citation],
    _tags :: [Text],
    _uploaded :: UTCTime,
    _modified :: UTCTime
   }
   deriving (Eq, Show, Generic)

makeLenses ''Source

instance FromJSON Source
instance ToJSON Source where
  toEncoding = genericToEncoding defaultOptions
