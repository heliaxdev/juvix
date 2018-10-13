module Config where

import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Char        (isLower, toLower)
import qualified Data.Yaml        as Y
import           Foundation
import           GHC.Generics
import qualified Prelude          as P

data Config = Config {
  configTezosNode :: P.String
} deriving (Generic)

loadConfig :: P.FilePath -> IO (Maybe Config)
loadConfig = Y.decodeFile

instance Y.FromJSON Config where
  parseJSON = customParseJSON

jsonOptions :: A.Options
jsonOptions = A.defaultOptions {
  A.fieldLabelModifier = (\(h:t) -> toLower h : t) . dropWhile isLower,
  A.omitNothingFields  = True,
  A.sumEncoding        = A.ObjectWithSingleField
}

customParseJSON :: (A.GFromJSON A.Zero (Rep a), Generic a) => A.Value -> A.Parser a
customParseJSON = A.genericParseJSON jsonOptions
