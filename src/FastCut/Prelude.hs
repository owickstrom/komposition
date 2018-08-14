module FastCut.Prelude (module X, readOne, readDecimal, readDouble) where

import           Protolude      as X

import qualified Data.Text.Read as Text

readOne :: Text.Reader a -> Text -> Maybe a
readOne r t =
  case r t of
    Left _err    -> Nothing
    Right (x, _) -> Just x

readDecimal :: Integral n => Text -> Maybe n
readDecimal = readOne Text.decimal

readDouble :: Text -> Maybe Double
readDouble = readOne Text.double
