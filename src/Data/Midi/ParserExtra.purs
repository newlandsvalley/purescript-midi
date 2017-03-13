module Data.Midi.ParserExtra
   ( many1Till ) where

import Data.List (List, (:))
import Prelude (bind, pure)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (manyTill)

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
many1Till :: forall a end. Parser a -> Parser end -> Parser (List a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (x:xs)
