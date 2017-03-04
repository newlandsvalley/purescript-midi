module Data.Midi.ParserExtra
   ( count
   , many1Till
   , skip) where

import Data.List (List(..), (:), reverse)
import Prelude (Unit, (-), (<$), (<=), bind, pure, unit)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (manyTill)

-- | Parse `n` occurences of `p`. -
count :: forall a. Int -> Parser a -> Parser (List a)
count n p =
  let
    accumulate x acc =
      if x <= 0 then
        pure (reverse acc)
      else do
        res <- p
        accumulate (x - 1) (res : acc)
  in
    accumulate n Nil

-- | Apply a parser and skip its result.
skip :: forall a. Parser a -> Parser Unit
skip p = unit <$ p

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
many1Till :: forall a end. Parser a -> Parser end -> Parser (List a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (x:xs)
