module ParserExtra ( count
                   , skip) where

import Data.List (List(..), (:), reverse)
import Prelude (Unit, (-), (<$), (<=), (>>=), pure, unit)
import Text.Parsing.StringParser (Parser)

-- | Parse `n` occurences of `p`. -
count :: forall a. Int -> Parser a -> Parser (List a)
count n p =
  let
    accumulate x acc =
      if x <= 0 then
        pure (reverse acc)
      else
        p >>= (\res -> accumulate (x - 1) (res : acc))
  in
    accumulate n Nil

-- | Apply a parser and skip its result.
skip :: forall a. Parser a -> Parser Unit
skip p = unit <$ p
