module BinaryFileIO.FileIO
  ( FILEIO
  , Filespec
  , loadBinaryFile ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)
import Prelude (Unit)

-- | a file name and its contents
type Filespec =
  {
    contents :: String
  , name :: String
  }

-- | File IO Effect
foreign import data FILEIO :: Effect

foreign import loadBinaryFileImpl :: forall e. (Filespec -> Eff e Unit) -> Eff e Unit

-- | load a binary file
loadBinaryFile :: forall e. Aff e Filespec
loadBinaryFile = makeAff (\error success -> loadBinaryFileImpl success)
