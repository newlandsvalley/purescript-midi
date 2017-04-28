module Main where

import Player (foldp, initialState, view)
import Audio.SoundFont (AUDIO)
import BinaryFileIO.FileIO (FILEIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects (fileio :: FILEIO, au :: AUDIO | fx)) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
