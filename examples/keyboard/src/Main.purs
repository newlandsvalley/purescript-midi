module Main where

import App (foldp, initialState, view)
import Audio.SoundFont (AUDIO)
import Data.Midi.WebMidi (WEBMIDI)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

-- | Start and render the app
main :: Eff (CoreEffects (wm :: WEBMIDI, au :: AUDIO )) Unit
-- identical to:
-- main :: Eff (channel :: CHANNEL, err :: EXCEPTION, wm :: WEBMIDI, au ::AUDIO ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
