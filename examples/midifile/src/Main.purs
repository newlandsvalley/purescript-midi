module Main where

import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi.Parser (parse, normalise, translateRunningStatus)
import Prelude (Unit, bind, const, discard, show, pure, ($), (<>))
import Pux (EffModel, noEffects, start)
import Pux.DOM.Events (onChange)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, h1, p, input)
import Text.Smolder.HTML.Attributes (type', id, accept)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import Signal.Channel (CHANNEL)
import BinaryFileIO.FileIO

data Event
  = NoOp
  | RequestFileUpload
  | FileLoaded Filespec

type State =
  { filespec :: Maybe Filespec }

initialState :: State
initialState = {
    filespec : Nothing
  }

foldp :: Event -> State -> EffModel State Event (fileio :: FILEIO)
foldp NoOp state =  noEffects $ state
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadBinaryFile
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp (FileLoaded filespec) state =
   noEffects $ saveFilespec filespec state

saveFilespec :: Filespec -> State -> State
saveFilespec filespec state =
   state { filespec =  Just filespec }

viewParsedFile :: State -> String
viewParsedFile state =
  case state.filespec of
    Nothing ->
      ""
    Just fs ->
      fullParse fs.contents

fullParse :: String -> String
fullParse s =
  case translateRunningStatus $ parse $ normalise $ s of
    Left err ->
      ("Parse error:" <> err)
    Right midi ->
      (show midi)

view :: State -> HTML Event
view state =
   div  do
     h1 ! centreStyle $ text "view a MIDI file"
     div do
       input ! type' "file" ! id "fileinput" ! accept ".midi"
         #! onChange (const RequestFileUpload)
       p $ text $ viewParsedFile state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

main :: Eff (channel :: CHANNEL, exception :: EXCEPTION, fileio :: FILEIO ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
