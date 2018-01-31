module Main where

import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Data.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Data.MediaType (MediaType(..))
import Control.Monad.Aff (Aff)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Data.Midi.Parser (parse, normalise)
import Prelude (Unit, bind, const, discard, show, map, pure, ($), (<>), (<<<))
import Pux (EffModel, noEffects, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (div, h1, p, button)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import Signal.Channel (CHANNEL)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Class (liftEff)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.Int.Bits (and)
import Data.Char (fromCharCode)
import Data.String (fromCharArray)


type ReturnedType = ArrayBuffer

data Event
  = NoOp
  | RequestUrlUpload
  | FileLoaded ArrayBuffer


type State =
  { midi :: Maybe ArrayBuffer }

initialState :: State
initialState = {
    midi : Nothing
  }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, console :: CONSOLE)
foldp NoOp state =  noEffects $ state
foldp RequestUrlUpload state =
 { state: state
   , effects:
     [ do
         contents <- loadMidi "lillasystern.midi"
         pure $ Just $ FileLoaded contents
     ]
  }
foldp (FileLoaded contents) state =
   noEffects $ saveMidi contents state

saveMidi :: ArrayBuffer -> State -> State
saveMidi contents state =
   state { midi = Just contents }

viewParsedFile :: State -> String
viewParsedFile state =
  case state.midi of
    Nothing ->
      ""
    Just contents ->
      fullParse $ denormalise $ toUint8Array contents

toUint8Array :: ArrayBuffer ->  Uint8Array
toUint8Array ab =
  asUint8Array $ whole ab

denormalise :: Uint8Array -> String
denormalise =
  let
    f = fromCharCode <<< ((and) 0xFF)
  in
    fromCharArray <<< map f <<< toIntArray

logResponseHeaders :: ∀ eff.  Array ResponseHeader -> Eff (console :: CONSOLE | eff) Unit
logResponseHeaders hs =
  traverse_ logShow hs

loadMidi :: ∀ e.
  String
  -> Aff
     ( ajax :: AJAX
     , console :: CONSOLE
     | e
     )
     ArrayBuffer
loadMidi name = do
  let
    url =
      "midi/" <> name
      --  "http://localhost/PureScript/purescript-midi/midi/" <> name
  res <- affjax $ defaultRequest
           { url = url
           , method = Left GET
           , headers = [ Accept (MediaType "audio/midi")]
           }
  _ <- liftEff $ logResponseHeaders res.headers
  pure $ res.response

fullParse :: String -> String
fullParse s =
  case parse $ normalise $ s of
    Left err ->
      ("Parse error:" <> err)
    Right midi ->
      (show midi)

view :: State -> HTML Event
view state =
   div  do
     h1 ! centreStyle $ text "view a MIDI file loaded from URL"
     div do
       button ! className "hoverable" #! onClick (const RequestUrlUpload) $ text "load example"
       p $ text $ viewParsedFile state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

main :: Eff (channel :: CHANNEL, exception :: EXCEPTION, ajax :: AJAX, console :: CONSOLE ) Unit
main = do
  app <- start
    { initialState: initialState
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
