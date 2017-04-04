module Main where

import CSS.TextAlign (textAlign)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)
import Prelude (Unit, bind, const, show, pure, ($), (<>))
import Pux (EffModel, renderToDOM, start, noEffects)
import Pux.CSS (center, style)
import Pux.Html (Html, Attribute, text, h1, input, div, p)
import Pux.Html.Events (onChange)
import Pux.Html.Attributes (type_, id_, accept)
import Signal.Channel (CHANNEL)
import FileIO.FileIO

data Action
  = NoOp
  | RequestFileUpload
  | FileLoaded Filespec

type State =
  { filespec :: Maybe Filespec }

initialState :: State
initialState = {
    filespec : Nothing
  }

update :: Action -> State -> EffModel State Action (fileio :: FILEIO)
update NoOp state =  noEffects $ state
update RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile
         pure $ FileLoaded filespec
     ]
  }
update (FileLoaded filespec) state =
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
  case parse s of
    Left ppe ->
      ("Parse error:" <> (show ppe))
    Right abc ->
      (show $ toMidi abc)

view :: State -> Html Action
view state =
  div []
    [
      h1 [ centreStyle ] [ text "load an ABC file as MIDI" ]
      , input
            [ type_ "file"
            , id_ "fileinput"
            , accept ".abc"
            , onChange (const RequestFileUpload)
            ]
            []
      , p [] [ text $ viewParsedFile state]
    ]

centreStyle :: forall a. Attribute a
centreStyle =
    style $ do
       textAlign center

main :: Eff (channel :: CHANNEL, err :: EXCEPTION, fileio :: FILEIO ) Unit
main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
