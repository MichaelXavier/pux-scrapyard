module Components.ListItem
    ( State(..)
    , Action(..)
    , update
    , view
    , initialState
    ) where


-------------------------------------------------------------------------------
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Prelude (const)
import Pux.Html ((##), input, button, (#), (!), div, text, Html)
import Pux.Html.Attributes (defaultValue, className)
import Pux.Html.Events (onKeyUp, onClick)
-------------------------------------------------------------------------------


type State = {
      text :: String
    , newText :: Maybe String
    , deleted :: Boolean
    , id :: Int
    }

data Action = Delete
            | Edit String
            | CancelEdit
            | SaveEdit


initialState :: Int -> String -> State
initialState id text = { text: text
                       , newText: Nothing
                       , deleted: false
                       , id: id
                       }

update :: Action -> State -> State
update Delete s                     = s { deleted = true }
update (Edit newText) s             = s { newText = Just newText }
update CancelEdit s                 = s { newText = Nothing }
update SaveEdit s@{newText: Just t} = s { text = t
                                        , newText = Nothing
                                        }
update SaveEdit s                   = s


view :: State -> Html Action
view s = div ! className "component" ##
  textContent <>
  [ button !
      onClick (const Delete) #
      text "Delete"]
  where
    textContent = maybe displayCurrent displayEdit s.newText
    displayCurrent = [
      text s.text
    , button
        ! onClick (const (Edit s.text))
        # text "Edit"
    ]
    displayEdit newText = [
      editInput newText
    , button
        ! onClick (const CancelEdit)
        # text "Cancel"
    , button
        ! onClick (const SaveEdit)
        # text "Save"
    ]
    editInput newText = input
      [ defaultValue newText
      , onKeyUp (\e -> Edit e.target.value)
      ]
      []
