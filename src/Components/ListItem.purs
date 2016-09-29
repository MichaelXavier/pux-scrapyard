module Components.ListItem
    ( State(..)
    , Action(..)
    , update
    , view
    , initialState
    ) where


-------------------------------------------------------------------------------
import Pux.Html as H
import Prelude (const)
import Pux.Html (button, (#), (!), div, text, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      text :: String
    , deleted :: Boolean
    , id :: Int
    }

data Action = Nop
            | Delete


initialState :: Int -> String -> State
initialState id text = {text: text, deleted: false, id: id}

update :: Action -> State -> State
update Nop s    = s
update Delete s = s { deleted = true}


view :: State -> Html Action
view s = div ! className "component" # do
  text s.text
  button
    ! onClick (const Delete)
    # text "Delete"
  where
    bind = H.bind
