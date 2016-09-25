module Components.ListItem
    ( State(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Pux.Html ((#), (!), div, text, Html)
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------


type State = {
      text :: String
    }


data Action = Nop


update :: Action -> State -> State
update Nop s = s


view :: State -> Html Action
view s = div ! className "component" # do
  text s.text
