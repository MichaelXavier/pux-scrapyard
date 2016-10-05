module Components.AJAXList
    ( State(..)
    , AJAXListItem(..)
    , ItemStatus(..)
    , initialState
    , Action(..)
    , ItemAction(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Pux.Html (div, Html)
-------------------------------------------------------------------------------


type State = {
      items :: M.Map Int AJAXListItem
    }

type AJAXListItem = {
      text :: String
    , newText :: Maybe String
    , status :: ItemStatus
    , id :: Int
    }

data ItemStatus = Created
                | Deleting
                | Deleted


initialState :: State
initialState = { items: mempty }


data Action = RefreshList
            | ItemAction Int ItemAction


data ItemAction = DeleteItem


update :: Action -> State -> State
update _ s = s


view :: State -> Html Action
view _ = div
  []
  []
