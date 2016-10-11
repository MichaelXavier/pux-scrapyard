module Components.App
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Components.AJAXList as AJAXList
import Components.Counter as Counter
import Components.List as List
import Components.Now as Now
import Pux.Html as H
import Control.Alt ((<|>))
import Data.Functor ((<$), map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Network.HTTP.Affjax (AJAX)
import Prelude (pure)
import Pux (EffModel, noEffects)
import Pux.Html (text, li, ul, nav, (#), div, (!), Html)
import Pux.Html.Attributes (className)
import Pux.Router (link, lit, end, router)
-------------------------------------------------------------------------------


data Action = AJAXListAction AJAXList.Action


type State = {
      ajaxListState :: AJAXList.State
    }


initialState :: State
initialState = { ajaxListState: AJAXList.initialState
               }


--TODO: can probably use some of the state helpers
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (AJAXListAction a) s =
  let eff = AJAXList.update a s.ajaxListState
  in { state: s { ajaxListState = eff.state}
     , effects: map (map AJAXListAction) eff.effects}


--TODO: maybe request data on page change?
view :: State -> Html Action
view s =
  div
    ! className "component"
    # do
      page
  where
    page = map AJAXListAction (AJAXList.view s.ajaxListState)
    bind = H.bind
