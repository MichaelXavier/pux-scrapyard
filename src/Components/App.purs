module Components.App
    ( State(..)
    , initialState
    , Action(..)
    , Route(..)
    , update
    , view
    , match
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
import Network.HTTP.Affjax (AJAX)
import Pux (noEffects, EffModel)
import Pux.Html (text, li, ul, nav, (#), div, (!), Html)
import Pux.Html.Attributes (className)
import Pux.Router (link, lit, end, router)
-------------------------------------------------------------------------------


data Route = NowR
           | CounterR
           | ListR
           | AJAXListR
           | NotFoundR


data Action = PageView Route
            | NowAction Now.Action
            | CounterAction Counter.Action
            | ListAction List.Action
            | AJAXListAction AJAXList.Action


match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = NowR <$ end
          <|> NowR <$ lit "now"
          <|> CounterR <$ lit "counter"
          <|> ListR <$ lit "list"
          <|> AJAXListR <$ lit "ajax-list"


type State = {
      nowState :: Now.State
    , counterState :: Counter.State
    , listState :: List.State
    , ajaxListState :: AJAXList.State
    , currentRoute :: Route
    }


initialState :: State
initialState = { nowState: Now.initialState
               , counterState: Counter.initialState
               , listState: List.initialState
               , ajaxListState: AJAXList.initialState
               , currentRoute: NowR
               }


navigation :: Html Action
navigation =
  nav # do
    ul # do
      li # do
        link "/now" # text "Now"
      li # do
        link "/counter" # text "Counter"
      li # do
        link "/list" # text "List"
      li # do
        link "/ajax-list" # text "AJAX List"
  where
    bind = H.bind


--TODO: can probably use some of the state helpers
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (PageView r) s = noEffects (s { currentRoute = r})
update (NowAction a) s =
  let eff = Now.update a s.nowState
  in { state: s { nowState = eff.state}
     , effects: map (map NowAction) eff.effects}
update (CounterAction a) s = noEffects (s { counterState = Counter.update a s.counterState })
update (ListAction a) s = noEffects (s { listState = List.update a s.listState })
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
      navigation
      page s.currentRoute
  where
    page NowR = map NowAction (Now.view s.nowState)
    page CounterR = map CounterAction (Counter.view s.counterState)
    page ListR = map ListAction (List.view s.listState)
    page AJAXListR = map AJAXListAction (AJAXList.view s.ajaxListState)
    page NotFoundR = text "Not found!"
    bind = H.bind
