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
           | NotFoundR


data Action = PageView Route
            | NowAction Now.Action
            | CounterAction Counter.Action
            | ListAction List.Action


match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = NowR <$ end
          <|> NowR <$ lit "now"
          <|> CounterR <$ lit "counter"
          <|> ListR <$ lit "list"


type State = {
      nowState :: Now.State
    , counterState :: Counter.State
    , listState :: List.State
    , currentRoute :: Route
    }


initialState :: State
initialState = { nowState: Now.initialState
               , counterState: Counter.initialState
               , listState: List.initialState
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
  where
    bind = H.bind

update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (PageView r) s = noEffects (s { currentRoute = r})
update (NowAction a) s =
  let eff = Now.update a s.nowState
  in { state: s { nowState = eff.state}
     , effects: map (map NowAction) eff.effects}
update (CounterAction a) s = noEffects (s { counterState = Counter.update a s.counterState })
update (ListAction a) s = noEffects (s { listState = List.update a s.listState })


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
    page NotFoundR = text "Not found!"
    bind = H.bind
