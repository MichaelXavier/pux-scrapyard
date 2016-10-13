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
import Control.Alt ((<|>))
import Data.Functor ((<$), map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Network.HTTP.Affjax (AJAX)
import Prelude (pure)
import Pux (mapEffects, mapState, EffModel, noEffects)
import Pux.Html ((##), text, li, ul, nav, (#), div, (!), Html)
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
    matcher = defRoute <$ end
          <|> NowR <$ lit "now"
          <|> CounterR <$ lit "counter"
          <|> ListR <$ lit "list"
          <|> AJAXListR <$ lit "ajax-list"
    defRoute = AJAXListR


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
               ,ajaxListState: AJAXList.initialState
               , currentRoute: NowR
               }

-- can i use # for single item trees
navigation :: Html Action
navigation =
  nav #
    ul ##
      [ li #
          link "/now" # text "Now"
      , li #
          link "/counter" # text "Counter"
      , li #
          link "/list" # text "List"
      , li #
          link "/ajax-list" # text "AJAX List"
      ]


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (PageView r) s =
  { state: s { currentRoute = r}
    -- refresh the initial state on nav. note that this could lose data in the ajax list case so it may not be a good idea
  , effects: case r of
     AJAXListR -> [pure (AJAXListAction AJAXList.RefreshList)]
     NowR -> [pure (NowAction Now.RequestNow)]
     _ -> mempty
  }
update (NowAction a) s =
  mapState (\ns -> s { nowState = ns}) (mapEffects NowAction (Now.update a s.nowState))
update (CounterAction a) s = noEffects (s { counterState = Counter.update a s.counterState })
update (ListAction a) s = noEffects (s { listState = List.update a s.listState })
update (AJAXListAction a) s =
  mapState (\as -> s { ajaxListState = as})
           (mapEffects AJAXListAction (AJAXList.update a s.ajaxListState))


--TODO: maybe request data on page change?
view :: State -> Html Action
view s =
  div !
    className "component" ##
    [ navigation
    , page s.currentRoute
    ]
  where
    page NowR = map NowAction (Now.view s.nowState)
    page CounterR = map CounterAction (Counter.view s.counterState)
    page ListR = map ListAction (List.view s.listState)
    page AJAXListR = map AJAXListAction (AJAXList.view s.ajaxListState)
    page _ = text "Not found!"
