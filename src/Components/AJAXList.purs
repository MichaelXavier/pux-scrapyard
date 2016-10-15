module Components.AJAXList
    ( State(..)
    , Status(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Components.AJAXListItem as AJAXListItem
import Data.Array as A
import Data.Map as M
import Resources.AJAXList as ListR
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude (map, (<<<))
import Pux (mapState, mapEffects, EffModel, noEffects)
import Pux.Html (Html, div, li, ol, text, (##), (!), (#))
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------


type State = {
      items :: M.Map Int AJAXListItem.State
    , status :: Status
    }

data Status = ListNotFetched
            | ListFetching
            | ListFetched
            | ListFetchError String


initialState :: State
initialState = { items: mempty, status: ListNotFetched }


data Action = RefreshList
            | ReceiveList (Either String (M.Map Int AJAXListItem.State))
            | AJAXItemAction Int AJAXListItem.Action


mapItemsById
    :: Array AJAXListItem.State
    -> M.Map Int AJAXListItem.State
mapItemsById = M.fromFoldable <<< map toPair
  where
    toPair i = Tuple i.id i


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveList (Left e)) s =
  noEffects (s { status = ListFetchError e})
update (ReceiveList (Right items)) s =
  noEffects (s { items = items, status = ListFetched})
update RefreshList s = {
      state: s { status = ListFetching }
    , effects: [map (ReceiveList <<< map (mapItemsById <<< map AJAXListItem.fromRawState)) ListR.getList]
    }
-- TODO: we'll be responsible for saving
update (AJAXItemAction id a) s = case M.lookup id s.items of
  Just itemState -> mapState updateItem (mapEffects (AJAXItemAction id) (AJAXListItem.update a itemState))
  Nothing -> noEffects s
  where
    updateItem { status: AJAXListItem.ItemDeleted} =
      s { items = M.delete id s.items }
    updateItem itemState =
      s { items = M.insert id itemState s.items }

view :: State -> Html Action
view s = div !
  className "component" ##
  items
  where
    items = case s.status of
      ListNotFetched -> [text "Not fetched"]
      ListFetching -> [ text "Fetching"
                      , items'
                      ]
      ListFetched -> [items']
      ListFetchError e -> [ text ("Error: " <> e)
                          , items'
                          ]
    items' = ol
      []
      (A.fromFoldable (map viewItem' (M.values s.items)))
    viewItem' i = li #
      map (AJAXItemAction i.id) (AJAXListItem.view i)
