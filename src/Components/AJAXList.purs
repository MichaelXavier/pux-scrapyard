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
import Pux.Html as H
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)
import Data.Either (Either(Right, Left), either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (const, pure, show, (<<<), map, bind)
import Pux (noEffects, EffModel)
import Pux.Html (button, text, (!), li, ol, (#), div, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
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
-- initialState = { items: mempty, status: ListNotFetched }
initialState = { items: M.singleton 0 (AJAXListItem.initialState 0 "sample"), status: ListNotFetched }


-- data Action = RefreshList
--             | ReceiveList (Either String (M.Map Int AJAXListItem.State))
--             | ItemAction Int AJAXListItem.Action

data Action = ItemAction Int AJAXListItem.Action



mapItemsById
    :: Array AJAXListItem.State
    -> M.Map Int AJAXListItem.State
mapItemsById = M.fromFoldable <<< map toPair
  where
    toPair i = Tuple i.id i


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
-- update (ReceiveList (Left e)) s =
--   noEffects (s { status = ListFetchError e})
-- update (ReceiveList (Right items)) s =
--   noEffects (s { items = items, status = ListFetched})
-- update RefreshList s = {
--       state: s { status = ListFetching }
--     , effects: [do
--       res <- attempt (get "/items.json")
--       let decode reply = decodeJson reply.response
--       let t = either (Left <<< show) (map (mapItemsById <<< (map AJAXListItem.fromRawState)) <<< decode) res
--       pure (ReceiveList t)
--       ]
--     }
-- TODO: yikes! actually get a failed pattern match where it tries to match a DeleteItem to a ItemAction i DeleteItem, pux bug?
update (ItemAction id a) s = noEffects s { items = M.update updateItem' id s.items}
  where
    updateItem' { status: AJAXListItem.ItemDeleted} = Nothing
    updateItem' itemStatus = Just (AJAXListItem.update a itemStatus)


--TODO: how do we do an initial load
view :: State -> Html Action
view s = div
  ! className "component"
  # do
    items
    -- button ! onClick (const RefreshList) # text "Refresh"
  where
    bind = H.bind
    items = case s.status of
      --ListNotFetched -> text "Not fetched"
      ListNotFetched -> items'
      ListFetching -> do
        text "Fetching"
        items'
      ListFetched -> items'
      ListFetchError e -> do
        text ("Error: " <> e)
        items'
    items' = ol
      []
      (A.fromFoldable (map viewItem' (M.values s.items)))
    viewItem' i = li
      # do
        map (ItemAction i.id) (AJAXListItem.view i)
