module Components.AJAXList
    ( State(..)
    , Status(..)
    , initialState
    , Action(..)
    , FAction(..)
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
import Prelude (otherwise, (==), const, pure, show, (<<<), map, bind)
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
--initialState = { items: mempty, status: ListNotFetched }
initialState = { items: M.singleton 0 first, status: ListNotFetched }
  where
    first = {text: "first", newText: Nothing, status: AJAXListItem.ItemCreated, id: 0}


-- data Action = RefreshList
--             | ReceiveList (Either String (M.Map Int AJAXListItem.State))
--             | AJAXItemAction Int AJAXListItem.Action

-- so this only works with ONE CONSTRUCTOR WHAT THE FUCK
-- data Action = AJAXItemAction Int AJAXListItem.Action
-- data Action = AJAXItemAction AJAXListItem.Action
--             | Nop
-- so this works, what about a newtype
--type Action = AJAXListItem.Action
-- this works
-- newtype Action = AJAXItemAction AJAXListItem.Action
-- this works
--data Action = AJAXItemAction AJAXListItem.Action
-- this breaks, irrespective of order
--data Action = Nop | AJAXItemAction AJAXListItem.Action
-- name change doesnt work
data FAction = Nop | AJAXItemAction AJAXListItem.Action
type Action = FAction



mapItemsById
    :: Array AJAXListItem.State
    -> M.Map Int AJAXListItem.State
mapItemsById = M.fromFoldable <<< map toPair
  where
    toPair i = Tuple i.id i


-- update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
-- is it ajax? now lets turn on nop that should break it
-- IS IT EFF THEN? can you not use eff with multi constructor effmodel?
--update :: forall eff. Action -> State -> EffModel State Action eff
-- IS IT THE MOTHERFUCKING ARITY OF THE CONSTRUCTOR? SERIOUSLY WHAT THE FUCK
update :: Action -> State -> State
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
-- TODO: yikes! actually get a failed pattern match where it tries to match a DeleteItem to a AJAXItemAction i DeleteItem, pux bug?
-- update (AJAXItemAction id a) s = noEffects s { items = M.update go id s.items}
-- update (AJAXItemAction id a) s = s { items = M.update go id s.items}
update (AJAXItemAction a) s = s { items = M.update go 0 s.items}
  where
    -- go { status: AJAXListItem.ItemDeleted} = Nothing
    -- go itemState = Just (AJAXListItem.update a itemState)
    -- WHY
    go itemState = go' (AJAXListItem.update a itemState)
    -- this partial match is ok right?
    go' {status: AJAXListItem.ItemDeleted } = Nothing
    go' itemState = Just (AJAXListItem.update a itemState)
    -- go' itemState
    --   | itemState.deleted = Nothing
    --   | otherwise         = Just (AJAXListItem.update a itemState )
update Nop s = s -- is it order dependent?


--SO ITS AJAX, commenting ou t refresh button and the other actions fixes it
--TODO: how do we do an initial load
view :: State -> Html Action
view s = div
  ! className "component"
  # do
    items
    --button ! onClick (const RefreshList) # text "Refresh"
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
        -- map (AJAXItemAction i.id) (AJAXListItem.view i)
        map AJAXItemAction (AJAXListItem.view i)
        -- is it the naming with list?
