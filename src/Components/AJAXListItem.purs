module Components.AJAXListItem
    ( State(..)
    , fromRawState
    , Status(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Resources.AJAXList as ListR
import Data.Either (Either(Right, Left))
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Network.HTTP.Affjax (AJAX)
import Prelude (map, Unit, ($), const)
import Pux (noEffects, EffModel)
import Pux.Html (input, div, (##), (#), (!), button, text, Html)
import Pux.Html.Attributes (defaultValue)
import Pux.Html.Events (onKeyUp, onClick)
-------------------------------------------------------------------------------


type State = {
      text :: String
    , newText :: Maybe String
    , status :: Status
    , id :: Int
    }

data Action = Delete
            | ReceiveDelete (Either String Unit)
            | Edit String
            | CancelEdit
            | SaveEdit


fromRawState :: ListR.RawListItem -> State
fromRawState (ListR.RawListItem i) = {
      text: i.text
    , newText: Nothing
    , status: ItemCreated
    , id: i.id
    }

--TODO: shold probably gather up the server representation and API calls into a module
toRawState :: State -> ListR.RawListItem
toRawState s = ListR.RawListItem {text: s.text, id: s.id}


data Status = ItemCreated
            | ItemDeleting
            | ItemDeleted
            | ItemDirty



--TODO: ajax to actually delete from server
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveDelete (Left _)) s = noEffects s --TODO: display error
update (ReceiveDelete (Right _)) s = noEffects (s { status = ItemDeleted})
update Delete s = {
    state: s { status = ItemDeleting }
  , effects: [map ReceiveDelete (ListR.deleteItem (toRawState s))]
  }
update (Edit newText) s =
  noEffects (s { newText = Just newText })
update CancelEdit s =
  noEffects (s { newText = Nothing })
--TODO: ajax request ,put into saving
update SaveEdit s@{newText: Just t} = noEffects $
  s { text = t
    , newText = Nothing
    }
update SaveEdit s = noEffects (s { status = ItemDirty })


view :: State -> Html Action
view { status: ItemDeleting } = text "Deleting..."
view s = div ##
  textContent <>
  [ button !
      onClick (const Delete) #
      text "Delete"
  ]
  where
    textContent = maybe displayCurrent displayEdit s.newText
    displayCurrent = [
      text s.text
    , button
        ! onClick (const (Edit s.text))
        # text "Edit"
    ]
    displayEdit newText = [
      editInput newText
    , button
        ! onClick (const CancelEdit)
        # text "Cancel"
    , button
        ! onClick (const SaveEdit)
        # text "Save"
    ]
    editInput newText = input
      [ defaultValue newText
      , onKeyUp (\e -> Edit e.target.value)
      ]
      []
