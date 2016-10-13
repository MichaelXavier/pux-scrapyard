module Components.AJAXListItem
    ( State(..)
    , RawState(..)
    , fromRawState
    , Status(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Argonaut ((.?), decodeJson, class DecodeJson)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Network.HTTP.Affjax (AJAX)
import Prelude (const, pure, bind)
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

newtype RawState = RawState {
      text :: String
    , id :: Int
    }


data Action = Delete
            | Edit String
            | CancelEdit
            | SaveEdit


instance decodeJsonRawAJAXListItem :: DecodeJson RawState where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "text"
    i <- obj .? "id"
    pure (RawState {text: t, id: i})


fromRawState :: RawState -> State
fromRawState (RawState i) = {
      text: i.text
    , newText: Nothing
    , status: ItemCreated
    , id: i.id
    }


data Status = ItemCreated
            | ItemDeleting
            | ItemDeleted



--TODO: ajax to actually delete from server
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update Delete i =
  noEffects (i { status = ItemDeleting })
update (Edit newText) s =
  noEffects (s { newText = Just newText })
update CancelEdit s =
  noEffects (s { newText = Nothing })
--TODO: ajax request ,put into saving
update SaveEdit s@{newText: Just t} = noEffects
  s { text = t
    , newText = Nothing
    }
update SaveEdit s =
  noEffects s


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

