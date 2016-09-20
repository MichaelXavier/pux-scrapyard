-- | Current location of the ISS
-- use http://api.open-notify.org/iss-now.json
module Components.ISS
    ( State(..)
    , initialState
    , Loc(..)
    , Lat(..)
    , Lon(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Either
import Control.Monad.Aff (attempt)
import Data.Argonaut ((.?), class DecodeJson, decodeJson)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (const, map, (<<<), show, (<>), bind, ($), pure)
import Pux (noEffects, EffModel)
import Pux.Html (button, p, text, div, Html)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      loc :: Maybe Loc
    , status :: String
    }

-------------------------------------------------------------------------------
newtype ISSPos = ISSPos Loc


issLoc :: ISSPos -> Loc
issLoc (ISSPos l) = l


instance decodeJsonISSPos :: DecodeJson ISSPos where
  decodeJson json = do
    obj <- decodeJson json
    pos <- obj .? "iss_position"
    lat <- pos .? "latitude"
    lon <- pos .? "longitude"
    pure (ISSPos { lat: Lat lat, lon: Lon lon })


-------------------------------------------------------------------------------
type Loc = {
      lat :: Lat
    , lon :: Lon
    }


-------------------------------------------------------------------------------
newtype Lat = Lat Number


-------------------------------------------------------------------------------
newtype Lon = Lon Number


-------------------------------------------------------------------------------
initialState :: State
initialState = { loc: Nothing, status: "Not fetched" }


-------------------------------------------------------------------------------
data Action = RequestLoc
            | ReceiveLoc (Either String Loc)


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveLoc (Left e)) s =
  noEffects (s { status = e})
update (ReceiveLoc (Right l)) s = noEffects (s {loc = Just l})
update RequestLoc s = {
      state: s { status = "Fetching"}
    , effects: [ do
      res <- attempt (get "http://api.open-notify.org/iss-now.json")
      let decode reply = decodeJson reply.response :: Either String ISSPos
      let loc = either (Left <<< show) (map issLoc <<< decode) res
      pure (ReceiveLoc loc)
      ]
    }


-------------------------------------------------------------------------------
view :: State -> Html Action
view s =
  div
    []
    [ p
        []
        [ text ("lat: " <> lat)
        ]
    , p
        []
        [ text ("lon: " <> lon)
        ]
    , p
        []
        [ text ("status: " <> s.status)
        ]
    , p
        []
        [ button
            [ onClick (const RequestLoc)
            ]
            [ text "Refresh"
            ]
        ]
    ]
  where
    -- i should probably use a type alias
    lat = case s.loc of
      Just {lat: (Lat n)} -> show n
      Nothing -> "Unknown"
    lon = case s.loc of
      Just {lon: (Lon n)} -> show n
      Nothing -> "Unknown"
