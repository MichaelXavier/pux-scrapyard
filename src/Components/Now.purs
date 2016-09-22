module Components.Now
    ( State(..)
    , initialState
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
import Prelude (const, map, (<<<), show, (<>), ($), pure, bind)
import Pux (noEffects, EffModel)
import Pux.Html ((!), (#), (##), button, p, text, div, Html)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      now :: Maybe String
    , status :: String
    }

-------------------------------------------------------------------------------
newtype NowResp = NowResp String


now :: NowResp -> String
now (NowResp n) = n


instance decodeJsonNowResp :: DecodeJson NowResp where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "now"
    pure (NowResp t)

-------------------------------------------------------------------------------
initialState :: State
initialState = { now: Nothing, status: "Not fetched" }


-------------------------------------------------------------------------------
data Action = RequestNow
            | ReceiveNow (Either String String)


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveNow (Left e)) s =
  noEffects (s { status = e})
update (ReceiveNow (Right t)) s = noEffects (s { now = Just t
                                               , status = "Fetched"})
update RequestNow s = {
      state: s { status = "Fetching"}
    , effects: [ do
      res <- attempt (get "/now.json")
      let decode reply = decodeJson reply.response :: Either String NowResp
      let t = either (Left <<< show) (map now <<< decode) res
      pure (ReceiveNow t)
      ]
    }


-------------------------------------------------------------------------------
-- would like to use the do notation with html but it requires bind to be imported and it conflicts with the one from prelude
view :: State -> Html Action
view s =
  div
    []
    [ p #
        text ("now: " <> currentTime)
    , p #
        text ("status: " <> s.status)
    , p # (button ! onClick (const RequestNow) # text "Refresh")
    ]
  where
    currentTime = case s.now of
      Nothing -> "Unknown"
      Just t -> t
