module App exposing (..)

import HipstoreUI exposing (Product)
import Html exposing (Html, div, text)
import RemoteData exposing (WebData, isLoading)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.batch []
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



---- VIEW ----


uiConfig : Model -> HipstoreUI.Config Msg
uiConfig model =
    { onAddToCart = \_ -> NoOp
    , onRemoveFromCart = \_ -> NoOp
    , onClickViewCart = NoOp
    , onClickViewProducts = NoOp
    , products = RemoteData.NotAsked
    , cart = RemoteData.NotAsked
    , loadingIndicator = True
    }


view : Model -> Html Msg
view model =
    div []
        [ HipstoreUI.products <| uiConfig model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
