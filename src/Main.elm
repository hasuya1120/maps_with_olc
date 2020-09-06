module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html
import Html.Styled as HtmlS
import Html.Styled.Attributes exposing (..)
import Maps
import Maps.Geo
import Maps.Map
import VirtualDom


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { map : Maps.Model Msg }


type Msg
    = MapsMsg (Maps.Msg Msg)


init : ( Model, Cmd Msg )
init =
    let
        skyTree =
            Maps.Geo.latLng 35.710067 139.8085064
    in
    ( { map =
            Maps.defaultModel
                |> Maps.updateMap (Maps.Map.setHeight 600)
                |> Maps.updateMap (Maps.Map.setWidth 1000)
                |> Maps.updateMap (Maps.Map.viewBounds <| Maps.Geo.centeredBounds 14 skyTree)
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapsMsg mapsMsg ->
            model.map
                |> Maps.update mapsMsg
                |> Tuple.mapFirst (\map -> { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapsMsg)



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ mapView model ]


mapView : Model -> VirtualDom.Node Msg
mapView model =
    model.map |> Maps.view |> Maps.mapView MapsMsg |> HtmlS.toUnstyled
