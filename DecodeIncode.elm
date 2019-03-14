module Main exposing (Code, Model(..), Msg(..), getCode, init, main, update, view)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (..)



-- import Task
-- import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Model


type Model
    = Loading
    | Success Codes
    | Failure --http请求失败
    | ParseError --解析json失败


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getCode
    )



--view


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading"

        Success codes ->
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-4" ] (firstColum codes)

                    -- (getLine codes)
                    , div
                        [ class "col-md-4" ]
                        []
                    , div
                        [ class "col-md-4" ]
                        []
                    ]
                ]

        Failure ->
            text "sorry......"

        ParseError ->
            text "ParseError......"


firstColum codes =
    List.map (\x -> div [] [ span [] [ text x.linecode ], span [] [ text x.address ] ]) codes



--


getLine codes =
    List.map (\x -> span [] [ text x ]) codes



--update


type Msg
    = GetCode (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCode result ->
            case result of
                Ok codeList ->
                    -- TODO 处理字符串-->List
                    case Decode.decodeString codeDecoder codeList of
                        Ok codes ->
                            ( Success codes, Cmd.none )

                        Err message ->
                            -- Debug.toString message
                            ( ParseError, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


type alias Code =
    { linecode : String
    , address : String
    }


type alias Codes =
    List Code


codeDecoder : Decoder Codes
codeDecoder =
    Decode.field "content"
        (Decode.list
            (Decode.map2 Code
                (Decode.field "line" string)
                (Decode.field "address" string)
            )
        )



-- Http


codeUrl =
    "/source/json.txt"


getCode : Cmd Msg
getCode =
    Http.get
        { url = codeUrl
        , expect = Http.expectString GetCode
        }
