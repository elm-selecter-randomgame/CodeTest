port module Main exposing (Address, Content, Model, Msg(..), RightCode, codeDecoder, dismantling, init, lowerCase, main, rightDecode, stringtoChar, subscriptions, toElm, toJs, tranMaybeInt, transitionList, update, view)

-- module Main exposing (Address, Content, Model, Msg(..), RightCode, codeDecoder, dismantling, init, jsonBody, main, rightDecode, stringtoChar, subscriptions, tranMaybeInt, transitionList, update, view)
-- import Markdown exposing (defaultOptions)

import Array exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser exposing (..)
import Html.Parser.Util exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Markdown exposing (..)
import Regex exposing (..)
import Url exposing (..)
import Url.Builder as UB
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


port toJs : String -> Cmd msg


port toElm : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ toElm ToElm ]



--{address : []}


codeDecoder : JD.Decoder Address
codeDecoder =
    JD.field "address" (JD.list JD.string)



--{content : "", fileName : ""}


rightDecode : JD.Decoder RightCode
rightDecode =
    JD.map2 RightCode
        (JD.field "content" JD.string)
        (JD.field "fileName" JD.string)



--Model


type alias Address =
    List String


type alias Content =
    List String


type alias RightCode =
    { content : String
    , fileName : String
    }


type alias Model =
    { leftDiv : String
    , centerDiv : Address
    , rightDiv : RightCode
    , onClickLine : String
    , changeUrl : String --储存更改的url
    , key : Nav.Key --更改url必须的key
    , url : Url.Url
    , highlightCode : String --接收从js端传过来的解析好的html string
    , testLine : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { leftDiv = ""
      , centerDiv = []
      , rightDiv = { content = "", fileName = "" }
      , onClickLine = ""
      , testLine = ""
      , changeUrl = ""
      , key = key
      , url = url
      , highlightCode = ""
      }
    , Http.post
        { url = "/init"
        , body = Http.emptyBody
        , expect = Http.expectString GetWord
        }
    )



--import function


lowerCase : Regex.Regex
lowerCase =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\n"



--update


type Msg
    = Display
    | GetWord (Result Http.Error String)
    | GetIndex Int
    | GetCodeIndex Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TransiForAddress String
    | GetRightWord (Result Http.Error String)
      -- | ToJs String
    | ToElm String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Display ->
            ( model, Cmd.none )

        GetWord result ->
            case result of
                Ok fullText ->
                    case JD.decodeString codeDecoder fullText of
                        Ok listContent ->
                            ( { model | centerDiv = listContent }, Cmd.none )

                        Err _ ->
                            ( { model | centerDiv = [] }, Cmd.none )

                Err _ ->
                    ( { model | centerDiv = [] }, Cmd.none )

        GetRightWord result ->
            case result of
                Ok fullText ->
                    case JD.decodeString rightDecode fullText of
                        Ok content ->
                            ( { model | rightDiv = content }, toJs content.content )

                        Err _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetIndex index ->
            ( { model | onClickLine = String.fromInt (index + 1) }, Cmd.none )

        GetCodeIndex index ->
            ( { model | testLine = String.fromInt (index + 1) }, Cmd.none )

        UrlChanged url ->
            ( if String.contains "?" (Url.toString model.url) then
                { model
                    | url =
                        case
                            Url.fromString
                                (UB.relative
                                    [ String.left
                                        (tranMaybeInt (List.head (String.indexes "?" (Url.toString model.url))))
                                        (Url.toString model.url)
                                    ]
                                    []
                                )
                        of
                            Just transUrl ->
                                transUrl

                            Nothing ->
                                model.url
                }

              else
                { model
                    | url =
                        case
                            Url.fromString
                                (UB.relative [ Url.toString model.url ]
                                    []
                                )
                        of
                            Just transUrl ->
                                transUrl

                            Nothing ->
                                model.url
                }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Cmd.none )

        TransiForAddress address ->
            ( model
            , Http.post
                { url = "/code"
                , body = Http.multipartBody [ Http.stringPart "address" address ]
                , expect = Http.expectString GetRightWord
                }
            )

        -- ToJs message ->
        --   (model,toJs message)
        ToElm message ->
            ( { model | highlightCode = message }, Cmd.none )



--把穿过来的highlightCode进行去bug处理


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


replaceBug : String -> String
replaceBug string =
    userReplace "\n</span>" (\_ -> "</span>\n") string



--把maybe类型的数字转化为int型


tranMaybeInt string =
    Maybe.withDefault 1000 string


view : Model -> Browser.Document Msg
view model =
    { title = "post request and code highlight"
    , body =
        [ div [ class "total" ]
            [ div [ class "leftDiv" ] [ text model.leftDiv ]
            , div [ class "centerDiv", contenteditable True ]
                [ Html.pre [] (dismantling model)
                , button [ class "transition" ] [ text "转化" ]

                -- , div [ class "attribute" ] [ text ("get line by onClick：" ++ model.onClickLine) ]
                ]
            , div [ class "rightDiv" ]
                [ pre [ class "hljs" ] (lineNumber model.highlightCode)
                ]
            , div []
                [ text ("get line by onClick:" ++ model.testLine) ]
            ]
        ]
    }



--把获得的高亮好的代码通过\n拆分成数组


caseHighlightCode code =
    Regex.split lowerCase code


lineNumber listCode =
    List.map
        (\x ->
            div [ onClick (GetCodeIndex (Tuple.first x)) ]
                (case run (Tuple.second x) of
                    Ok html ->
                        toVirtualDom html

                    Err _ ->
                        [ text (Tuple.second x) ]
                )
        )
        (transitionCodeList listCode)


transitionCodeList listCode =
    List.indexedMap Tuple.pair (caseHighlightCode (replaceBug listCode))



--拆解获取的code每一行显示到<div>中


dismantling : Model -> List (Html Msg)
dismantling model =
    List.map
        (\x ->
            div [ onClick (GetIndex (Tuple.first x)) ]
                [ a [ onClick (TransiForAddress (Tuple.second x)) ] [ text (Tuple.second x) ] ]
        )
        (transitionList model)



--获取的address字符串转化为全部为小写的字母Array


stringtoChar x =
    Array.fromList (List.map (\a -> Char.toLower a) (String.toList x))



--为把http获取的txt文本转化为List->元组List（添加index）


transitionList model =
    List.indexedMap Tuple.pair model.centerDiv
