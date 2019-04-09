--module Main exposing (Action(..), Model, getPanel, init, main, update, view)
-- import List.Extra exposing (getAt, removeAt)


module Main exposing (Codes, Model, Msg(..), dismantling, getContent, init, main, subscriptions, transitionList, update, view)

import Array exposing (..)
import Browser
import Hex exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (..)
import Markdown
import Random
import Random.List exposing (..)
import SyntaxHighlight exposing (elm, monokai, toInlineHtml, useTheme)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Model


type alias Codes =
    List Code


type alias Code =
    { linecode : String
    , address : String
    }


type alias Model =
    { leftDiv : String, centerDiv : Codes, rightDiv : String, onClickLine : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { leftDiv = ""
      , centerDiv = []
      , rightDiv = ""
      , onClickLine = ""
      }
    , Http.get
        { url = "transitionWord.json" --本地json文件
        , expect = Http.expectString GetWord
        }
    )



-- getContent =
--   Json.Decode.string "\"content\""
--update


type Msg
    = Display
    | GetWord (Result Http.Error String)
    | GetIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Display ->
            ( model, Cmd.none )

        GetWord result ->
            case result of
                Ok codeList ->
                    case Decode.decodeString codeDecoder codeList of
                        Ok codes ->
                            ( { model | centerDiv = codes }, Cmd.none )

                        Err _ ->
                            ( { model | centerDiv = [] }, Cmd.none )

                Err _ ->
                    ( { model | centerDiv = [] }, Cmd.none )

        GetIndex index ->
            ( { model | onClickLine = String.fromInt (index + 1) }, Cmd.none )


codeDecoder : Decoder Codes
codeDecoder =
    Decode.field "content"
        (Decode.list
            (Decode.map2 Code
                (Decode.field "line" string)
                (Decode.field "address" string)
            )
        )



--String.fromInt将整数转换为字符串


getContent string =
    Decode.list string


view : Model -> Html Msg
view model =
    div [ class "total" ]
        [ div [ class "leftDiv" ] [ text model.leftDiv ]
        , div [ class "centerDiv", contenteditable True ]
            --  contenteditable指示元素的内容是否可编辑
            [ --pre [] (dismantling model)
              -- textarea [ class "x86 Assembly", id "code" ] (dismantling model)
              pre [ class "line-numbers" ]
                [ code [ class "lang-asm6502", id "some-code" ]
                    [ span []
                        (dismantling model)
                    ]
                ]

            --pre [] [ code [] (dismantling model) ]
            --info
            --  pre表示其内容已预先格式化，并且必须保留此格式
            , button [ onClick Display, class "transition" ] [ text "转化" ]
            , div [ class "attribute" ] [ text ("get line by onClick：" ++ model.onClickLine) ]
            ]
        , div [ class "rightDiv" ] [ text model.rightDiv ]
        , p []
            --例子
            [ useTheme monokai
            , elm "isEmpty : String -> Bool"
                |> Result.map toInlineHtml
                |> Result.withDefault
                    (code [] [ text "isEmpty : String -> Bool" ])
            ]
        ]



--例子：


myCode : Html msg
myCode =
    Markdown.toHtml []
        """
  '''elm
  view =
    Atom.row
      [basicBlock
      ]
  '''
    """



--拆解获取的code每一行显示到<div>中


dismantling : Model -> List (Html Msg)
dismantling model =
    List.map
        (\x ->
            div []
                [ span [ onClick (GetIndex (Tuple.first x)) ] [ text (Tuple.second x).linecode ]
                , span [] [ text "  " ]
                , span [ onClick (GetIndex (Tuple.first x)) ]
                    [ text
                        --十六进制转二进制方法一
                        --String.dropLeft 2 还可以换成String.slice 2 9
                        (case Hex.fromString (String.toLower (String.dropLeft 2 (Tuple.second x).address)) of
                            Ok result ->
                                String.fromInt result

                            Err _ ->
                                "ERR"
                        )

                    --方法二
                    -- (case conversion (Tuple.second x).address of
                    --     Ok result ->
                    --         String.fromInt result
                    --
                    --     Err _ ->
                    --         "Err"
                    -- )
                    ]
                ]
        )
        (transitionList model)



-- conversion x =
--     if (String.fromChar (Maybe.withDefault '0' (Array.get 0 (stringtoChar x))) ++ String.fromChar (Maybe.withDefault 'x' (Array.get 1 (stringtoChar x)))) == "0x" then
--         Hex.fromString (String.fromList (Array.toList (Array.set 1 '0' (stringtoChar x))))
--
--     else
--         Hex.fromString x
--
--
-- stringtoChar x =
--     Array.fromList (List.map (\a -> Char.toLower a) (String.toList x))
--Tuple.first从元组中提取第一个值
--为把http获取的txt文本转化为List->元组List


transitionList model =
    List.indexedMap Tuple.pair model.centerDiv



-- conversion : String -> Result String Int
-- conversion x =
--     case Hex.fromString (String.slice 2 9 (Tuple.second x).address) of
--         Ok result ->
--             { model | centerDiv = result }
--
--         Err _ ->
--             { model | centerDiv = [] }
--List.indexedMap Tuple.pair与map相同，但该函数也应用于每个元素的索引（从零开始）
--indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
--对中间代码的更改-5+93
-- transitionLeft : Model -> String
-- transitionLeft model =
--   String.reverse model.centerDiv
-- transitionRight : Model -> String
-- transitionRight model =
--   String.repeat 2 model.centerDiv
