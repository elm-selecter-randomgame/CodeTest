--module Main exposing (Action(..), Model, getPanel, init, main, update, view)

-- import List.Extra exposing (getAt, removeAt)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Http
import Random
import Random.List exposing (..)
import Json.Decode exposing (..)
import Hex exposing (..)
import Array exposing (..)


main =
  Browser.element{init = init, update = update, view = view, subscriptions = subscriptions}

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
-- point : Decoder Point
-- point =
--   map2 Point
--   (field "address" Json.Decode.string)
--   (field "line" Json.Decode.string)

--解析json数据把json中的content数组解成elm中的[{linecode = "",address = ""}..]
codeDecoder : Decoder Codes
codeDecoder =
    Json.Decode.field "content"
        (Json.Decode.list
            (Json.Decode.map2 Code
                (Json.Decode.field "line" string)
                (Json.Decode.field "address" string)
            )
        )

--Model
type alias Code =
    { linecode : String
    , address : String
    }
type alias Codes =
    List Code
type alias Content = List String
type alias Model =
  {leftDiv : String, centerDiv : Codes, rightDiv : String, onClickLine : String}


init : () -> (Model,Cmd Msg)
init _=
  ({
  leftDiv = ""
  ,centerDiv = []
  ,rightDiv = ""
  ,onClickLine = ""}
  ,Http.get
      { url = "/fileSpace/transitionWord.json" --本地json文件
      , expect = Http.expectString GetWord
      })


-- getContent =
--   Json.Decode.string "\"content\""

--update
type Msg =
          Display
        | GetWord (Result Http.Error String)
        | GetIndex Int

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
  case msg of
    Display ->
      (model,Cmd.none)
    GetWord result ->
      case result of
        Ok fullText ->
          case decodeString codeDecoder fullText of
          Ok listContent ->
            ({model | centerDiv = listContent},Cmd.none)
          Err _ ->
            ({model | centerDiv = []},Cmd.none)
        Err _ ->
          ({model | centerDiv = []},Cmd.none)
    GetIndex index ->
      ({model | onClickLine = (String.fromInt (index+1))},Cmd.none)

getContent string =
  Json.Decode.list string

view : Model -> Html Msg
view model =
  div [class "total"] [
   div [class "leftDiv"] [text model.leftDiv]
  ,div [class "centerDiv",contenteditable True] [
    Html.pre [] (dismantling model)
  ,button [onClick Display, class "transition"] [text "转化"]
  ,div [class "attribute"] [text ("get line by onClick："++ model.onClickLine)]]

  ,div [class "rightDiv"] [text model.rightDiv]
  ]

--拆解获取的code每一行显示到<div>中
dismantling : Model -> List (Html Msg)
dismantling model =
  List.map (\x -> div [onClick (GetIndex (Tuple.first x))]
   [
   --  span [] [text ((Tuple.second x).address)]
   -- ,span [] [text ((Tuple.second x).line)]
  -- (case decodeString (field "address" Json.Decode.string) (Tuple.second x) of
  --     Ok address ->
  --       span [] [text (address)]
  --     Err _ ->
  --       span [] [text "解读错误2"]),
  -- (case decodeString (field "line" Json.Decode.string) (Tuple.second x) of
  --     Ok content ->
  --       span [] [text content]
  --     Err _ ->
  --       span [] [text "解读错误1"])
  span [] [text (case transitionHex ((Tuple.second x).address) of
    Ok number ->
      String.fromInt number
    Err _ ->
      "cuowu")],
  span [ style "display" "inline-block", style "width"  "20px" ] [],
  span [] [text ((Tuple.second x).linecode)]
    -- (case decodeString point (Tuple.second x) of
    --   Ok lines ->
    --     span [] [text lines.line]
    --   Err _ ->
    --     span [] [text "jieducuowo"])
        ]) (transitionList model)

--获取的address字符串转化为全部为小写的字母Array
stringtoChar x =
  Array.fromList (List.map (\a -> Char.toLower a) (String.toList x ))

--x代表address默认传进来的数就是16进制数
transitionHex x =
  if ((String.fromChar (Maybe.withDefault '0' (Array.get 0 (stringtoChar x)))) ++
    (String.fromChar (Maybe.withDefault 'x' (Array.get 1 (stringtoChar x))))) == "0x"
        then Hex.fromString (String.fromList (Array.toList (Array.set 1 '0' (stringtoChar x))))
        else Hex.fromString x

--为把http获取的txt文本转化为List->元组List
transitionList model =
  List.indexedMap Tuple.pair model.centerDiv

--对中间代码的更改
-- transitionLeft : Model -> String
-- transitionLeft model =
--   String.reverse model.centerDiv
--
-- transitionRight : Model -> String
-- transitionRight model =
--   String.repeat 2 model.centerDiv
