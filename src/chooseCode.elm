--module Main exposing (Action(..), Model, getPanel, init, main, update, view)

-- import List.Extra exposing (getAt, removeAt)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Http
import Random
import Random.List exposing (..)
import Json.Decode as JD
import Hex exposing (..)
import Array exposing (..)
import Url exposing (..)
import Url.Builder as UB


main =
  Browser.application{init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  , onUrlRequest = update
  , onUrlChange = update}

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


--解析json数据把json中的content数组解成elm中的[{linecode = "",address = ""}..]
codeDecoder : JD.Decoder Codes
codeDecoder =
    JD.field "content"
        (JD.list
            (JD.map2 Code
                (JD.field "line" JD.string)
                (JD.field "address" JD.string)
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
  {leftDiv : String
  , centerDiv : Codes
  , rightDiv : String
  , onClickLine : String
  , changeUrl : String --储存更改的url
  }


init : () -> (Model,Cmd Msg)
init _=
  ({
  leftDiv = ""
  ,centerDiv = []
  ,rightDiv = ""
  ,onClickLine = ""
  ,changeUrl = "/fileSpace/transitionWord.json"}
  ,Http.get
      { url = "/fileSpace/transitionWord.json" --本地json文件
      , expect = Http.expectString GetWord})



--update
type Msg =
          Display
        | GetWord (Result Http.Error String)
        | GetIndex Int
        | ChangeUrl String
        | ClickedLink Browser.UrlRequest

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
  case msg of
    Display ->
      (model,Cmd.none)
    GetWord result ->
      case result of
        Ok fullText ->
          case JD.decodeString codeDecoder fullText of
          Ok listContent ->
            ({model | centerDiv = listContent},Cmd.none)
          Err _ ->
            ({model | centerDiv = []},Cmd.none)
        Err _ ->
          ({model | centerDiv = []},Cmd.none)
    GetIndex index ->
      ({model | onClickLine = (String.fromInt (index+1))},Cmd.none)
    ChangeUrl url ->
      (if String.contains "?" model.changeUrl then
      {model | changeUrl = UB.relative [(String.left
          (tranMaybe (List.head (String.indexes "?" model.changeUrl)))
          model.changeUrl)] [UB.string "address"
          (case transitionHex url of
            Ok number ->
              String.fromInt number
            Err _ ->
              "cuowu")]}
              else
        {model | changeUrl = UB.relative [model.changeUrl]
      [UB.string "address" (case transitionHex url of
        Ok number ->
          String.fromInt number
        Err _ ->
          "cuowu")]},Cmd.none)
    ClickedLink urlRequest ->
        case urlRequest of
          Internal url ->
            (model
            ,Nav.pushUrl model.key (Url.toString url))
          External url ->
            (model
            ,Nav.load url)

getContent string =
  JD.list string

--把maybe类型的数字转化为int型
tranMaybe string =
  Maybe.withDefault 1000 string

view : Model -> Html Msg
view model =
  div [class "total"] [
   div [class "leftDiv"] [text model.leftDiv]
  ,div [class "centerDiv",contenteditable True] [
    Html.pre [] (dismantling model)
  ,button [onClick Display, class "transition"] [text "转化"]
  ,div [class "attribute"] [text ("get line by onClick："++ model.onClickLine)]]

  ,div [class "rightDiv"] [text model.changeUrl]
  ]

--拆解获取的code每一行显示到<div>中
dismantling : Model -> List (Html Msg)
dismantling model =
  List.map (\x -> div [onClick (GetIndex (Tuple.first x))]
   [
  span [onClick (ChangeUrl ((Tuple.second x).address)) ] [text (case transitionHex ((Tuple.second x).address) of
    Ok number ->
      String.fromInt number
    Err _ ->
      "cuowu")],
  span [ style "display" "inline-block", style "width"  "20px" ] [],
  span [] [text ((Tuple.second x).linecode)]
        ]) (transitionList model)

--获取的address字符串转化为全部为小写的字母Array
stringtoChar x =
  Array.fromList (List.map (\a -> Char.toLower a) (String.toList x ))

--x代表address默认传进来的数就是16进制数，把String的address转化10进制的int
transitionHex x =
  if ((String.fromChar (Maybe.withDefault '0' (Array.get 0 (stringtoChar x)))) ++
    (String.fromChar (Maybe.withDefault 'x' (Array.get 1 (stringtoChar x))))) == "0x"
        then Hex.fromString (String.fromList (Array.toList (Array.set 1 '0' (stringtoChar x))))
        else Hex.fromString x

--为把http获取的txt文本转化为List->元组List（添加index）
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
