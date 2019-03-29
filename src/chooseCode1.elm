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

main : Program () Model Msg
main =
  Browser.application{init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  , onUrlRequest = LinkClicked
  , onUrlChange = UrlChanged
  }

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
  , key : Nav.Key --更改url必须的key
  , url : Url.Url
  }

{-
type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }
-}

init : () -> Url.Url -> Nav.Key -> (Model,Cmd Msg)
init flags url key=
  ({
  leftDiv = ""
  ,centerDiv = []
  ,rightDiv = ""
  ,onClickLine = ""
  ,changeUrl = "/fileSpace/transitionWord.json"
  ,key = key
  ,url = url}
  ,Http.get
      { url = "/fileSpace/transitionWord.json" --本地json文件
      , expect = Http.expectString GetWord})



--update
type Msg =
          Display
        | GetWord (Result Http.Error String)
        | GetIndex Int
        -- | ChangeUrl String
        | LinkClicked Browser.UrlRequest
        | UrlChanged Url.Url

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
    UrlChanged url ->
      (if String.contains "?" (Url.toString model.url) then
      {model | url = Just (Url.fromString (UB.relative [(String.left
          (tranMaybeInt (List.head (String.indexes "?" (Url.toString model.url))))
          (Url.toString model.url))] [UB.string "address"
          (case transitionHex (Url.toString url) of
            Ok number ->
              String.fromInt number
            Err _ ->
              "cuowu")]))}
              else
        {model | url = Just (Url.fromString (UB.relative [(Url.toString model.url)]
      [UB.string "address" (case transitionHex (Url.toString url) of
        Ok number ->
          String.fromInt number
        Err _ ->
          "cuowu")]))},Cmd.none)
    LinkClicked urlRequest ->
        case urlRequest of
          Browser.Internal url ->
            (model
            ,Nav.pushUrl model.key (Url.toString url))
          Browser.External url ->
            (model
            ,Nav.load url)
    -- UrlChanged url ->
    --   ( { model | url = url }
    --   , Cmd.none
    --   )



--把maybe类型的数字转化为int型
tranMaybeInt string =
  Maybe.withDefault 1000 string
--把string类型的url转化为Url
tranMaybeString string =
  Maybe.withDefault Url.Url string

view : Model -> Browser.Document Msg
view model =

  {title = "URL change"
  ,body =
    [  div [class "total"] [
       div [class "leftDiv"] [text model.leftDiv]
      ,div [class "centerDiv",contenteditable True] [
        Html.pre [] (dismantling model)
      ,button [onClick Display, class "transition"] [text "转化"]
      ,div [class "attribute"] [text ("get line by onClick："++ model.onClickLine)]]

      ,div [class "rightDiv"] [text (Url.toString model.url)]
      ]
    ]

  }

--拆解获取的code每一行显示到<div>中
dismantling : Model -> List (Html Msg)
dismantling model =
  List.map (\x -> div [onClick (GetIndex (Tuple.first x))]
   [
   --onClick (ChangeUrl ((Tuple.second x).address))
  a [ href ((Tuple.second x).address) ] [text (case transitionHex ((Tuple.second x).address) of
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
