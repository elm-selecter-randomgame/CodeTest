--module Main exposing (Action(..), Model, getPanel, init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Array exposing (..)
import Url exposing (..)
import Url.Builder as UB
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string )
import Debug

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
codeDecoder : JD.Decoder Address
codeDecoder =
    JD.field "address" (JD.list JD.string)

rightDecode : JD.Decoder RightCode
rightDecode =
  JD.map2 RightCode
    (JD.field "content" JD.string)
    (JD.field "fileName" JD.string)

--Model
type alias Address = List String
type alias Content = List String
type alias RightCode =
  {content : String
  ,fileName : String
 }
type alias Model =
  {leftDiv : String
  , centerDiv : Address
  , rightDiv : RightCode --右边的代码
  , onClickLine : String
  , changeUrl : String --储存更改的url
  , key : Nav.Key --更改url必须的key
  , url : Url.Url
  }


init : () -> Url.Url -> Nav.Key -> (Model,Cmd Msg)
init flags url key=
  ({
  leftDiv = ""
  ,centerDiv = []
  ,rightDiv = {content = "",fileName = ""}
  ,onClickLine = ""
  ,changeUrl = ""
  ,key = key
  ,url = url}
  ,Http.post
      { url = "/init"
      , body = Http.emptyBody
      , expect = Http.expectString GetWord})

--update
type Msg =
          Display
        | GetWord (Result Http.Error String)
        | GetIndex Int
        | LinkClicked Browser.UrlRequest
        | UrlChanged Url.Url
        | TransiForAddress String
        | GetRightWord (Result Http.Error String)

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
    GetRightWord result ->
      case result of
        Ok fullText ->
          case JD.decodeString rightDecode fullText of
            Ok content ->
              ({model | rightDiv = content},Cmd.none)
            Err _ ->
              (model,Cmd.none)
        Err _ ->
          (model,Cmd.none)
    GetIndex index ->
      ({model | onClickLine = (String.fromInt (index+1))},Cmd.none)
    UrlChanged url ->
      (if String.contains "?" (Url.toString model.url) then
      {model | url = (case (Url.fromString (UB.relative [(String.left
          (tranMaybeInt (List.head (String.indexes "?" (Url.toString model.url))))
          (Url.toString model.url))] [])) of
              Just transUrl -> transUrl
              Nothing -> model.url)}
              else
        {model | url = case (Url.fromString (UB.relative [(Url.toString model.url)]
      [])) of
            Just transUrl -> transUrl
            Nothing -> model.url},Cmd.none)
    LinkClicked urlRequest ->
        case urlRequest of
          Browser.Internal url ->
            (model,Cmd.none)
          Browser.External href ->
            (model
            ,Cmd.none)
    TransiForAddress address ->
       (model,
      Http.post{
      url = "/code"
      ,body = Http.multipartBody [Http.stringPart "address"  address]
      ,expect = Http.expectString GetRightWord
      })

jsonBody : JE.Value -> Http.Body
jsonBody value =
  Http.jsonBody value

--把maybe类型的数字转化为int型
tranMaybeInt string =
  Maybe.withDefault 1000 string

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

      ,div [class "rightDiv"] [
      pre [] [
         div [] [text ((model.rightDiv).content)]
        ,div [] [text ((model.rightDiv).fileName)]
          ]
        ]
      ]
    ]

  }

--拆解获取的code每一行显示到<div>中
dismantling : Model -> List (Html Msg)
dismantling model =
  List.map (\x -> div [onClick (GetIndex (Tuple.first x))]
   [ a [onClick (TransiForAddress (Tuple.second x) )  ] [ text (Tuple.second x) ] ]) (transitionList model)

--href (Tuple.second x),onClick (ChangeUrl (Tuple.second x))

--获取的address字符串转化为全部为小写的字母Array
stringtoChar x =
  Array.fromList (List.map (\a -> Char.toLower a) (String.toList x ))


--x代表address默认传进来的数就是16进制数，把String的address转化10进制的int
-- transitionHex x =
--   if ((String.fromChar (Maybe.withDefault '0' (Array.get 0 (stringtoChar x)))) ++
--     (String.fromChar (Maybe.withDefault 'x' (Array.get 1 (stringtoChar x))))) == "0x"
--         then Hex.fromString (String.fromList (Array.toList (Array.set 1 '0' (stringtoChar x))))
--         else Hex.fromString x


--为把http获取的txt文本转化为List->元组List（添加index）
transitionList model =
  List.indexedMap Tuple.pair model.centerDiv
