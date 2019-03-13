--module Main exposing (Action(..), Model, getPanel, init, main, update, view)

-- import List.Extra exposing (getAt, removeAt)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Http
import Random
import Random.List exposing (..)
import Task
import Time
import Json.Decode exposing (..)

main =
  Browser.element{init = init, update = update, view = view, subscriptions = subscriptions}

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

--Model
type alias Content = List String
type alias Model =
  {leftDiv : String, centerDiv : Content, rightDiv : String, onClickLine : String}


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
          case decodeString (field "content" (Json.Decode.list string)) fullText of
          Ok listContent ->
            ({model | centerDiv = listContent},Cmd.none)
          Err _ ->
            ({model | centerDiv = ["获取文本失败"]},Cmd.none)
        Err _ ->
          ({model | centerDiv = ["获取文本失败"]},Cmd.none)
    GetIndex index ->
      ({model | onClickLine = (String.fromInt (index+1))},Cmd.none)

getContent string =
  Json.Decode.list string

view : Model -> Html Msg
view model =
  div [class "total"] [
   div [class "leftDiv"] [text model.leftDiv]
  ,div [class "centerDiv",contenteditable True] [
    pre [] (dismantling model)
  ,button [onClick Display, class "transition"] [text "转化"]
  ,div [class "attribute"] [text ("get line by onClick："++ model.onClickLine)]]

  ,div [class "rightDiv"] [text model.rightDiv]
  ]

--拆解获取的code每一行显示到<div>中
dismantling : Model -> List (Html Msg)
dismantling model =
  List.map (\x -> div [onClick (GetIndex (Tuple.first x))] [text (Tuple.second x)]) (transitionList model)

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
