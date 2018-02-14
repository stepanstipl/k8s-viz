import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode exposing ((|:))
import Update.Extra exposing (andThen)
import Time
import Svg
import Svg.Attributes
import Collage
import Collage.Layout
import Collage.Render
import Collage.Text
import Color
import Dict
import String.Extra
import Debug

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- VARS

api_server = "http://localhost:8001/k8s-api"
namespace = "default"
end_deployments = api_server ++ "/apis/apps/v1beta2/namespaces/" ++ namespace ++ "/deployments"
end_pods = api_server ++ "/api/v1/namespaces/" ++ namespace ++ "/pods"
end_replicasets = api_server ++ "/apis/apps/v1beta2/namespaces/" ++ namespace ++ "/replicasets"
end_svcs = api_server ++ "/api/v1/namespaces/" ++ namespace ++ "/services"
end_ings = api_server ++ "/apis/extensions/v1beta1/namespaces/" ++ namespace ++ "/ingresses"

pod_margin = 25
pod_width = 100

-- MODEL

type alias Metadata =
  { name: String
  }

type alias Pods =
  List Pod

type alias Svcs =
  List Svc

type alias Pod =
  { name: String
  , labels: Dict.Dict String String
  , owner: String
  , phase: String
  , conditions: List (Dict.Dict String String)
  , deletionTimestamp: Maybe String
  }

type alias Ing =
  { name: String,
    http_paths: List (List IngPath)
  }

type alias IngPath =
  {
    path: String,
    backend: String
  }

type alias Svc =
  { name: String
  , selectors: Maybe (Dict.Dict String String)
  }

type alias Model =
  { deployments: List String
  , replicasets: List String
  , pods : List Pod
  , svcs : List Svc
  , ings : List Ing
  }

emptyPod = { name = ""
           , labels = Dict.empty
           , owner = ""
           , phase = ""
           , conditions = []
           }

init : (Model, Cmd Msg)
init =
  ( Model [] [] [] [] [],
    getPods
  )

-- UPDATE
type Msg
  = Refresh
  | Tick Time.Time
  | GetPods
  | GetSvcs
  | GetIngs
  | GetDeployments
  | GetReplicaSets
  | NewPods (Result Http.Error (Pods))
  | NewSvcs (Result Http.Error (Svcs))
  | NewIngs (Result Http.Error (List Ing))
  | NewDeployments (Result Http.Error (List String))
  | NewReplicaSets (Result Http.Error (List String))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Refresh ->
      model ! []
        |> andThen update GetPods
        |> andThen update GetDeployments
        |> andThen update GetReplicaSets
        |> andThen update GetSvcs
        |> andThen update GetIngs

    Tick time ->
      model ! []
        |> andThen update GetPods
        |> andThen update GetDeployments
        |> andThen update GetReplicaSets
        |> andThen update GetSvcs
        |> andThen update GetIngs

    GetDeployments ->
      (model, getDeployments)

    GetPods ->
      (model, getPods)

    GetSvcs ->
      (model, getSvcs)

    GetIngs ->
      (model, getIngs)

    GetReplicaSets ->
      (model, getReplicaSets)

    NewPods (Ok value) ->
      ({model | pods = value}, Cmd.none)

    NewPods (Err _) ->
      (model, Cmd.none)

    NewSvcs (Ok value) ->
      ( {model | svcs = value}, Cmd.none)

    NewSvcs (Err _) ->
      (model, Cmd.none)

    NewIngs (Ok value) ->
      ( {model | ings = value}, Cmd.none)

    NewIngs (Err _) ->
      (model, Cmd.none)

    NewDeployments (Ok value) ->
      ({model | deployments = value}, Cmd.none)

    NewDeployments (Err _) ->
      (model, Cmd.none)

    NewReplicaSets (Ok value) ->
      ({model | replicasets = value}, Cmd.none)

    NewReplicaSets (Err _) ->
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div [ class "lists" ]
    [ renderList "Services" (List.map .name model.svcs)
    , renderList "Ingresses" (List.map .name model.ings)
    , renderList "Deployments: " model.deployments
    , renderList "ReplicaSets: " model.replicasets
    , renderList "Pods: " (List.map .name model.pods)
    , renderAll model.deployments model.replicasets model.pods model.svcs model.ings
    ]

renderList name lst =
    div []
      [ div [] [ text name]
      , ul [] (List.map (\l -> li [] [ text l ]) lst)
      ]

filterPods: Dict.Dict String String -> Pods -> Pods
filterPods labels pods =
    if not (Dict.isEmpty labels) then
      pods
        |> List.filter (hasLabels labels)
        |> List.filter (\l -> l.deletionTimestamp == Nothing)
        |> List.filter (\l -> l.phase == "Running")
    else
      []

hasLabels: Dict.Dict String String -> Pod -> Bool
hasLabels labels pod =
  let
    kv_present: String -> String -> Bool
    kv_present key value =
      (Maybe.withDefault "" (Dict.get key pod.labels)) == value
  in
    labels
      |> Dict.filter kv_present
      |> Dict.isEmpty
      |> not

renderAll : List String -> List String -> Pods -> Svcs -> List Ing -> Html.Html msg
renderAll deployments replicasets pods svcs ings =
  let
    deps  =
      List.map (renderDeployment replicasets pods) deployments
        |> wrapH

    resources =
      [ renderIngs ings, renderSvcs svcs, deps ]
        |> wrapV

    traffic =
      [ renderSvcs2Pods pods svcs resources ]
      ++ [ renderIng2Svcs ings resources ]

  in
    traffic
      ++ [ resources ]
      |> Collage.Layout.stack
      |> Collage.Render.svg

renderSvcs: Svcs -> Collage.Collage msg
renderSvcs svcs =
  svcs
    |> List.map (\l -> renderEmptyFrame 200 50 "svc-" l.name)
    |> wrapH

renderIngPaths: Ing -> Collage.Collage msg
renderIngPaths ing =
  ing.http_paths
    |> List.foldr (++) []
    |> List.sortBy .path
    |> List.map (\l -> renderEmptyFrame 100 25 "ing-path-" l.path)
    |> wrapH

renderEmptyFrame: Float -> Float -> String -> String -> Collage.Collage msg
renderEmptyFrame x y prefix name =
  let
    san_name =
      String.Extra.replace "/" "" name
    frame =
      Collage.rectangle x y
        |> Collage.filled (Collage.transparent)
        |> Collage.Layout.name (prefix ++ san_name)
        |> Collage.Layout.center

    label =
      Collage.Text.fromString name
        |> Collage.rendered
        |> Collage.Layout.name ("label-" ++ prefix ++ san_name)
        |> Collage.Layout.center
  in
    [ label, frame ]
      |> List.map (Collage.Layout.center)
      |> Collage.Layout.stack

renderFrameAround: String -> String -> Collage.Collage msg -> Collage.Collage msg
renderFrameAround prefix name inner_content =
  let
    san_name =
      String.Extra.replace "/" "" name
    frame =
      Collage.rectangle (Collage.Layout.width content) (Collage.Layout.height content)
        |> Collage.filled (Collage.transparent)
        |> Collage.Layout.name (prefix ++ san_name)
        |> Collage.Layout.center

    label =
      Collage.Text.fromString name
        |> Collage.rendered
        |> Collage.Layout.name ("label-" ++ prefix ++ san_name)
        |> Collage.Layout.center

    content =
      [ label, inner_content ]
        |> Collage.Layout.vertical
  in
    [ content, frame ]
      |> List.map (Collage.Layout.center)
      |> Collage.Layout.stack

renderIngs: List Ing -> Collage.Collage msg
renderIngs ings =
  let
    render_ing ing =
      let
        paths = renderIngPaths ing
          |> Collage.Layout.center
      in
        renderFrameAround "ing-" ing.name paths
  in
    ings
       |> List.map render_ing
       |> wrapH
       |> Collage.Layout.center


renderSvcs2Pods: Pods -> Svcs -> Collage.Collage msg -> Collage.Collage msg
renderSvcs2Pods pods svcs collage =
  let
    get_points svc =
      List.map (\l -> Collage.Layout.locate ("pod-" ++ l) Collage.Layout.top collage) (List.map .name (selected_pods svc))

    get_start svc =
      Collage.Layout.locate ("svc-" ++ svc.name) Collage.Layout.bottom collage

    selected_pods svc =
      filterPods (Maybe.withDefault Dict.empty svc.selectors) pods

    render_svc2pods svc =
      List.map (Collage.segment (Maybe.withDefault (0,0) (get_start svc))) (List.map (Maybe.withDefault (0,0)) (get_points svc))
        |> List.map (Collage.traced (Collage.dot Collage.thick (Collage.uniform Color.yellow)))
        |> Collage.Layout.stack
        |> Collage.Layout.name ("svc2pods-" ++ svc.name)

  in
    svcs
      |> List.map render_svc2pods
      |> Collage.Layout.stack

renderIng2Svcs: List Ing -> Collage.Collage msg -> Collage.Collage msg
renderIng2Svcs ings collage =
  let
    render_ing2svcs ing =
      let
        san_name name =
          String.Extra.replace "/" "" name

        start path =
          Collage.Layout.locate ("ing-path-" ++ san_name(path.path)) Collage.Layout.bottom collage

        end path =
          Collage.Layout.locate ("svc-" ++ path.backend) Collage.Layout.top collage

        render_path2svc path =
          Collage.segment (Maybe.withDefault (0,0) (start path)) (Maybe.withDefault (0,0) (end path))
            |> Collage.traced (Collage.dot Collage.thick (Collage.uniform Color.yellow))
            |> Collage.Layout.name ("ing2svcs-" ++ (san_name path.path))
    in
      ing.http_paths
        |> List.foldr (++) []
        |> List.map render_path2svc
        |> Collage.Layout.stack
  in
    ings
      |> List.map render_ing2svcs
      |> Collage.Layout.stack

renderDeployment : List String -> Pods -> String -> Collage.Collage msg
renderDeployment replicasets pods name =
  let
     rs =
       List.map (renderReplicaSet (filterNamePods name pods)) (filterNamePrefix name replicasets)
         |> wrapH
  in
    renderFrameAround "dep-" name rs

greater : Float -> Float -> Float
greater a b =
  if a > b then
    a
  else
    b

wrapH : List (Collage.Collage msg) -> Collage.Collage msg
wrapH list =
  let
    space =
      Collage.Layout.spacer pod_margin pod_margin
      --  |> Collage.Layout.debug
  in
    [ space ]
      ++ (List.intersperse space list)
      ++ [ space ]
      |> List.map (Collage.Layout.align Collage.Layout.top)
      |> Collage.Layout.horizontal
      |> wrapItem
      |> List.map (Collage.Layout.align Collage.Layout.left)
      |> Collage.Layout.vertical
      |> Collage.Layout.center

wrapV : List (Collage.Collage msg) -> Collage.Collage msg
wrapV list =
  let
    space =
      Collage.Layout.spacer pod_margin pod_margin
      --  |> Collage.Layout.debug
  in
    [ space ]
      ++ (List.intersperse space list)
      ++ [ space ]
      |> List.map (Collage.Layout.align Collage.Layout.top)
      |> Collage.Layout.vertical
      |> wrapItem
      |> List.map (Collage.Layout.align Collage.Layout.left)
      |> Collage.Layout.horizontal
      |> Collage.Layout.center

wrap : List (Collage.Collage msg) -> List (Collage.Collage msg)
wrap list =
  let
    space =
      Collage.Layout.spacer pod_margin pod_margin
      -- |> Collage.Layout.debug
  in
    [ space ]
      ++ (List.intersperse space list)
      ++ [ space ]

wrapItem : Collage.Collage msg -> List (Collage.Collage msg)
wrapItem item =
  let
    space =
      Collage.Layout.spacer pod_margin pod_margin
      --  |> Collage.Layout.debug
  in
    [ space, item, space ]

renderReplicaSet : Pods -> String -> Collage.Collage msg
renderReplicaSet pods name =
  let
     podsc =
       List.map renderPod (filterNamePods name pods)
         |> wrap
         |> Collage.Layout.horizontal
         |> wrapItem
         |> Collage.Layout.vertical
         |> Collage.Layout.align Collage.Layout.base

     rs =
       Collage.rectangle (greater (Collage.Layout.width podsc) (Collage.Layout.width label)) (Collage.Layout.height podsc)
         |> Collage.filled (Collage.transparent)
         |> Collage.Layout.align Collage.Layout.base
         |> Collage.Layout.name ("rs-" ++ name)

     label =
       Collage.Text.fromString name
         |> Collage.rendered
         |> Collage.shiftY ((Collage.Layout.height podsc) / 2 - (pod_margin / 2))
         |> Collage.Layout.name ("label-rs-" ++ name)

  in
   [label, podsc, rs]
     |> Collage.Layout.stack
     -- |> Collage.Layout.debug

renderPod : Pod -> Collage.Collage msg
renderPod pod =
  let
    podc =
      Collage.rectangle pod_width pod_width
        |> Collage.filled (Collage.transparent)
        |> Collage.Layout.name ("pod-" ++ pod.name)

    get_status =
      if (pod.deletionTimestamp == Nothing) then
        "good"
      else
        "bad"


  in
    [podc]
      |> Collage.group
      |> Collage.Layout.name ("pod-" ++ pod.phase ++ "-" ++ get_status ++ "-" ++ pod.name)
      -- |> Collage.Layout.debug

filterNamePrefix : String -> List String -> List String
filterNamePrefix prefix list =
  list
    |> List.filter (String.startsWith prefix)

filterNamePods : String -> Pods -> Pods
filterNamePods prefix list =
  list
    |> List.filter (\m -> String.startsWith prefix m.name )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick

-- HTTP
getPods : Cmd Msg
getPods =
  Http.send NewPods (Http.get end_pods podDecoder)

getSvcs : Cmd Msg
getSvcs =
  Http.send NewSvcs (Http.get end_svcs svcDecoder)

getIngs : Cmd Msg
getIngs =
  Http.send NewIngs (Http.get end_ings ingDecoder)

getReplicaSets : Cmd Msg
getReplicaSets =
  Http.send NewReplicaSets (Http.get end_replicasets itemsNameDecoder)

getDeployments : Cmd Msg
getDeployments =
  Http.send NewDeployments (Http.get end_deployments itemsNameDecoder)

itemsNameDecoder : Decode.Decoder (List String)
itemsNameDecoder =
  Decode.field "items" (Decode.list (Decode.at ["metadata","name"] Decode.string))

type alias PodLight =
  { name: String
  , labels: Dict.Dict String String
  , owner: String
  }

podDecoder : Decode.Decoder (Pods)
podDecoder =
  Decode.field "items"
    (Decode.list
      (Decode.map6 Pod
          (Decode.at ["metadata", "name"] Decode.string)
          (Decode.at ["metadata", "labels"] (Decode.dict Decode.string))
          (Decode.at ["metadata", "ownerReferences"] (Decode.index 0 (Decode.field "name" Decode.string)))
          (Decode.at ["status", "phase"] Decode.string)
          (Decode.at ["status", "conditions"] (Decode.list (Decode.dict (Decode.oneOf [Decode.string, Decode.null ""]))))
          (Decode.maybe (Decode.at ["metadata", "deletionTimestamp"] Decode.string))
       )
    )

svcDecoder : Decode.Decoder (Svcs)
svcDecoder =
  Decode.field "items"
    (Decode.list
      (Decode.map2 Svc
          (Decode.at ["metadata", "name"] Decode.string)
          (Decode.maybe
            (Decode.at ["spec", "selector"] (Decode.dict Decode.string))
          )
       )
    )

ingDecoder : Decode.Decoder (List Ing)
ingDecoder =
  Decode.field "items"
    (Decode.list
      (Decode.map2 Ing
          (Decode.at ["metadata", "name"] Decode.string)
            (Decode.at ["spec", "rules"]
              (Decode.list
                (Decode.at ["http", "paths"]
                  (Decode.list
                    (Decode.map2 IngPath
                      (Decode.field "path" Decode.string)
                      (Decode.at ["backend", "serviceName"] Decode.string)
                    )
                  )
                )
              )
            )

       )
    )