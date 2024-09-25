module Loop2 exposing (..)

import Browser
import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts
import Length
import Pixels
import Point3d
import Sphere3d
import Cylinder3d
import Triangle3d
import Scene3d
import Axis3d
import Scene3d.Material as Material
import Viewpoint3d
import Time
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Mouse as Mouse


main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }
       
type alias Model = {points: List (Point3d.Point3d Length.Meters WorldCoordinates)
                        --points: List {x:Float, y:Float, z:Float}                        
                   , eulerCycle: List {from:Int, to:Int}
                   , tourLength: Int
                   , start : Maybe {x:Float, y:Float}
                   }

type Msg = Elapsed Time.Posix 
    | RStart {x: Float, y: Float}
    | RMove {x: Float, y: Float}
    | REnd {x: Float, y: Float}
    | Forward
    | Backward

type WorldCoordinates = WorldCoordinates
    
init : () -> (Model, Cmd Msg)
init _ =
    ({points = List.map (\p -> (Point3d.meters (3.0*p.x) (3.0*p.y) (3.0*p.z))) pointCoords
     ,eulerCycle = eulerCycle
     ,tourLength = 0
     , start = Nothing
     }
    ,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RStart pos ->
            let
                dummy = Debug.log "RStart" pos
            in
            ({model | start = Just pos}, Cmd.none)
        RMove pos ->
            let
                dir = Debug.log "" <| case model.start of
                                          Just from -> { x = pos.y - from.y
                                                       , y = pos.x - from.x
                                                       }
                                          -- Just from -> { x = -pos.y + from.y
                                          --              , y = pos.x - from.x
                                          --              }
                                          Nothing -> {x=1,y=0}
                axis = Maybe.withDefault Axis3d.x <|
                       Axis3d.throughPoints
                           Point3d.origin
                           (Point3d.meters  0 dir.x dir.y )
                angle = Angle.degrees  (0.5*(sqrt (dir.x^2 + dir.y^2)))
                points = List.map (\v -> Point3d.rotateAround
                                           axis angle v
                                    ) model.points
            in
            ({model | start = case model.start of
                                  Just drom -> Just pos
                                  Nothing -> Nothing
             ,points = case model.start of
                             Just from ->  points
                             Nothing -> model.points
             }
            , Cmd.none)
        REnd pos ->
            let
                dummy = Debug.log "REnd" pos
                newModel = {model|start = Nothing}
            in
                (newModel, Cmd.none)
        Elapsed t -> --(model, Cmd.none)
            --({model | tourLength = modBy 192 (model.tourLength+1)}, Cmd.none)
                    (model, Cmd.none)
        Forward ->
            let
                findVisited idx =
                    let
                        appeared = List.map (\e -> e.from ) <| List.take idx eulerCycle
                        edge = Maybe.withDefault {from=-1,to=-1} <| List.head <| List.drop idx eulerCycle
                        end = edge.to
                    in
                        if (List.member end appeared) then
                            idx
                        else
                            findVisited (idx+1)
            in
                --({model | tourLength = modBy 192 (model.tourLength+1)}, Cmd.none)
                ({model | tourLength = ((findVisited model.tourLength)+1)}, Cmd.none)
        Backward ->
            ({model | tourLength = modBy 192 (model.tourLength-1)}, Cmd.none)


view: Model -> Html Msg
view model =
    let
        material =
            Material.nonmetal
                { baseColor = Color.lightBlue
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        redMaterial =
            Material.nonmetal
                { baseColor = Color.red
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        yellowMaterial =
            Material.metal
                { baseColor = Color.yellow
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }

        blackMaterial =
            Material.nonmetal
                { baseColor = Color.black
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        sphereView center mat =
            Scene3d.sphere mat <|
                --Sphere3d.withRadius (Length.meters 0.1) (Point3d.meters center.x center.y center.z)
                Sphere3d.withRadius (Length.meters 0.1) center

        spheres =
            List.indexedMap (\idx pt -> if (idx==0 ) then
                                            sphereView pt yellowMaterial
                                        else if (idx==5) then
                                            sphereView pt blackMaterial
                                             else
                                                 sphereView pt material
                            ) model.points
        point p =
            Point3d.meters p.x p.y p.z

        dist p q =
            Point3d.distanceFrom p q

        vertex i =
            Maybe.withDefault (Point3d.meters 0 0 0) <|
                List.head <| List.drop i model.points
                
        edge edata =
            Scene3d.cylinderWithShadow material <|
                Cylinder3d.startingAt 
                    (vertex edata.from)
                    (Maybe.withDefault Direction3d.x <|
                         (Direction3d.from (vertex edata.from) (vertex edata.to)))
                    {radius = Length.meters 0.025
                    ,length = (dist (vertex edata.from) (vertex edata.to))
                    }

        cover edata =
            Scene3d.cylinderWithShadow redMaterial <|
                Cylinder3d.startingAt 
                    (vertex edata.from)
                    (Maybe.withDefault Direction3d.x <|
                         (Direction3d.from (vertex edata.from) (vertex edata.to)))
                    {radius = Length.meters 0.06
                    ,length = (dist (vertex edata.from) (vertex edata.to))
                    }
                    
        edges = (List.map edge model.eulerCycle) ++
                (List.map cover ((List.take (model.tourLength)) model.eulerCycle))
            
                
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 10 0 5
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 40
                }
        current = Maybe.withDefault {from=0,to=0} <| List.head (List.drop (model.tourLength-1) model.eulerCycle)
    in
    Html.div[Pointer.onDown (\ev-> RStart (relativePosition ev))
            ,Pointer.onUp (\ev-> REnd (relativePosition ev))
            ,Pointer.onMove (\ev-> RMove (relativePosition ev))
            ]
        [
         {-Html.input
            [ Attrs.type_ "range"
            , Attrs.min "0"
            , Attrs.max "180"
            --, Attrs.value <| String.fromFloat model.angle2
            ]
            []-}
        Html.h1 [] [Html.text ((String.fromInt model.tourLength)++" "++
                                    (String.fromInt current.from)++"-->"++
                                    (String.fromInt current.to)
                               )]
        ,Html.button [Evts.onClick Forward] [Html.text "+"]
        ,Html.button [Evts.onClick Backward] [Html.text "-"]
        ,Scene3d.sunny
             { camera = camera
             , clipDepth = Length.meters 0.05
             , dimensions = ( Pixels.int 1000, Pixels.int 1000 )
             , background = Scene3d.transparentBackground
             , entities = spheres ++ edges
             , shadows = True
             , upDirection = Direction3d.z
             , sunlightDirection = Direction3d.xz (Angle.degrees -120)
             }
        ]


relativePosition ev =
    {x= Tuple.first ev.pointer.offsetPos
    ,y= Tuple.second ev.pointer.offsetPos
    }
        
pointCoords = [{x=0,y=0,z=1},
                   {x=1,y=0,z=0},
                   {x=0,y=1,z=0},
                   {x=-1,y=0,z=0},
                   {x=0,y=-1,z=0},
                   {x=0,y=0,z=-1},
                   {x=0.7071067811865475,y=0,z=0.7071067811865475},
                   {x=0,y=0.7071067811865475,z=0.7071067811865475},
                   {x=-0.7071067811865475,y=0,z=0.7071067811865475},
                   {x=0,y=-0.7071067811865475,z=0.7071067811865475},
                   {x=0.7071067811865475,y=0,z=-0.7071067811865475},
                   {x=0,y=0.7071067811865475,z=-0.7071067811865475},
                   {x=-0.7071067811865475,y=0,z=-0.7071067811865475},
                   {x=0,y=-0.7071067811865475,z=-0.7071067811865475},
                   {x=0.7071067811865475,y=0.7071067811865475,z=0},
                   {x=-0.7071067811865475,y=0.7071067811865475,z=0},
                   {x=-0.7071067811865475,y=-0.7071067811865475,z=0},
                   {x=0.7071067811865475,y=-0.7071067811865475,z=0},
                   {x=0.3826834323650898,y=0,z=0.9238795325112867},
                   {x=0.9238795325112867,y=0,z=0.3826834323650898},
                   {x=0,y=0.3826834323650898,z=0.9238795325112867},
                   {x=0,y=0.9238795325112867,z=0.3826834323650898},
                   {x=-0.3826834323650898,y=0,z=0.9238795325112867},
                   {x=-0.9238795325112867,y=0,z=0.3826834323650898},
                   {x=0,y=-0.3826834323650898,z=0.9238795325112867},
                   {x=0,y=-0.9238795325112867,z=0.3826834323650898},
                   {x=0.3826834323650898,y=0,z=-0.9238795325112867},
                   {x=0.9238795325112867,y=0,z=-0.3826834323650898},
                   {x=0,y=0.3826834323650898,z=-0.9238795325112867},
                   {x=0,y=0.9238795325112867,z=-0.3826834323650898},
                   {x=-0.3826834323650898,y=0,z=-0.9238795325112867},
                   {x=-0.9238795325112867,y=0,z=-0.3826834323650898},
                   {x=0,y=-0.3826834323650898,z=-0.9238795325112867},
                   {x=0,y=-0.9238795325112867,z=-0.3826834323650898},
                   {x=0.9238795325112867,y=0.3826834323650898,z=0},
                   {x=0.3826834323650898,y=0.9238795325112867,z=0},
                   {x=-0.3826834323650898,y=0.9238795325112867,z=0},
                   {x=-0.9238795325112867,y=0.3826834323650898,z=0},
                   {x=-0.9238795325112867,y=-0.3826834323650898,z=0},
                   {x=-0.3826834323650898,y=-0.9238795325112867,z=0},
                   {x=0.3826834323650898,y=-0.9238795325112867,z=0},
                   {x=0.9238795325112867,y=-0.3826834323650898,z=0},
                   {x=0.816496580927726,y=0.408248290463863,z=0.408248290463863},
                   {x=0.408248290463863,y=0.816496580927726,z=0.408248290463863},
                   {x=0.408248290463863,y=0.408248290463863,z=0.816496580927726},
                   {x=-0.408248290463863,y=0.816496580927726,z=0.408248290463863},
                   {x=-0.816496580927726,y=0.408248290463863,z=0.408248290463863},
                   {x=-0.408248290463863,y=0.408248290463863,z=0.816496580927726},
                   {x=-0.816496580927726,y=-0.408248290463863,z=0.408248290463863},
                   {x=-0.408248290463863,y=-0.816496580927726,z=0.408248290463863},
                   {x=-0.408248290463863,y=-0.408248290463863,z=0.816496580927726},
                   {x=0.408248290463863,y=-0.816496580927726,z=0.408248290463863},
                   {x=0.816496580927726,y=-0.408248290463863,z=0.408248290463863},
                   {x=0.408248290463863,y=-0.408248290463863,z=0.816496580927726},
                   {x=0.816496580927726,y=0.408248290463863,z=-0.408248290463863},
                   {x=0.408248290463863,y=0.816496580927726,z=-0.408248290463863},
                   {x=0.408248290463863,y=0.408248290463863,z=-0.816496580927726},
                   {x=-0.408248290463863,y=0.816496580927726,z=-0.408248290463863},
                   {x=-0.816496580927726,y=0.408248290463863,z=-0.408248290463863},
                   {x=-0.408248290463863,y=0.408248290463863,z=-0.816496580927726},
                   {x=-0.816496580927726,y=-0.408248290463863,z=-0.408248290463863},
                   {x=-0.408248290463863,y=-0.816496580927726,z=-0.408248290463863},
                   {x=-0.408248290463863,y=-0.408248290463863,z=-0.816496580927726},
                   {x=0.408248290463863,y=-0.816496580927726,z=-0.408248290463863},
                   {x=0.816496580927726,y=-0.408248290463863,z=-0.408248290463863},
                   {x=0.408248290463863,y=-0.408248290463863,z=-0.816496580927726}
              ]

eulerCycle= [{from = 0, to = 18},
                 {from = 18, to = 6},
                 {from = 6, to = 19},
                 {from = 19, to = 1},
                 {from = 1, to = 27},
                 {from = 27, to = 10},
                 {from = 10, to = 26},
                 {from = 26, to = 5},
                 {from = 5, to = 28},
                 {from = 28, to = 11},
                 {from = 11, to = 29},
                 {from = 29, to = 2},
                 {from = 2, to = 21},
                 {from = 21, to = 7},
                 {from = 7, to = 20},
                 {from = 20, to = 0},
                 {from = 0, to = 22},
                 {from = 22, to = 8},
                 {from = 8, to = 23},
                 {from = 23, to = 3},
                 {from = 3, to = 31},
                 {from = 31, to = 12},
                 {from = 12, to = 30},
                 {from = 30, to = 5},
                 {from = 5, to = 32},
                 {from = 32, to = 13},
                 {from = 13, to = 33},
                 {from = 33, to = 4},
                 {from = 4, to = 25},
                 {from = 25, to = 9},
                 {from = 9, to = 24},
                 {from = 24, to = 18},
                 {from = 18, to = 20},
                 {from = 20, to = 22},
                 {from = 22, to = 24},
                 {from = 24, to = 50},
                 {from = 50, to = 8},
                 {from = 8, to = 46},
                 {from = 46, to = 15},
                 {from = 15, to = 36},
                 {from = 36, to = 2},
                 {from = 2, to = 35},
                 {from = 35, to = 14},
                 {from = 14, to = 34},
                 {from = 34, to = 1},
                 {from = 1, to = 41},
                 {from = 41, to = 17},
                 {from = 17, to = 40},
                 {from = 40, to = 4},
                 {from = 4, to = 39},
                 {from = 39, to = 16},
                 {from = 16, to = 38},
                 {from = 38, to = 3},
                 {from = 3, to = 37},
                 {from = 37, to = 15},
                 {from = 15, to = 45},
                 {from = 45, to = 7},
                 {from = 7, to = 43},
                 {from = 43, to = 14},
                 {from = 14, to = 42},
                 {from = 42, to = 6},
                 {from = 6, to = 44},
                 {from = 44, to = 7},
                 {from = 7, to = 47},
                 {from = 47, to = 8},
                 {from = 8, to = 48},
                 {from = 48, to = 16},
                 {from = 16, to = 49},
                 {from = 49, to = 9},
                 {from = 9, to = 50},
                 {from = 50, to = 22},
                 {from = 22, to = 47},
                 {from = 47, to = 20},
                 {from = 20, to = 44},
                 {from = 44, to = 18},
                 {from = 18, to = 53},
                 {from = 53, to = 6},
                 {from = 6, to = 52},
                 {from = 52, to = 17},
                 {from = 17, to = 51},
                 {from = 51, to = 9},
                 {from = 9, to = 53},
                 {from = 53, to = 51},
                 {from = 51, to = 25},
                 {from = 25, to = 39},
                 {from = 39, to = 33},
                 {from = 33, to = 40},
                 {from = 40, to = 25},
                 {from = 25, to = 49},
                 {from = 49, to = 39},
                 {from = 39, to = 61},
                 {from = 61, to = 13},
                 {from = 13, to = 62},
                 {from = 62, to = 12},
                 {from = 12, to = 58},
                 {from = 58, to = 15},
                 {from = 15, to = 57},
                 {from = 57, to = 11},
                 {from = 11, to = 55},
                 {from = 55, to = 14},
                 {from = 14, to = 54},
                 {from = 54, to = 10},
                 {from = 10, to = 56},
                 {from = 56, to = 11},
                 {from = 11, to = 59},
                 {from = 59, to = 12},
                 {from = 12, to = 60},
                 {from = 60, to = 16},
                 {from = 16, to = 61},
                 {from = 61, to = 33},
                 {from = 33, to = 63},
                 {from = 63, to = 13},
                 {from = 13, to = 65},
                 {from = 65, to = 10},
                 {from = 10, to = 64},
                 {from = 64, to = 17},
                 {from = 17, to = 63},
                 {from = 63, to = 40},
                 {from = 40, to = 51},
                 {from = 51, to = 52},
                 {from = 52, to = 19},
                 {from = 19, to = 34},
                 {from = 34, to = 27},
                 {from = 27, to = 41},
                 {from = 41, to = 19},
                 {from = 19, to = 42},
                 {from = 42, to = 34},
                 {from = 34, to = 54},
                 {from = 54, to = 27},
                 {from = 27, to = 64},
                 {from = 64, to = 63},
                 {from = 63, to = 65},
                 {from = 65, to = 26},
                 {from = 26, to = 28},
                 {from = 28, to = 30},
                 {from = 30, to = 32},
                 {from = 32, to = 26},
                 {from = 26, to = 56},
                 {from = 56, to = 28},
                 {from = 28, to = 59},
                 {from = 59, to = 30},
                 {from = 30, to = 62},
                 {from = 62, to = 60},
                 {from = 60, to = 31},
                 {from = 31, to = 37},
                 {from = 37, to = 23},
                 {from = 23, to = 38},
                 {from = 38, to = 31},
                 {from = 31, to = 58},
                 {from = 58, to = 37},
                 {from = 37, to = 46},
                 {from = 46, to = 45},
                 {from = 45, to = 21},
                 {from = 21, to = 35},
                 {from = 35, to = 29},
                 {from = 29, to = 36},
                 {from = 36, to = 21},
                 {from = 21, to = 43},
                 {from = 43, to = 42},
                 {from = 42, to = 44},
                 {from = 44, to = 43},
                 {from = 43, to = 35},
                 {from = 35, to = 55},
                 {from = 55, to = 54},
                 {from = 54, to = 56},
                 {from = 56, to = 55},
                 {from = 55, to = 29},
                 {from = 29, to = 57},
                 {from = 57, to = 58},
                 {from = 58, to = 59},
                 {from = 59, to = 57},
                 {from = 57, to = 36},
                 {from = 36, to = 45},
                 {from = 45, to = 47},
                 {from = 47, to = 46},
                 {from = 46, to = 23},
                 {from = 23, to = 48},
                 {from = 48, to = 49},
                 {from = 49, to = 50},
                 {from = 50, to = 48},
                 {from = 48, to = 38},
                 {from = 38, to = 60},
                 {from = 60, to = 61},
                 {from = 61, to = 62},
                 {from = 62, to = 32},
                 {from = 32, to = 65},
                 {from = 65, to = 64},
                 {from = 64, to = 41},
                 {from = 41, to = 52},
                 {from = 52, to = 53},
                 {from = 53, to = 24},
                 {from = 24, to = 0}]

       
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 200 Elapsed
