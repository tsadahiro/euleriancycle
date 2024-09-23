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



main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }
       
type alias Model = {points: List {x:Float, y:Float, z:Float}
                   ,eulerCycle: List {from:Int, to:Int}
                   ,tourLength: Int
                   }

type Msg = Elapsed Time.Posix
    
init : () -> (Model, Cmd Msg)
init _ =
    ({points = List.map (\p -> {x=3.0*p.x, y=3.0*p.y, z=3.0*p.z}) pointCoords
     ,eulerCycle = eulerCycle
     ,tourLength = 0
     }
    ,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Elapsed t ->
            ({model | tourLength = modBy 192 (model.tourLength+1)}, Cmd.none)


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
        blackMaterial =
            Material.nonmetal
                { baseColor = Color.black
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        sphereView center =
            Scene3d.sphere material <|
                Sphere3d.withRadius (Length.meters 0.1) (Point3d.meters center.x center.y center.z)

        spheres =
            List.map sphereView model.points

        point p =
            Point3d.meters p.x p.y p.z

        dist p q =
            sqrt ((p.x - q.x)^2 + (p.y - q.y)^2 + (p.z - q.z)^2 )

        vertex i =
            Maybe.withDefault {x=0,y=0,z=0} <|
                --List.head <| List.drop (i-1) model.points
                List.head <| List.drop i model.points
                
        edge edata =
            Scene3d.cylinderWithShadow material <|
                Cylinder3d.startingAt 
                    (point <| vertex edata.from)
                    (Maybe.withDefault Direction3d.x <|
                         (Direction3d.from (point <| vertex edata.from) (point <| vertex edata.to)))
                    {radius = Length.meters 0.05
                    ,length = Length.meters (dist (vertex edata.from) (vertex edata.to))
                    }

        cover edata =
            Scene3d.cylinderWithShadow redMaterial <|
                Cylinder3d.startingAt 
                    (point <| vertex edata.from)
                    (Maybe.withDefault Direction3d.x <|
                         (Direction3d.from (point <| vertex edata.from) (point <| vertex edata.to)))
                    {radius = Length.meters 0.06
                    ,length = Length.meters (dist (vertex edata.from) (vertex edata.to))
                    }
                    
        edges = (List.map edge model.eulerCycle) ++
                (List.map cover (List.take model.tourLength model.eulerCycle))
            
                
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 10 10 8
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Html.div[]
        [
         {-Html.input
            [ Attrs.type_ "range"
            , Attrs.min "0"
            , Attrs.max "180"
            --, Attrs.value <| String.fromFloat model.angle2
            ]
            []-}
        Html.div [] [Html.text (String.fromInt model.tourLength)]
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
