module ThreeD exposing (..)

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
    ({points = pointCoords
     ,eulerCycle = eulerCycle
     ,tourLength = 0
     }
    ,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Elapsed t ->
            ({model | tourLength = modBy 120 (model.tourLength+1)}, Cmd.none)


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
                List.head <| List.drop (i-1) model.points
                
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
            
                
    {-
        plane =
            Scene3d.rotateAround Axis3d.y (Angle.degrees model.angle2) <|
                 Scene3d.quadWithShadow redMaterial
                     (Point3d.meters 1 1 0)
                     (Point3d.meters -1 1 0)
                     (Point3d.meters -1 -1 0)
                     (Point3d.meters 1 -1 0)
     -}
        -- satellite =
        --     Scene3d.rotateAround Axis3d.x (Angle.degrees model.angle) <|
        --         Scene3d.sphereWithShadow material <|
        --             Sphere3d.withRadius (Length.meters 0.1) (Point3d.meters 0.5 0.7 0.7)
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

        
pointCoords = [
 {x = 0.70254926, y = -0.182676886, z =-2.75453073},
 {x = 1.04615765, y =  0.963598573, z =-2.32610897},
 {x = 1.67027945, y = -0.628986210, z =-2.33654727},
 {x = 2.14060975, y = -1.502477278, z =-1.54196429},
 {x = 0.77308434, y = -1.306905566, z =-2.55072056},
 {x =-0.49518985, y = -1.378566323, z =-2.50707822},
 {x =-0.80869611, y = -2.296346174, z =-1.68552279},
 {x =-1.44616480, y = -0.805073331, z =-2.22930790},
 {x =-1.88639296, y =  0.296913344, z =-1.77391369},
 {x =-0.56572494, y = -0.254337643, z =-2.71088839},
 {x =-2.47197519, y =  0.060198713, z =-0.67090916},
 {x =-1.57693270, y =  1.329270861, z =-1.38806889},
 {x =-0.58073447, y =  2.081996839, z =-1.15026214},
 {x =-1.00595311, y =  0.847649033, z =-2.25549418},
 {x =-0.55063233, y =  2.310492446, z = 0.09971505},
 {x = 0.56149743, y =  2.146535881, z =-1.18956726},
 {x = 1.61740997, y =  1.509759444, z =-1.49798881},
 {x =-0.00975488, y =  1.600375010, z =-2.01768742},
 {x = 2.30010194, y =  1.344705570, z =-0.43862662},
 {x = 2.01388784, y =  0.517289249, z =-1.90812551},
 {x = 1.66611625, y =  1.879473103, z = 0.52451677},
 {x = 2.72369037, y =  0.558022872, z = 0.27698995},
 {x = 2.80200882, y = -0.690261491, z = 0.50329003},
 {x = 2.69657981, y =  0.352235375, z =-0.84876332},
 {x = 2.21642659, y = -0.926976122, z = 1.60629457},
 {x = 2.49254855, y = -1.722619008, z = 0.11744524},
 {x = 1.85826005, y = -2.329047298, z =-0.80205581},
 {x = 2.77489826, y = -0.896048988, z =-0.62246324},
 {x = 0.88066596, y = -2.940555224, z =-0.26733418},
 {x = 1.24341465, y = -2.180396634, z =-1.75613758},
 {x = 0.91076810, y = -2.712059616, z = 0.98264301},
 {x =-0.26156594, y = -3.005094266, z =-0.22802905},
 {x =-1.33608262, y = -2.509535881, z =-0.69213589},
 {x = 0.26582057, y = -2.791904560, z =-1.22141595},
 {x =-1.97006831, y = -1.974768348, z = 0.27100750},
 {x =-1.75967105, y = -1.722853183, z =-1.40775247},
 {x =-1.28737634, y = -2.139822222, z = 1.33036969},
 {x =-2.36654618, y = -0.982298153, z = 0.68114420},
 {x =-2.44486463, y =  0.265986211, z = 0.45484411},
 {x =-2.39365674, y = -1.188085650, z =-0.44460908},
 {x =-1.81057612, y =  0.872414500, z = 1.37434516},
 {x =-2.16251492, y =  1.092556230, z =-0.28506436},
 {x =-1.34024582, y = -0.001076568, z = 2.16892814},
 {x =-0.91338102, y =  1.550333856, z = 1.58851846},
 {x = 0.06421306, y =  2.161841782, z = 1.05379683},
 {x =-1.52822642, y =  1.698984520, z = 0.63443669},
 {x = 1.13872974, y =  1.666283396, z = 1.51790367},
 {x = 0.59159957, y =  2.375031489, z = 0.06040993},
 {x = 0.82522348, y =  0.748503545, z = 2.33945910},
 {x = 2.08970468, y =  1.092790405, z = 1.24013334},
 {x =-0.44305072, y =  0.676842788, z = 2.38310144},
 {x = 0.89575857, y = -0.375725135, z = 2.54326926},
 {x = 1.33598674, y = -1.477711810, z = 2.08787506},
 {x = 1.77619842, y =  0.175010554, z = 2.06168877},
 {x = 0.33978851, y = -2.230437788, z = 1.85006830},
 {x = 1.90696633, y = -1.959333639, z = 1.22044977},
 {x =-0.71612402, y = -1.593661350, z = 2.15848985},
 {x =-0.23146381, y = -2.776598659, z = 1.02194814},
 {x =-0.37251563, y = -0.447385892, z = 2.58691161},
 {x =-1.68385421, y = -1.147352027, z = 1.74050639}]

eulerCycle= [{from = 1, to = 2   },
                 {from = 2, to = 17  },
                 {from = 17, to = 16 },
                 {from = 16, to = 13 },
                 {from = 13, to = 12 },
                 {from = 12, to = 9  },
                 {from = 9, to = 8   },
                 {from = 8, to = 6   },
                 {from = 6, to = 5   },
                 {from = 5, to = 1   },
                 {from = 1, to = 3   },
                 {from = 3, to = 4   },
                 {from = 4, to = 27  },
                 {from = 27, to = 26 },
                 {from = 26, to = 23 },
                 {from = 23, to = 22 },
                 {from = 22, to = 19 },
                 {from = 19, to = 17 },
                 {from = 17, to = 20 },
                 {from = 20, to = 2  },
                 {from = 2, to = 18  },
                 {from = 18, to = 13 },
                 {from = 13, to = 15 },
                 {from = 15, to = 45 },
                 {from = 45, to = 44 },
                 {from = 44, to = 41 },
                 {from = 41, to = 39 },
                 {from = 39, to = 11 },
                 {from = 11, to = 9  },
                 {from = 9, to = 14  },
                 {from = 14, to = 10 },
                 {from = 10, to = 6  },
                 {from = 6, to = 7   },
                 {from = 7, to = 33  },
                 {from = 33, to = 32 },
                 {from = 32, to = 29 },
                 {from = 29, to = 27 },
                 {from = 27, to = 30 },
                 {from = 30, to = 4  },
                 {from = 4, to = 28  },
                 {from = 28, to = 23 },
                 {from = 23, to = 25 },
                 {from = 25, to = 53 },
                 {from = 53, to = 52 },
                 {from = 52, to = 49 },
                 {from = 49, to = 47 },
                 {from = 47, to = 21 },
                 {from = 21, to = 19 },
                 {from = 19, to = 24 },
                 {from = 24, to = 20 },
                 {from = 20, to = 3  },
                 {from = 3, to = 5   },
                 {from = 5, to = 30  },
                 {from = 30, to = 34 },
                 {from = 34, to = 7  },
                 {from = 7, to = 36  },
                 {from = 36, to = 33 },
                 {from = 33, to = 35 },
                 {from = 35, to = 37 },
                 {from = 37, to = 57 },
                 {from = 57, to = 55 },
                 {from = 55, to = 31 },
                 {from = 31, to = 29 },
                 {from = 29, to = 34 },
                 {from = 34, to = 32 },
                 {from = 32, to = 58 },
                 {from = 58, to = 31 },
                 {from = 31, to = 56 },
                 {from = 56, to = 25 },
                 {from = 25, to = 54 },
                 {from = 54, to = 49 },
                 {from = 49, to = 51 },
                 {from = 51, to = 43 },
                 {from = 43, to = 41 },
                 {from = 41, to = 46 },
                 {from = 46, to = 15 },
                 {from = 15, to = 48 },
                 {from = 48, to = 16 },
                 {from = 16, to = 18 },
                 {from = 18, to = 14 },
                 {from = 14, to = 12 },
                 {from = 12, to = 42 },
                 {from = 42, to = 11 },
                 {from = 11, to = 40 },
                 {from = 40, to = 35 },
                 {from = 35, to = 38 },
                 {from = 38, to = 39 },
                 {from = 39, to = 42 },
                 {from = 42, to = 46 },
                 {from = 46, to = 44 },
                 {from = 44, to = 51 },
                 {from = 51, to = 59 },
                 {from = 59, to = 43 },
                 {from = 43, to = 60 },
                 {from = 60, to = 37 },
                 {from = 37, to = 58 },
                 {from = 58, to = 55 },
                 {from = 55, to = 53 },
                 {from = 53, to = 56 },
                 {from = 56, to = 26 },
                 {from = 26, to = 28 },
                 {from = 28, to = 24 },
                 {from = 24, to = 22 },
                 {from = 22, to = 50 },
                 {from = 50, to = 21 },
                 {from = 21, to = 48 },
                 {from = 48, to = 45 },
                 {from = 45, to = 47 },
                 {from = 47, to = 50 },
                 {from = 50, to = 54 },
                 {from = 54, to = 52 },
                 {from = 52, to = 59 },
                 {from = 59, to = 57 },
                 {from = 57, to = 60 },
                 {from = 60, to = 38 },
                 {from = 38, to = 40 },
                 {from = 40, to = 36 },
                 {from = 36, to = 8  },
                 {from = 8, to = 10  },
                 {from = 10, to = 1  }]

    
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 500 Elapsed
