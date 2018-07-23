module Common.Demo
    exposing
        ( Demo
        , DemoProgram
        , demo
        , addBodies
        , dropOnClick
        , run
        )

import AnimationFrame
import Common.Math as Math
import Common.Meshes as Meshes exposing (Attributes)
import Common.Shaders as Shaders
import Html exposing (Html)
import Html.Attributes exposing (width, height, style, type_, checked)
import Html.Events exposing (onClick, onCheck)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics exposing (World)
import Random
import Dict exposing (Dict)
import Task
import Time exposing (Time)
import WebGL exposing (Entity, Shader, Mesh)
import Window
import Common.Bodies as Bodies exposing (DemoBody)


-- Model


type alias Model =
    { debugContacts : Bool -- Set to True to see collision points
    , debugNormals : Bool -- Set to True to see normal spikes
    , debugEdges : Bool -- Set to True to see edge markers
    , debugWireframes : Bool -- Set to True to see wireframes
    , showSettings : Bool
    , screenWidth : Int
    , screenHeight : Int
    , initialWorld : World
    , world : World
    , bodies : Dict Int DemoBody
    }



-- Msg


type Msg
    = Tick Time
    | ToggleContacts Bool
    | ToggleNormals Bool
    | ToggleEdges Bool
    | ToggleWireframes Bool
    | ToggleSettings
    | Resize Window.Size
    | ResetClick
    | SceneClick
    | AddBody ( DemoBody, Physics.Body )



-- App


{-| Protect the config from being directly modifiable
-}
type Demo
    = Demo DemoConfig


type alias DemoConfig =
    { world : World
    , randomBody : Maybe (Random.Generator ( DemoBody, Physics.Body ))
    , bodies : Dict Int DemoBody
    }


{-| Initial demo configuration
-}
demo : Demo
demo =
    Demo
        { world =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)
                |> Physics.addBody plane
                |> Tuple.first
        , bodies = Dict.empty
        , randomBody = Nothing
        }


planeOffset : Vec3
planeOffset =
    vec3 0 0 -1


plane : Physics.Body
plane =
    Physics.body
        |> Physics.addShape Physics.plane
        |> Tuple.first
        |> Physics.offsetBy planeOffset


{-| Allow to drop random bodies on click
-}
dropOnClick : Random.Generator ( DemoBody, Physics.Body ) -> Demo -> Demo
dropOnClick randomBody (Demo demo) =
    Demo { demo | randomBody = Just randomBody }


{-| Add initial bodies for the scene
-}
addBodies : List ( DemoBody, Physics.Body ) -> Demo -> Demo
addBodies bodiesWithMeshes (Demo demo) =
    Demo
        (List.foldl addBodyWithMesh demo bodiesWithMeshes)


addBodyWithMesh : ( DemoBody, Physics.Body ) -> { a | world : World, bodies : Dict Int DemoBody } -> { a | world : World, bodies : Dict Int DemoBody }
addBodyWithMesh ( mesh, body ) demo =
    let
        ( world, bodyId ) =
            Physics.addBody body demo.world
    in
        { demo
            | world = world
            , bodies = Dict.insert bodyId mesh demo.bodies
        }


type alias DemoProgram =
    Program Never Model Msg


{-| Run the demo as an Elm program!
-}
run : Demo -> DemoProgram
run (Demo demo) =
    Html.program
        { init = init demo
        , update = update demo.randomBody
        , subscriptions = subscriptions
        , view = view
        }


init : DemoConfig -> ( Model, Cmd Msg )
init demo =
    ( { debugContacts = False
      , debugNormals = False
      , debugEdges = False
      , debugWireframes = False
      , showSettings = False

      -- replaced by resize, including the initial resize
      , screenWidth = 1
      , screenHeight = 1
      , initialWorld = demo.world
      , world = demo.world
      , bodies = demo.bodies
      }
    , Task.perform Resize Window.size
    )


update : Maybe (Random.Generator ( DemoBody, Physics.Body )) -> Msg -> Model -> ( Model, Cmd Msg )
update randomBody msg model =
    case msg of
        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        ToggleContacts debugContacts ->
            ( { model | debugContacts = debugContacts }, Cmd.none )

        ToggleNormals debugNormals ->
            ( { model | debugNormals = debugNormals }, Cmd.none )

        ToggleEdges debugEdges ->
            ( { model | debugEdges = debugEdges }, Cmd.none )

        ToggleWireframes debugWireframes ->
            ( { model | debugWireframes = debugWireframes }, Cmd.none )

        Resize { width, height } ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model | world = Physics.step (1 / 60) model.world }
            , Cmd.none
            )

        SceneClick ->
            case randomBody of
                Nothing ->
                    ( model, Cmd.none )

                Just randomBody ->
                    ( model, Random.generate AddBody randomBody )

        ResetClick ->
            ( { model | world = model.initialWorld }, Cmd.none )

        AddBody bodyAndMesh ->
            ( addBodyWithMesh bodyAndMesh model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]


type alias SceneParams =
    { lightDirection : Vec3
    , camera : Mat4
    , perspective : Mat4
    , bodies : Dict Int DemoBody
    , debugWireframes : Bool
    }


view : Model -> Html Msg
view model =
    Html.div
        []
        [ webGL model
        , settings model
        ]


settings : Model -> Html Msg
settings { showSettings, debugContacts, debugEdges, debugNormals, debugWireframes } =
    Html.div
        [ style
            [ ( "position", "fixed" )
            , ( "right", "6px" )
            , ( "top", "0" )
            , ( "font-family", "monospace" )
            , ( "color", "white" )
            ]
        ]
        (if showSettings then
            [ button ToggleSettings "Hide Settings"
            , Html.div
                [ style
                    [ ( "padding", "6px" )
                    , ( "min-width", "24ch" )
                    , ( "background", "rgb(50, 50, 50)" )
                    , ( "border-radius", "0 0 4px 4px" )
                    ]
                ]
                [ checkbox ToggleContacts debugContacts "collision points"
                , checkbox ToggleNormals debugNormals "normals"
                , checkbox ToggleEdges debugEdges "unique edges"
                , checkbox ToggleWireframes debugWireframes "wireframes"
                , Html.button [ onClick ResetClick, style [ ( "margin", "10px 0 5px" ) ] ] [ Html.text "Click to restart the demo" ]
                ]
            ]
         else
            [ button ToggleSettings "Show Settings" ]
        )


button : Msg -> String -> Html Msg
button msg text =
    Html.button
        [ style
            [ ( "padding", "6px" )
            , ( "box-sizing", "content-box" )
            , ( "min-width", "24ch" )
            , ( "color", "inherit" )
            , ( "border", "none" )
            , ( "font", "inherit" )
            , ( "text-align", "center" )
            , ( "margin", "0" )
            , ( "display", "block" )
            , ( "background", "rgb(61, 61, 61)" )
            ]
        , onClick msg
        ]
        [ Html.text text ]


checkbox : (Bool -> Msg) -> Bool -> String -> Html Msg
checkbox msg value label =
    Html.label [ style [ ( "display", "block" ), ( "padding", "5px 0" ) ] ]
        [ Html.input
            [ onCheck msg
            , checked value
            , type_ "checkbox"
            , style [ ( "margin-right", "10px" ) ]
            ]
            []
        , Html.text label
        ]


webGL : Model -> Html Msg
webGL { screenWidth, screenHeight, world, bodies, debugContacts, debugNormals, debugEdges, debugWireframes } =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ width screenWidth
        , height screenHeight
        , style [ ( "display", "block" ) ]
        , onClick SceneClick
        ]
        (let
            aspectRatio =
                (toFloat screenWidth) / (toFloat screenHeight)

            sceneParams =
                { lightDirection = Vec3.normalize (vec3 -1 -1 -1)
                , camera = Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k
                , perspective = Mat4.makePerspective 24 aspectRatio 5 2000
                , bodies = bodies
                , debugWireframes = debugWireframes
                }
         in
            [ ( True, Physics.foldl (addShape sceneParams) )
            , ( debugContacts
              , Physics.foldContacts (addContactIndicator sceneParams)
              )
            , ( debugNormals
              , Physics.foldFaceNormals (addNormalIndicator sceneParams)
              )
            , ( debugEdges
              , Physics.foldUniqueEdges (addEdgeIndicator sceneParams)
              )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second
                |> List.foldl (\fn entities -> fn entities world) []
        )


addShape : SceneParams -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape { lightDirection, bodies, camera, perspective, debugWireframes } { transform, bodyId } tail =
    case ( Dict.get bodyId bodies, debugWireframes ) of
        ( Nothing, _ ) ->
            tail

        ( Just demoBody, True ) ->
            WebGL.entity
                Shaders.vertex
                Shaders.fragment
                (Bodies.getWireframe demoBody)
                { camera = camera
                , color = vec3 0.9 0.9 0.9
                , lightDirection = lightDirection
                , perspective = perspective
                , transform = transform
                }
                :: tail

        ( Just demoBody, False ) ->
            -- Draw a shadow
            WebGL.entity
                Shaders.vertex
                Shaders.shadowFragment
                (Bodies.getMesh demoBody)
                { camera = camera
                , color = vec3 0.25 0.25 0.25
                , lightDirection = lightDirection
                , perspective = perspective
                , transform =
                    transform
                        -- project on the floor
                        |> Mat4.mul
                            (Math.makeShadow
                                planeOffset
                                Vec3.k
                                lightDirection
                            )
                }
                -- Draw a mesh
                :: WebGL.entity
                    Shaders.vertex
                    Shaders.fragment
                    (Bodies.getMesh demoBody)
                    { camera = camera
                    , color = vec3 0.9 0.9 0.9
                    , lightDirection = lightDirection
                    , perspective = perspective
                    , transform = transform
                    }
                :: tail


{-| Render collision point for the purpose of debugging
-}
addContactIndicator : SceneParams -> Vec3 -> List Entity -> List Entity
addContactIndicator { lightDirection, camera, perspective } contactPoint tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        contactMesh
        { camera = camera
        , perspective = perspective
        , color = vec3 1 0 0
        , lightDirection = lightDirection
        , transform =
            Mat4.makeTranslate contactPoint
        }
        :: tail


{-| Render shape face normals for the purpose of debugging
-}
addNormalIndicator : SceneParams -> Mat4 -> Vec3 -> Vec3 -> List Entity -> List Entity
addNormalIndicator { lightDirection, camera, perspective } transform normal facePoint tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        normalMesh
        { camera = camera
        , lightDirection = lightDirection
        , color = vec3 1 0 1
        , perspective = perspective
        , transform =
            Math.makeRotateKTo normal
                |> Mat4.mul
                    (facePoint
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail


{-| Render shapes' unique edge for the purpose of debugging
-}
addEdgeIndicator : SceneParams -> Mat4 -> Vec3 -> Vec3 -> List Entity -> List Entity
addEdgeIndicator { lightDirection, camera, perspective } transform edge origin tail =
    WebGL.entity
        Shaders.vertex
        Shaders.fragment
        edgeMesh
        { camera = camera
        , lightDirection = lightDirection
        , color = vec3 0 1 0
        , perspective = perspective
        , transform =
            Math.makeRotateKTo edge
                |> Mat4.mul
                    (origin
                        |> Mat4.makeTranslate
                        |> Mat4.mul transform
                    )
        }
        :: tail



-- Meshes


normalMesh : Mesh Attributes
normalMesh =
    Meshes.makePyramid 0.05 0.05


edgeMesh : Mesh Attributes
edgeMesh =
    Meshes.makePyramid 0.1 0.5


contactMesh : Mesh Attributes
contactMesh =
    Meshes.makeSphere 2 0.07
