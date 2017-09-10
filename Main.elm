module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, toTuple)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Mouse exposing (..)
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { cameraPos : Vec3
    , rays : List ( Vec3, Vec3 )
    }


type Msg
    = MoveMouse Mouse.Position
    | MouseClick Mouse.Position


init : ( Model, Cmd Msg )
init =
    ( Model (vec3 3 3 3) [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMouse pos ->
            let
                angle =
                    angleOfCamera pos

                cameraPos =
                    getCameraPosFromAngle angle (Vec3.getY model.cameraPos)
            in
                ( { model | cameraPos = cameraPos }, Cmd.none )

        MouseClick pos ->
            let
                destination =
                    vec3 0 1 0

                origin =
                    model.cameraPos

                newRays =
                    model.rays ++ [ ( destination, origin ) ]
            in
                ( { model | rays = newRays }, Cmd.none )


getCameraPosFromAngle : Float -> Float -> Vec3
getCameraPosFromAngle angle heightOfCamera =
    let
        cosine =
            cos angle

        sinus =
            sin angle

        hypotenuse =
            3

        adjacent =
            cosine * hypotenuse

        opposite =
            sinus * hypotenuse
    in
        vec3 adjacent heightOfCamera opposite


angleOfCamera : Mouse.Position -> Float
angleOfCamera { x, y } =
    (toFloat x) / 1000 * 3.6


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MoveMouse, Mouse.clicks MouseClick ]


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width 1000
        , height 1000
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            cubeMesh
            (uniforms model)
        , WebGL.entity
            vertexShader
            fragmentShader
            floor
            (uniforms2 model)
        , WebGL.entity
            vertexShader
            fragmentShader
            (lines model)
            (uniforms2 model)
        ]


type alias Position =
    { x : Float
    , y : Float
    }


getClickPosition : Model -> Position -> ( Vertex, Vertex )
getClickPosition model { x, y } =
    let
        normalizedPosition =
            vec3 ((x * 2) / 1000 - 1) (1 - (2 * y) / 1000) 1

        mat4 =
            Maybe.withDefault Mat4.identity (Mat4.inverse (camera model))

        inversedVec3 =
            Mat4.transform mat4 normalizedPosition

        norm =
            Vec3.normalize inversedVec3

        ( vx, vy, vz ) =
            toTuple norm
    in
        Debug.log (toString norm)
            ( Vertex (vec3 0 0 0) (vec3 vx vy vz), Vertex (vec3 0 0 0) (vec3 3 3 2) )


createLineFromVec3 : ( Vec3, Vec3 ) -> ( Vertex, Vertex )
createLineFromVec3 ( a, b ) =
    ( Vertex (vec3 0 0 0) a, Vertex (vec3 0 0 0) b )


lines : Model -> Mesh Vertex
lines model =
    let
        rays =
            List.map createLineFromVec3 model.rays
    in
        WebGL.lines rays


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


perspective =
    Mat4.makePerspective 45 1 0.01 100


camera model =
    Mat4.makeLookAt model.cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Model -> Uniforms
uniforms model =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * 0) (vec3 0 1 0))
            (Mat4.makeRotate (2 * 0) (vec3 1 0 0))
    , perspective = perspective
    , camera = camera model
    , shade = 0.8
    }


uniforms2 : Model -> Uniforms
uniforms2 model =
    { rotation =
        Mat4.identity
    , perspective = perspective
    , camera = camera model
    , shade = 0.8
    }


floor : Mesh Vertex
floor =
    WebGL.lines
        [ ( Vertex (vec3 1 0 0) (vec3 0 -1 1), Vertex (vec3 1 0 0) (vec3 0 -1 -1) )
        , ( Vertex (vec3 0 1 0) (vec3 -1 -1 0), Vertex (vec3 0 1 0) (vec3 1 -1 0) )
        , ( Vertex (vec3 0 0 1) (vec3 0 -1 0), Vertex (vec3 0 0 1) (vec3 0 1 0) )

        --        ,  ( Vertex (vec3 0 1 0) (vec3 0 1 0), Vertex (vec3 0 1 0) (vec3 0 -1 0))
        --        , ( Vertex (vec3 0 0 1) (vec3 -1 -1 1), Vertex (vec3 0 0 1) (vec3 -1 -1 -1))
        --        , ( Vertex (vec3 1 0 0) (vec3 0 0 0), Vertex (vec3 1 0 0) (vec3 0 0 0))
        ]



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 0.5 0.5 0.5

        lft =
            vec3 -0.5 0.5 0.5

        lbt =
            vec3 -0.5 -0.5 0.5

        rbt =
            vec3 0.5 -0.5 0.5

        rbb =
            vec3 0.5 -0.5 -0.5

        rfb =
            vec3 0.5 0.5 -0.5

        lfb =
            vec3 -0.5 0.5 -0.5

        lbb =
            vec3 -0.5 -0.5 -0.5
    in
        [ face Color.green rft rfb rbb rbt
        , face Color.blue rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
