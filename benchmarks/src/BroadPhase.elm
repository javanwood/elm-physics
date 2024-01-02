module BroadPhase exposing (main)

import Acceleration
import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collision.ConvexConvex
import Direction3d
import Internal.BroadPhase as BroadPhase
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.World
import Physics.Body
import Physics.World
import Point3d
import Shapes.Convex as Convex exposing (Convex)


main : BenchmarkProgram
main =
    program <|
        describe "BroadPhase.addContacts"
            [ thing
            ]


type BodyId
    = Floor


thing : Benchmark
thing =
    let
        (Internal.World.Protected world) =
            Physics.World.empty
                |> Physics.World.withGravity
                    (Acceleration.metersPerSecondSquared 9.80665)
                    Direction3d.negativeZ
                |> Physics.World.add
                    (Physics.Body.plane Floor
                        |> Physics.Body.moveTo (Point3d.meters 0 0 -3)
                    )
    in
    Benchmark.compare "Broad Phase"
        "addContacts"
        (\_ -> BroadPhase.addContacts world)
        "addContacts2"
        (\_ -> BroadPhase.addContacts2 world)
