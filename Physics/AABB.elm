module Physics.AABB
    exposing
        ( AABB
        , convexPolyhedron
        , extend
        , impossible
        , plane
        , sphere
        , toHalfExtends
        )

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform exposing (Transform)
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Array.Hamt as Array exposing (Array)
import Physics.Const as Const


type alias AABB =
    { upperBound : Vec3
    , lowerBound : Vec3
    }


zero : AABB
zero =
    { lowerBound = Const.zero3
    , upperBound = Const.zero3
    }


maximum : AABB
maximum =
    { lowerBound = vec3 -Const.maxNumber -Const.maxNumber -Const.maxNumber
    , upperBound = vec3 Const.maxNumber Const.maxNumber Const.maxNumber
    }


impossible : AABB
impossible =
    { lowerBound = vec3 Const.maxNumber Const.maxNumber Const.maxNumber
    , upperBound = vec3 -Const.maxNumber -Const.maxNumber -Const.maxNumber
    }


extend : AABB -> AABB -> AABB
extend aabb1 aabb =
    let
        ( lx, ly, lz ) =
            Vec3.toTuple aabb.lowerBound

        ( ux, uy, uz ) =
            Vec3.toTuple aabb.upperBound

        ( l1x, l1y, l1z ) =
            Vec3.toTuple aabb1.lowerBound

        ( u1x, u1y, u1z ) =
            Vec3.toTuple aabb1.upperBound
    in
        { lowerBound = vec3 (min lx l1x) (min ly l1y) (min lz l1z)
        , upperBound = vec3 (max ux u1x) (max uy u1y) (max uz u1z)
        }


overlaps : AABB -> AABB -> Bool
overlaps aabb1 aabb2 =
    let
        ( l1x, l1y, l1z ) =
            Vec3.toTuple aabb1.lowerBound

        ( u1x, u1y, u1z ) =
            Vec3.toTuple aabb1.upperBound

        ( l2x, l2y, l2z ) =
            Vec3.toTuple aabb2.lowerBound

        ( u2x, u2y, u2z ) =
            Vec3.toTuple aabb2.upperBound
    in
        ((l2x <= u1x && u1x <= u2x) || (l1x <= u2x && u2x <= u1x))
            && ((l2y <= u1y && u1y <= u2y) || (l1y <= u2y && u2y <= u1y))
            && ((l2z <= u1z && u1z <= u2z) || (l1z <= u2z && u2z <= u1z))


convexPolyhedron : ConvexPolyhedron -> Transform -> AABB
convexPolyhedron convexPolyhedron transform =
    Array.foldl
        (\point ->
            let
                p =
                    Transform.pointToWorldFrame transform point
            in
                extend (AABB p p)
        )
        impossible
        convexPolyhedron.vertices


toHalfExtends : AABB -> Vec3
toHalfExtends { lowerBound, upperBound } =
    lowerBound
        |> Vec3.sub upperBound
        |> Vec3.scale 0.5


plane : Transform -> AABB
plane { position, quaternion } =
    case Vec3.toTuple (Quaternion.rotate quaternion Vec3.k) of
        ( 1, _, _ ) ->
            { maximum | upperBound = vec3 (Vec3.getX position) Const.maxNumber Const.maxNumber }

        ( -1, _, _ ) ->
            { maximum | lowerBound = vec3 (Vec3.getX position) -Const.maxNumber -Const.maxNumber }

        ( _, 1, _ ) ->
            { maximum | upperBound = vec3 Const.maxNumber (Vec3.getY position) Const.maxNumber }

        ( _, -1, _ ) ->
            { maximum | lowerBound = vec3 -Const.maxNumber (Vec3.getY position) -Const.maxNumber }

        ( _, _, 1 ) ->
            { maximum | upperBound = vec3 Const.maxNumber Const.maxNumber (Vec3.getZ position) }

        ( _, _, -1 ) ->
            { maximum | lowerBound = vec3 -Const.maxNumber -Const.maxNumber (Vec3.getZ position) }

        ( _, _, _ ) ->
            maximum


sphere : Float -> Transform -> AABB
sphere radius { position } =
    let
        ( xc, yc, zc ) =
            Vec3.toTuple position
    in
        { lowerBound = vec3 (xc - radius) (yc - radius) (zc - radius)
        , upperBound = vec3 (xc + radius) (yc + radius) (zc + radius)
        }
