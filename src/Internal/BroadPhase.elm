module Internal.BroadPhase exposing (addContacts, addContacts2)

{-| This is very naive implementation of BroadPhase,
that checks if the bounding spheres of each two bodies overlap
-}

import Internal.Body as Body exposing (Body)
import Internal.Contact exposing (ContactGroup)
import Internal.NarrowPhase as NarrowPhase
import Internal.Shape exposing (Shape(..))
import Internal.Transform3d as Transform3d
import Internal.Vector3 as Vec3
import Internal.World exposing (World)


addContacts : World data -> World data
addContacts world =
    { world
        | contactGroups =
            case updateHeightmapCaches world.bodies of
                body :: restBodies ->
                    addContactsHelp body restBodies restBodies []

                [] ->
                    []
    }


updateHeightmapCaches : List (Body data) -> List (Body data)
updateHeightmapCaches bodies =
    List.map (updateHeightmapCachesHelp bodies) bodies


updateHeightmapCachesHelp : List (Body data) -> Body data -> Body data
updateHeightmapCachesHelp otherBodies body =
    case body.heightmap of
        Just function ->
            -- Do some stuff
            body

        _ ->
            body


{-| This will generate all pairs for body1, then all pairs for body2, etc.
We rely on this order in the Solver.elm
-}
addContactsHelp : Body data -> List (Body data) -> List (Body data) -> List (ContactGroup data) -> List (ContactGroup data)
addContactsHelp body1 currentBodies restBodies result =
    case restBodies of
        body2 :: newRestBodies ->
            addContactsHelp
                body1
                currentBodies
                newRestBodies
                (if bodiesMayContact body1 body2 then
                    case
                        NarrowPhase.getContacts
                            body1.worldShapes
                            body2.worldShapes
                    of
                        [] ->
                            result

                        contacts ->
                            { body1 = body1
                            , body2 = body2
                            , contacts = contacts
                            }
                                :: result

                 else
                    result
                )

        [] ->
            case currentBodies of
                newBody1 :: newRestBodies ->
                    addContactsHelp
                        newBody1
                        newRestBodies
                        newRestBodies
                        result

                [] ->
                    result


bodiesMayContact : Body data -> Body data -> Bool
bodiesMayContact body1 body2 =
    let
        boundingRadiuses =
            body1.boundingSphereRadius + body2.boundingSphereRadius

        distanceSquared =
            Vec3.distanceSquared
                (Transform3d.originPoint body1.transform3d)
                (Transform3d.originPoint body2.transform3d)
    in
    (boundingRadiuses * boundingRadiuses - distanceSquared > 0)
        && (body1.mass + body2.mass /= 0)


addContacts2 : World data -> World data
addContacts2 world =
    let
        ( newBodies, contactGroups ) =
            case world.bodies of
                body :: restBodies ->
                    addContactsHelp1 body restBodies ( [], [] )

                [] ->
                    ( [], [] )
    in
    { world | bodies = newBodies, contactGroups = contactGroups }


addContactsHelp1 :
    Body data
    -> List (Body data)
    -> ( List (Body data), List (ContactGroup data) )
    -> ( List (Body data), List (ContactGroup data) )
addContactsHelp1 first next ( bodiesProcessed, contactsFound ) =
    let
        ( first_, next_, contacts ) =
            addContactsHelp2 first next
    in
    case next_ of
        second :: rest ->
            addContactsHelp1 second rest ( first_ :: bodiesProcessed, contactsFound ++ contacts )

        [] ->
            ( bodiesProcessed, contactsFound )


addContactsHelp2 : Body data -> List (Body data) -> ( Body data, List (Body data), List (ContactGroup data) )
addContactsHelp2 body1 bodiesToProcess =
    List.foldr
        (\body2 ( body1_, bodies, contacts ) ->
            if bodiesMayContact body1_ body2 then
                case NarrowPhase.getContacts body1_.worldShapes body2.worldShapes of
                    [] ->
                        ( body1_, body2 :: bodies, contacts )

                    contacts_ ->
                        ( body1_
                        , body2 :: bodies
                        , { body1 = body1
                          , body2 = body2
                          , contacts = contacts_
                          }
                            :: contacts
                        )

            else
                ( body1_, body2 :: bodies, contacts )
        )
        ( body1, [], [] )
        bodiesToProcess
