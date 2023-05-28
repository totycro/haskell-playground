import Door

-- handle state on value level like haskell
data Room a = MkRoom DoorState a

-- encode accessible on object level in function
enterRoom : Room a -> Maybe a
enterRoom (MkRoom Open x) = Just x
enterRoom (MkRoom _ _) = Nothing


-- alternate version where we encode accessible in object level function
data IsAccessible = Accessible | NotAccessible

isAccessible : DoorState -> IsAccessible
isAccessible Open = Accessible
isAccessible Closed = NotAccessible
isAccessible Locked = NotAccessible

enterRoom2 : Room a -> Maybe a
enterRoom2 (MkRoom ds x) = case (isAccessible ds) of
                                Accessible => Just x
                                NotAccessible => Nothing


