import Door

data Key : Type -> Type where
   MkKey : Eq ty => ty -> Key ty

Eq (Key ty) where
  (==) (MkKey @{useThisEq} k1) (MkKey k2) = (==) @{useThisEq} k1 k2
  -- need to tell idris which Eq instance to use because both
  -- k1 and k2 have one https://stackoverflow.com/a/73488001/4658170

-- own types for every state
-- Key type, DoorState and type of contents are in type
data Room : tk -> DoorState -> ty -> Type where
  RoomOpen : Key tk -> ty -> Room tk Open ty
  RoomClosed : Key tk -> ty -> Room tk Closed ty
  RoomLocked : Key tk -> ty -> Room tk Locked ty

-- just for fun
Functor (Room tk ds) where
  map func (RoomOpen k x) =  RoomOpen k (func x)
  map func (RoomClosed k x) = RoomClosed k (func x)
  map func (RoomLocked k x) = RoomLocked k (func x)

rOpen : Room Nat Open (List Int)
rOpen = RoomOpen (MkKey 3) [1, 2]

rLocked : Room String Locked (List Int)
rLocked = RoomLocked (MkKey "foobar") [5, 8]

unlockRoom : Key tk -> Room tk Locked ty -> Maybe (Room tk Closed ty)
unlockRoom givenKey (RoomLocked roomKey x) = case (givenKey == roomKey) of
                                                  True => Just $ RoomClosed roomKey x
                                                  False => Nothing

-- this gives a type error as desired
-- testUnlockOpen = unlockRoom (MkKey 123) rOpen

-- this also gives a type error as desired
--testUnlockLockedWrongKeyType = unlockRoom (MkKey []) rLocked

-- this gives nothing (wrong key)
testUnlockLockedIncorrect = unlockRoom (MkKey "wrong") rLocked

-- actually unlock
testUnlockLockedCorrect = unlockRoom (MkKey "foobar") rLocked
