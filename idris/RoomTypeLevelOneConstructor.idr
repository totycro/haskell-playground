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
  MkRoom : Key tk -> (ds: DoorState) -> ty -> Room tk ds ty

keyFits : Key tk -> Room tk ds ty -> Bool
keyFits givenKey (MkRoom roomKey _ _) = roomKey == givenKey

rOpen : Room String Open (List Integer)
rOpen = MkRoom (MkKey "abc") Open [1, 2]

rLocked : Room String Locked (List Integer)
rLocked = MkRoom (MkKey "foobar") Locked [5, 8]

unlockRoom : Key tk -> Room tk Locked ty -> Maybe (Room tk Closed ty)
unlockRoom givenKey room = case keyFits givenKey room of
                                True => Just $ doUnlock room
                                False => Nothing
                           where
                              doUnlock : Room tk Locked ty2 -> Room tk Closed ty2
                              doUnlock (MkRoom roomKey _ x) = (MkRoom roomKey _ x)



-- this gives a type error as desired
-- testUnlockOpen = unlockRoom (MkKey 123) rOpen

-- this also gives a type error as desired
--testUnlockLockedWrongKeyType = unlockRoom (MkKey []) rLocked

-- this gives nothing (wrong key)
testUnlockLockedIncorrect = unlockRoom (MkKey "wrong") rLocked

-- actually unlock
testUnlockLockedCorrect = unlockRoom (MkKey "foobar") rLocked


describeLockState : Room tk ds ty -> String
describeLockState (MkRoom key Open contents) = "It's open"
describeLockState (MkRoom key Closed contents) = "It's closed, but not locked"
describeLockState (MkRoom key Locked contents) = "Locked, gonna need a key"


data CanEnter : DoorState -> Type where
     CanEnterOpen : CanEnter Open
     CanEnterClosed : CanEnter Closed

getStuffFromRoom : {auto _ : CanEnter ds} -> (Room tk ds ty) -> ty
getStuffFromRoom (MkRoom key ds x) = x

testGetStuffOpen : List Integer
testGetStuffOpen = getStuffFromRoom rOpen

-- this does not type check as desired
-- testGetStuffLocked : List Integer
-- testGetStuffLocked = getStuffFromRoom rLocked

