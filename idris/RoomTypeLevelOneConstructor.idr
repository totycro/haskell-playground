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


rOpen : Room String Open (List Integer)
rOpen = MkRoom (MkKey "abc") Open [1, 2]

rLocked : Room String Locked (List Integer)
rLocked = MkRoom (MkKey "foobar") Locked [5, 8]

unlockRoom : Key tk -> Room tk Locked ty -> Maybe (Room tk Closed ty)
unlockRoom givenKey (MkRoom roomKey Locked x) = case (givenKey == roomKey) of
                                                     True => Just $ MkRoom roomKey Closed x
                                                     False => Nothing

-- this gives a type error as desired
-- testUnlockOpen = unlockRoom (MkKey 123) rOpen

-- this also gives a type error as desired
--testUnlockLockedWrongKeyType = unlockRoom (MkKey []) rLocked

-- this gives nothing (wrong key)
testUnlockLockedIncorrect = unlockRoom (MkKey "wrong") rLocked

-- actually unlock
testUnlockLockedCorrect = unlockRoom (MkKey "foobar") rLocked

