import Door

data Room DoorState ty = MkRoom ty


f : Room DoorState ty -> ty
f (MkRoom x) = ?x

data CanEnter : Room ds ty -> Type where
  CanEnterOpen : (r : Room Open _) -> CanEnter r
  --CanEnterOpen2 : CanEnter ?b
  -- CanEnterRest : (room) -> Void

testCanEnterOpen = CanEnter (rOpen)
-- this gives a type error as desired:
testCanEnterClosed = CanEnter (rClosed)

-- doesn't work like this, check example somehwere 
enterRoom2 : (CanEnter r) => (r : Room _ ty) -> ty
enterRoom2 (RoomOpen x) = x
enterRoom2 _ = ?a

-- testEnterRoom2Open = enterRoom2 rOpen
--testEnterRoom2Closed = enterRoom2 rClosed




