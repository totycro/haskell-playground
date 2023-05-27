import Door

-- own types for every sttyte
data Room : DoorState -> ty -> Type where
  RoomOpen : ty -> Room Open ty
  RoomClosed : ty -> Room Closed ty
  RoomLocked : ty -> Room Locked ty

rOpen : Room Open (List Int)
rOpen = RoomOpen [1, 2]

rClosed : Room Closed (List Int)
rClosed = RoomClosed [2, 3]

rLocked : Room Locked (List Int)
rLocked = RoomLocked [5, 8]

enterRoom : Room Open ty -> ty
enterRoom (RoomOpen x) = x


openRoom : Room Closed ty -> Room Open ty
openRoom (RoomClosed x) = RoomOpen x

unlockRoom : Room Locked ty -> Room Closed ty
unlockRoom (RoomLocked x) = RoomClosed x
