inductive Color where
  | red : Color
  | green : Color
  | blue : Color
deriving Repr

structure Event where
  color : Color
  number : Nat
  possible : Bool
deriving Repr

abbrev SuperEvent := List Event
abbrev SuperEventList := List SuperEvent

structure Game where
  id : Nat
  events : SuperEventList
deriving Repr

def mkEventHelper (color : String) (number : String) : Option Event :=
  match color with
  | "red" => some {color := Color.red, number := number.toNat!, possible := number.toNat! <= 12}
  | "green" => some {color := Color.blue, number := number.toNat!, possible := number.toNat! <= 13}
  | "blue" => some {color := Color.green, number := number.toNat!, possible := number.toNat! <= 14}
  | _ => none

def mkEvent? (line : String) : Option Event :=
  match line.splitOn with
  | number :: color :: _ => mkEventHelper color number
  | _ => none

def isEventListPossible (e : SuperEvent) : Bool := e.foldl
  (init := true) (fun acc x => acc && x.possible)

def isGamePossible (lst : SuperEventList) : Bool := lst.foldl
  (init := true) (fun acc x => acc && isEventListPossible x)

def mkSuperEventList (s : String) : SuperEventList :=
  let parseSemicolon events : List String := events.trim.splitOn ";" |>.map String.trim
  let parseComma s := s.splitOn "," |>.map String.trim
  let toSuperEvent : List String → SuperEvent := .filterMap mkEvent?
  parseSemicolon s |>.map (toSuperEvent ∘ parseComma)

def mkId (gameLine : String) : Nat :=
  gameLine.splitOn ":" |>.head!.trim.splitOn " " |>.drop 1 |>.head!.trim.toNat!

def mkGame? (gameLine : String) : Option Game :=
  let id := mkId gameLine
  let events := gameLine.splitOn ":" |>.drop 1 |>.head! |>.trim
  let superEventList := mkSuperEventList events
  if isGamePossible superEventList then
    some {id := id, events := superEventList}
  else none

def mkGames (text : String) : List Game :=
  text.splitOn "\n" |>.filterMap mkGame?

def day2Part1Fn (path : String) : IO Nat := do
  let input <- IO.FS.readFile path
  return mkGames input
    |>.foldl (init := 0) (· + ·.id)
