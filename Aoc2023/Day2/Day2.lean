inductive Color where
  | red : Color
  | green : Color
  | blue : Color
deriving Repr, BEq

structure Roll where
  color : Color
  number : Nat
deriving Repr

structure Event where
  roll : Roll
  possible : Bool
deriving Repr

abbrev EventList := List Event
abbrev EventListList := List EventList

structure Game where
  id : Nat
  events : EventListList
deriving Repr

def whichColor? : String → Option Color := fun
  | "red"   => some Color.red
  | "green" => some Color.green
  | "blue"  => some Color.blue
  | _       => none

def mkRoll? (color : String) (number : String) : Option Roll :=
  match (whichColor? color), (number.toNat?) with
  | some color, some number => some {color, number}
  | _, _ => none

def updateRollNumber (roll : Roll) (number : Nat) : Roll :=
  {roll with number := number}

def mkEvent? (line : String) : Option Event :=
  let event? color number : Option Event :=
    match mkRoll? color number with
    | some {color, number} =>
      match color with
      | Color.red => some {roll := {color, number}, possible := number <= 12}
      | Color.green => some {roll := {color, number}, possible := number <= 13}
      | Color.blue => some {roll := {color, number}, possible := number <= 14}
    | none => none
  match line.splitOn with
  | number :: color :: _ => event? color number
  | _ => none

def isEventListPossible (e : EventList) : Bool := e.foldl
  (init := true) (fun acc x => acc && x.possible)

def isGamePossible (game : Game) : Bool := game.events.foldl
  (init := true) (fun acc x => acc && isEventListPossible x)

def mkEventListList (s : String) : EventListList :=
  let parseSemicolon events : List String := events.trim.splitOn ";" |>.map String.trim
  let parseComma s := s.splitOn "," |>.map String.trim
  let toSuperEvent : List String → EventList := .filterMap mkEvent?
  parseSemicolon s |>.map (toSuperEvent ∘ parseComma)

def mkId (gameLine : String) : Nat :=
  gameLine.splitOn ":" |>.head!.trim.splitOn " " |>.drop 1 |>.head!.trim.toNat!

def mkGame? (gameLine : String) : Option Game :=
  let id := mkId gameLine
  let events := gameLine.splitOn ":" |>.drop 1 |>.head! |>.trim
  let eventListList := mkEventListList events
  let game := {id := id, events := eventListList}
  if isGamePossible game then some game else none

def day2Part1Fn (path : String) : IO Nat := do
  let input <- IO.FS.readFile path
  return input.splitOn "\n"
    |>.filterMap mkGame?
    |>.foldl (init := 0) (· + ·.id)

def getMaxRolls (rolls : List Roll) : List Roll :=
  let getMax color :=
    rolls.foldl (init := 0) (fun acc roll =>
      match roll.color == color with
      | true => if roll.number > acc then roll.number else acc
      | false => acc) |> fun number => {color, number}

  [getMax Color.red, getMax Color.green, getMax Color.blue]

def rollsNumberProduct (rolls : List Roll) : Nat :=
  match rolls with
  | [] => 1
  | hd :: tl => hd.number * (rollsNumberProduct tl)

def getMaxRollProducts (line : String) : Nat :=
  line.splitOn ":" |>.tail! |>.head! |>.trim
  |>.splitOn ";" |>.map (·.splitOn ",")
  |>.foldl (· ++ ·) []
  |>.filterMap (·.trim.splitOn |> fun | number :: color :: _ => mkRoll? color number | _ => none)
  |> getMaxRolls
  |> rollsNumberProduct

def day2Part2Fn (path : String) : IO Nat := do
  let input <- IO.FS.readFile path
  return input.splitOn "\n"
    |>.map getMaxRollProducts
    |>.foldl (· + ·) 0
