import Std.Data.HashMap

def ex := "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

def filterNoise (lst : List String) : List String :=
  let filterChars := fun
    | ';' | ':' | ',' => none
    | ch => some ch
  let filterString (str : String) : String :=
    let charList : List Char := (str.toList).filterMap filterChars
    charList.foldl (init := "") String.push
  lst.map filterString

def parseLine (line : String)  :=
  line.splitOn |> filterNoise

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

structure Game where
  id : Nat
  events : List Event
deriving Repr

def mkEvent? (color : String) (number : String) : Option Event :=
  match color with
  | "red" => some {color := Color.red, number := number.toNat!, possible := number.toNat! <= 12}
  | "blue" => some {color := Color.blue, number := number.toNat!, possible := number.toNat! <= 13}
  | "green" => some {color := Color.green, number := number.toNat!, possible := number.toNat! <= 14}
  | _ => none

def toColor? (line : String) : Option Event :=
  match line.splitOn with
  | number :: color :: _ => mkEvent? color number
  | _ => none

def mkID (gameID : String) :=
  gameID.trim.splitOn |>.tail! |>.head! |>.toNat!

def mkGame (events : String) : List (List Event) :=
  let parseSemicolon events : List String := events.trim.splitOn ";" |>.map String.trim
  let parseComma s := s.splitOn "," |>.map String.trim
  let validColors : List String → List Event := .filterMap toColor?
  parseSemicolon events |>.map (validColors ∘ parseComma)

def isGamePossible (game : Game) :=
  game.events

α

def f (line : String) : Game :=
  let lst := line.splitOn ":"
  {id := mkID (lst.head!), events := List.concat mkGame (lst.tail!.head!)}

#eval f ex
  -- |> String.trim
  -- |>.splitOn ";" |>.map String.trim |>.map (fun s => s.splitOn "," |> List.map String.trim)
  -- |>.map (fun lst => lst.filterMap toColor?)

#eval List.map (String.map (fun ch =>
  match ch with
  | ':' => ' '
  | ',' => ' '
  | ';' => ' '
  | _ => ch
)) (ex.splitOn " ")
