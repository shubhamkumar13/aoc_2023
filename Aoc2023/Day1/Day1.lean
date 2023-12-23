def dedup [BEq a] (lst : List a) : List a :=
  match lst with
  | [] => []
  | x :: xs => x :: List.filter (fun y => x != y) (dedup xs)

def CharToString (ch : Char) : String :=
  String.str "" ch

def removeEmptyStrings (lst : List String) : List String :=
  List.filter (fun s => s != "") lst

def splitAtDigitsHelper
  (state : String × List String)
  (ch : Char) : String × List String :=
  let (word, lst) := state
  if Char.isDigit ch then
    -- if character digit found change word to empty string
    -- append character as a string to the list
    -- append it before the word that is generated
    ("", List.append lst [ word, CharToString ch ])
  else
    -- if character not a digit keep appending character to the word
    (String.append word (CharToString ch), lst)

def splitAtDigits (s : String) : List String :=
  let initState := ("", [])
  String.foldl splitAtDigitsHelper initState s
  |> fun (first, lst) => List.append lst [first]
  |> removeEmptyStrings

def isStringDigit (s : String) : Bool :=
  match String.toNat? s with
  | none => false
  | some _ => true

def filterListStringDigit (lst : List String) : List String :=
  List.filter isStringDigit lst

def eliminateAllExceptFirstLast (lst : List String) : List String :=
  let first := List.head? lst
  let last := List.getLast? lst
  match first, last with
  | none, none => []
  | some first, some last => [first, last]
  | _, _ => []

def listToNat (n : Nat) (lst : List String) : Nat :=
  let stringToNat s :=
    match String.toNat? s with
    | none => 0
    | some n => n

  match lst with
  | [] => n
  | hd :: tl => listToNat (10 * n + stringToNat hd) tl

def getNumbers (lst : List String) : List Nat :=
  List.map (fun s =>
   splitAtDigits s |>
   filterListStringDigit |>
   eliminateAllExceptFirstLast |>
   (listToNat 0)) lst

def toSum (lst : List Nat) : Nat :=
  List.foldl (fun acc n => acc + n) 0 lst

def wordToNumber s :=
  match s with
  | "one" => "1"
  | "two" => "2"
  | "three" => "3"
  | "four" => "4"
  | "five" => "5"
  | "six" => "6"
  | "seven" => "7"
  | "eight" => "8"
  | "nine" => "9"
  | _ => s

def firstPass (listOfList : List (List String)) : List (List String) :=
  List.map (List.map wordToNumber) listOfList

def substringExtract (start : Nat) (finish : Nat) (s : String) : String  :=
  let start := String.Pos.mk start
  let finish := String.Pos.mk finish
  let substring := String.toSubstring s
  Substring.extract substring start finish
  |> Substring.toString

def getRest (at_ : Nat) (s : String) : String :=
  let at_ := String.Pos.mk at_
  let last := String.Pos.mk (String.length s)
  let substring := String.toSubstring s
  Substring.extract substring at_ last
  |> Substring.toString

def getWord (s : String) (startIndex : Nat) (endIndex : Nat) : String :=
  substringExtract startIndex endIndex s

def getNDigitWord (s : String) (index : Nat) (N : Nat) : String :=
  getWord s index (index + N) |> wordToNumber

def getNDigitIndexedWords (s : String) (N : Nat) : List (Nat × String) :=
  List.range (String.length s - 1)
  |> List.map (fun index => (index, getNDigitWord s index N))
  |> List.filter (fun (_, word) => isStringDigit word)

def get3DigitIndexWords (s : String) : List (Nat × String) :=
  getNDigitIndexedWords s 3

def get4DigitIndexWords (s : String) : List (Nat × String) :=
  getNDigitIndexedWords s 4

def get5DigitIndexWords (s : String) : List (Nat × String) :=
  getNDigitIndexedWords s 5

def concatDigitsInOrder (s : String) : List String :=
  let indexedThreeDigitWords := get3DigitIndexWords s
  let indexedFourDigitWords := get4DigitIndexWords s
  let indexedFiveDigitWords := get5DigitIndexWords s
  let indexedDigitWords := indexedThreeDigitWords ++ indexedFourDigitWords ++ indexedFiveDigitWords

  List.toArray indexedDigitWords
  |> flip Array.qsort (fun (a, _) (b, _) => a < b)
  |> Array.toList
  |> List.map Prod.snd

def getFirstAndLastOfList? (lst : List α) : Option (α × α) :=
  match List.head? lst, List.getLast? lst with
  | some x, some y => some (x, y)
  | _, _ => none

def secondPass (listOfList : List (List String)) : Nat :=
  let f acc digit :=
    if isStringDigit digit then
      acc ++ [digit]
    else
      acc ++ concatDigitsInOrder digit |> dedup

  let sum acc x :=
    let first := String.toNat? (Prod.fst x)
    let second := String.toNat? (Prod.snd x)

    match first, second with
    | some x, some y => acc + (10 * x + y)
    | _, _ => acc

  listOfList
  |> List.map (List.foldl f [])
  |> List.filterMap getFirstAndLastOfList?
  |> List.foldl sum 0

def part1Fn (path : String) : IO Nat := do
  let input <- IO.FS.readFile path
  pure (String.splitOn input "\n" |> getNumbers |> toSum)

def part2Fn (path : String) : IO Nat := do
  let input <- IO.FS.readFile path
  String.splitOn input "\n"
  |> List.map splitAtDigits
  |> firstPass
  |> secondPass
  |> pure
