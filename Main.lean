import «Aoc2023»

def day1 (_ : Unit) : IO Unit := do
  let input := "/home/sk/aoc_2023/Aoc2023/Day1/input.txt"
  let part1 <- part1Fn input
  let part2 <- part2Fn input
  IO.println s!"Day1 part1 = {part1}"
    -- day1 part2 is showing incorrect answer I don't know why
  IO.println s!"Day1 part2 = {part2}"

def main : IO Unit :=
  day1 ()
