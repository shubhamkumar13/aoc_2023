import Lake
open Lake DSL

package «aoc_2023» where
  -- add package configuration options here

lean_lib «Aoc2023» where
  -- add library configuration options here

require std from git "https://github.com/leanprover/std4" @ "main"

@[default_target]
lean_exe «aoc_2023» where
  root := `Main
