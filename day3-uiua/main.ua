DoMath ← (
  regex $ mul\((\d{1,3}),(\d{1,3})\)
  ⍚↘ 1
  ⋕
  ≡/×
  /+
)

$"Solution to part 1 test: _" DoMath &fras "test.txt"
$"Solution to part 1: _" DoMath &fras "input.txt"
