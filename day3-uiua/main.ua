DoMath ← (
  regex $ mul\((\d{1,3}),(\d{1,3})\)
  ⍚↘ 1
  ⋕
  ≡/×
  /+
)

$"Solution to part 1 test: _" DoMath &fras "test.txt"
$"Solution to part 1: _" DoMath &fras "input.txt"

PartDos ← (
  ⬚""regex $ (mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\))
  ≡↘1
  ⊸⊃($do ≡(≍□"do()"⊢)|$dont ≡(≍□"don't()"⊢)) # get two arrays indicating where do and don't strings are
  ⊙⊙≡(⬚0⋕↘1)                                 # parse mul args into numbers
  ⊂0 : ⊂ 1                                   # add a 1 and 0 to the start of the do and don't arrays to make the next bit esier
  ↘1\(↧1↥0+)-                                # subtract don't from do, then scan to propagate areas of do-ness
  ▽                                          # get only rows that are enabled
  /+≡/×
)

$"Solution to part 2 test: _" PartDos &fras "test.txt"
$"Solution to part 2: _" PartDos &fras "input.txt"
