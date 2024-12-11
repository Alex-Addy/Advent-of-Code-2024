# Takes a string of numbers separated by spaces and /n, returns a [[n]]
ParseString ← (
  ⊜(□⊜⋕⊸≠@ )⊸≠@\n
)

# For each row that is monotonically changing place a 1 in that rows position, 0 otherwise
FindMonotonicallyChanging ← (
  ⍚⧈-        # get the difference between each subsequent item
  ⊃(=¯1|=1)± # create two arrays, one for increasing, other for decreasing
  ∩⍚/×       # multiply each rows values together, getting a 1 only for rows that were consistent
  +          # add the arrays together, getting 1 for each row that was monotonic
)
⍤⤙≍ {1 1 0} FindMonotonicallyChanging {[1 2 3] [3 2 1] [1 3 2]}

FindChangesWithinRange ← (
  ⍚⧈-       # get diff between terms
  ⍚≡⌵       # get abs value for all rows
  ×⊃(≥1|≤3) # find values within range
  ⍚/×       # collapse matching rows
)
⍤⤙≍ {1 1 0} FindChangesWithinRange {[0 1 2] [3 0 ¯1] [7 0 4]}

ParseString &fras "test.txt"
FindMonotonicallyChanging .
FindChangesWithinRange :

°□/+ × # find rows that satisfy all conditions, count occurrences
$"Solution to part 1 test: _"

ParseString &fras "input.txt"
FindMonotonicallyChanging .
FindChangesWithinRange :

°□/+ ×
$"Solution to part 1: _"
