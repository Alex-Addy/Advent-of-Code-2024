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

# Part 2

Check ← (
  ⧈-
  ⊃( # Determine if monotonic
    ⊃(=¯1|=1)±
    +∩/*
  | # Determine if in range
    ≡≡⌵
    ⊃(≥1|≤3)
    /**≡/*
  )
  *
)

⍤⤙≍ 1 Check [1 2 3 4 5 6]
⍤⤙≍ 0 Check [2 1 3]
⍤⤙≍ 0 Check [1 2 7]
⍤⤙≍ 1 Check [¯1 ¯2 ¯4]
⍤⤙≍ 0 Check [2 2 3]

# Check if array would be valid if any one item was removed
CheckRem ← (
  ⧅<¯1
  ≡Check
  /↥
)
⍤⤙≍ 0 CheckRem [1 3 2 12 4 5]
⍤⤙≍ 1 CheckRem [1 3 2 4 5]
⍤⤙≍ 0 Check [1 3 2 4 5]

°□/+ ⍚CheckRem ParseString &fras "test.txt"
$"Solution to part 2 test: _"
°□/+ ⍚CheckRem ParseString &fras "input.txt"
$"Solution to part 2: _"
