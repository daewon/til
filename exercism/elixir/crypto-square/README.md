# Crypto Square

Implement the classic method for composing secret messages called a square code.

The input is first normalized: The spaces and punctuation are removed
from the English text and the message is downcased.

Then, the normalized characters are broken into rows.  These rows can be
regarded as forming a rectangle when printed with intervening newlines.

For example, the sentence

> If man was meant to stay on the ground god would have given us roots

is 54 characters long.

Broken into 8-character columns, it yields 7 rows.

Those 7 rows produce this rectangle when printed one per line:

```plain
ifmanwas
meanttos
tayonthe
groundgo
dwouldha
vegivenu
sroots
```

The coded message is obtained by reading down the columns going left to
right.

For example, the message above is coded as:

```plain
imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn sseoau
```

Write a program that, given an English text, outputs the encoded version
of that text.

The size of the square (number of columns) should be decided by the
length of the message.

If the message is a length that creates a perfect square (e.g. 4, 9, 16,
25, 36, etc), use that number of columns.

If the message doesn't fit neatly into a square, choose the number of
columns that corresponds to the smallest square that is larger than the
number of characters in the message.

For example, a message 4 characters long should use a 2 x 2 square. A
message that is 81 characters long would use a square that is 9 columns
wide.

A message between 5 and 8 characters long should use a rectangle 3
characters wide.

Output the encoded text grouped by column.

For example:

- "Have a nice day. Feed the dog & chill out!"
  - Normalizes to: "haveanicedayfeedthedogchillout"
  - Which has length: 30
  - And splits into 5 6-character rows:
    - "havean"
    - "iceday"
    - "feedth"
    - "edogch"
    - "illout"
  - Which yields a ciphertext beginning: "hifei acedl v…"

## Running tests

Execute the tests with:

```bash
$ elixir bob_test.exs
```

(Replace `bob_test.exs` with the name of the test file.)


### Pending tests

In the test suites, all but the first test have been skipped.

Once you get a test passing, you can unskip the next one by
commenting out the relevant `@tag :pending` with a `#` symbol.

For example:

```elixir
# @tag :pending
test "shouting" do
  assert Bob.hey("WATCH OUT!") == "Whoa, chill out!"
end
```

Or, you can enable all the tests by commenting out the
`ExUnit.configure` line in the test suite.

```elixir
# ExUnit.configure exclude: :pending, trace: true
```

For more detailed information about the Elixir track, please
see the [help page](http://exercism.io/languages/elixir).

## Source

J Dalbey's Programming Practice problems [http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html](http://users.csc.calpoly.edu/~jdalbey/103/Projects/ProgrammingPractice.html)
