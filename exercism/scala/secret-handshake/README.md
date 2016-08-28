# Secret Handshake

Write a program that will take a decimal number, and convert it to the appropriate sequence of events for a secret handshake.

> There are 10 types of people in the world: Those who understand
> binary, and those who don't.

You and your fellow cohort of those in the "know" when it comes to
binary decide to come up with a secret "handshake".

```
1 = wink
10 = double blink
100 = close your eyes
1000 = jump


10000 = Reverse the order of the operations in the secret handshake.
```

Here's a couple of examples:

Given the input 9, the function would return the array
["wink", "jump"]

Given the input "11001", the function would return the array
["jump", "wink"]


The program should consider strings specifying an invalid binary as the
value 0.

The Scala exercises assume an SBT project scheme. The exercise solution source
should be placed within the exercise directory/src/main/scala. The exercise
unit tests can be found within the exercise directory/src/test/scala.

To run the tests simply run the command `sbt test` in the exercise directory.

For more detailed info about the Scala track see the [help
page](http://exercism.io/languages/scala).

## Source

Bert, in Mary Poppins [http://www.imdb.com/character/ch0011238/quotes](http://www.imdb.com/character/ch0011238/quotes)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

