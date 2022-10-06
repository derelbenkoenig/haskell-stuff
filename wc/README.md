# `wc` with dependent types. 

## Counting by modes, monoidally
The idea is that there are multiple `CountMode`s (i.e. count by lines, words, etc.), and for each mode `m` there is a `CountBy m` type associated with it, which has an instance of the `CountModeC` class. That class requires a method to convert from a char, combine them together monoidally (it just subclasses `Monoid`), and spit out a final result. Some of the monoid instances are trivial: to count by chars, you just add up the number of chars; to count by lines, you just filter for only those chars that are `\n` and count up those. Others are trickier; counting by words requires doing some juggling to remember whether you're appending two fragments together in the middle of the word, and to avoid double-counting that word. The algorithm to do this I essentially lifted from https://github.com/ChrisPenner/wc. That project doesn't implement the `MaxLineLength` mode, so I had to implement that one myself, but the insight for how to do it was largely inspired by how they did the word counting. 

## Counting by multiple modes at once, with a product type
However, we also need a way to do multiple counts in tandem while traversing the input text only once. So therefore, what we essentially need is a way to dynamically define all possible _product types_ of those `CountBy` instances, and a `CountModeC` instance for each of those product types. 

## Depending on a runtime value to choose the type
We don't want to always count by all of the modes. The user at runtime will pass command line arguments to say which counts they care about, and we should only count those. So, we need a way to take a `[CountMode]` _at runtime_ and choose the corresponding _type_ by which to count. That's where `singletons` comes in. And, because a little extra work is needed to convince GHC that all the constraints are satisfied for all possible inputs, `exinst` and `constraints`.

 I'm sure even more clever use of singletons could be used to de-duplicate elements of that input list and only do each count once, but then "re-duplicate"(?) those results if they were asked for more than once... but in fact, that's not what `wc` does anyway. It prints each thing you ask for exactly once, regardless of how many times you specified the option, and it also does it in a fixed order, regardless of the order in which you specified them. That essentially amounts to gathering the values into an ordered `Set`, and then converting that set back to an ascending list, so that's what I'm doing (with a regular old term-level `Set`, rather than at the type level).

### Also, parsing command line options
This is by no means the most interesting part of the code but I did also use this as an opportunity to try out `optparse-applicative`, which is cool too. 
