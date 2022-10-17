# `wc` with dependent types.

See also: https://github.com/ChrisPenner/wc for a project that does some of this (but not the part
with dependent types)

## Counting by modes, monoidally

`wc` itself gives five different options for how to "count" the input text. Some of them are sort of "stateful"
or at least "context-dependent" in some way, particularly counting the number of words or the maximum line length.
They do, however, all share the property that they can be done in a "monoidal" way. This means using:
1. A data type that keeps around whatever contextual metadata is necessary while counting, and
2. An operation for combining those pieces of information which is associative but not necessarily commutative,
   and which has some "identity" or "neutral element" (representing an "empty" chunk of text).
Imagine taking the text, splitting it into chunks, couting up each chunk, and then combining those counts together.
You only need to keep around a little bit of context, specifically something describing the boundaries of those chunks
(are they in the middle of a word, of a line, etc.). These chunks could then be handled _in parallel_, as long as the
resulting partial results were then joined together in the correct order. This code doesn't actually parallelize, right
now, but it's designed to make that possible.

In my implementation, I define multiple `CountMode`s (i.e. count by lines, words, etc.), and for each mode `m` there is a `CountBy m` type associated with it, which has an instance of the `CountModeC` type class. That class requires a method to convert from a char, combine them together monoidally (it just subclasses `Monoid`), and spit out a final result. Some of the monoid instances are trivial: to count by chars, you just add up the number of chars; to count by lines, you just filter for only those chars that are `\n` and count up those. Others are trickier; counting by words requires doing some juggling to remember whether you're appending two fragments together in the middle of the word, and to avoid double-counting that word. The algorithm to do this I essentially lifted from https://github.com/ChrisPenner/wc. That project doesn't implement the `MaxLineLength` mode, so I had to implement that one myself, but the insight for how to do it was largely inspired by how they did the word counting.

## Counting by multiple modes at once, with a product type
However, we also need a way to do multiple counts in tandem while traversing the input text only once. So therefore, what we essentially need is a way to dynamically define all possible _product types_ of those `CountBy` instances, and a `CountModeC` instance for each of those product types.

## Depending on a runtime value to choose the type
We don't want to always count by all of the modes. The user at runtime will pass command line arguments to say which counts they care about, and we should only count those. So, we need a way to take a `[CountMode]` _at runtime_ and choose the corresponding _type_ by which to count. That's where `singletons` comes in. And, because a little extra work is needed to convince GHC that all the constraints are satisfied for all possible inputs, `exinst` and `constraints`.

 I'm sure even more clever use of singletons could be used to de-duplicate elements of that input list and only do each count once, but then "re-duplicate"(?) those results if they were asked for more than once... but in fact, that's not what `wc` does anyway. It prints each thing you ask for exactly once, regardless of how many times you specified the option, and it also does it in a fixed order, regardless of the order in which you specified them. That essentially amounts to gathering the values into an ordered `Set`, and then converting that set back to an ascending list, so that's what I'm doing (with a regular old term-level `Set`, rather than at the type level).

### Also, parsing command line options
This is by no means the most interesting part of the code but I did also use this as an opportunity to try out `optparse-applicative`, which is cool too.
