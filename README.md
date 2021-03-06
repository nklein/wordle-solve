WORDLE-SOLVE v1.0.20220209
==========================

This is meant to take in a list of words and play the game [Wordle][1] (in hard mode).

The simplest way to use this is to create a game iterator and give it a list of words to guess from.

    (defparameter *w* (read-words "/path/to/word/list/one/word/per/line"))

    (defparameter *it* (make-game-iterator *w*))

The `#'make-game-iterator` function can also take a `:guesser` parameter to specify a guesser.
The available guessers are described in a later section.
Additionally, it can also take an `:answers` parameter which is the subset of the valid guesses
which could also be an answer.
If `:answers` is not specified, then the guessers assume that any word is `WORDS` is a potential
solution to the puzzle.

Wordle has 12,972 words it will accept as a guess.
Only 2314 of those words will ever be the actual target of the puzzle.

To make an initial guess, invoke the iterator with no argument:

    (funcall *it*) => "pares"

Note: Your guesses will differ depending on what words are in your word list.
There are a number of places online that give the list of possible Wordle
targets and the list of acceptable Wordle guesses.

After you type the `pares` into Wordle, it will highlight the letters as either
green, yellow, or black. Call the iterator again passing it a string representation
of the resulting color pattern using `#\g` for green, `#\y` for yellow, and `#\b`
for black.

    (funcall *it* "bbbbb") => "cunit"

Repeat this process until you have found the answer.

    (funcall *it* "bbbyb") => "dilly"
    (funcall *it* "bgbgg") => "bigly"

You can reset the guesser by again invoking it with no arguments or by specifying
that the last guess was completely correct by passing in `"ggggg"`.

Here is an example run looking for a word that is not in the dictionary. If the
word were `"xyzzy"` (and that is not in the dictionary) then a run might look like:

    (funcall *it*) => "pares"
    (funcall *it* "bbbbb") => "cunit"
    (funcall *it* "bbbbb") => "booby"
    (funcall *it* "bbbbg") => NIL

If you want to have finer control over what is happening, you can use the `#'guess`
(or one of the other guesser functions) and `#'filter`
to explicitly manipulate the dictionary between each move.
Using the above dictionary `*w*` trying to guess `"bigly"`, you might do the following:

    (guess *w*) => "pares"

Entering this guess in the game results in all letters marked black.
So, then filtering on the new information:

    (guess (filter *w* '("pares" "bbbbb"))) => "cunit"

Entering this guess results in the `i` marked yellow while everything else is marked black.
So, then filtering on that information, too:

    (guess (filter *w* '("pares" "bbbbb")
                       '("cunit" "bbbyb"))) => "dilly"

Entering this guess results in the `i`, the second `l`, and the `y` marked green while
the other letters are marked black.
So, then filtering on that information, as well:

    (guess (filter *w* '("pares" "bbbbb")
                       '("cunit" "bbbyb")
                       '("dilly" "bgbgg))) => "bigly"

If you already know that the answer is `"bigly"`, then you can check to see the results of a guess:

    (score-guess "dilly" "bigly") => "bgbgg"

Or you can play the whole game out in one step and get the number of guesses it took and the actual
guesses used:

    (play-game *w* "bigly") => 4, (("pares" "bbbbb") ("cunit" "bbbyb") ("dilly" "bgbgg") ("bigly" "ggggg"))


 [1]: https://www.powerlanguage.co.uk/wordle/

ALL EXPORTED FUNCTIONS
----------------------


The `#'READ-WORDS` function takes a pathname and reads the file specified by `PATHNAME`.
It assumes one word per line with no whitespace (beyond the newlines).
It rejects any words which are not five lowercase letters.

    (read-words pathname)

The `#'ENTROPY-GUESS`, `#'ELIMINATION-GUESS`, and `#'GREEDY-GUESS` functions are
guesser functions.
They are described more in the following section.
They each take a list of `WORDS` and an optional number of guesses to `KEEP`.

    (entropy-guess words &key (keep 1) answers)
    (elimination-guess words &key (keep 1) answers)
    (greedy-guess words &key (keep 1) answers)

The `#'GUESS` function is simply a wrapper around a reasonable guesser.
At the moment, it simply calls `#'ENTROPY-GUESS`.

    (guess words &key answers &allow-other-keys)

The `#'FILTER` and `#'FILTER*` functions take a list of `WORDS` and a `GUESS-RESULTS` list.
The `GUESS-RESULTS` list is made up of lists of the form `(GUESS RESULT)` where the
`GUESS` is a string of the guess made and the `RESULT` is a string encoding the colors
that Wordle assigned to this guess.
The `RESULT` must be five letters and made up of the characters `#\g` (to indicate green),
`#\y` (to indicate yellow), and `#\b` (to indicate black).
The functions return the subset of the `WORDS` list that is consistent with all of the guess results so far.

    (filter words &rest guess-results)
    (filter* words guess-results)

The `SCORE-GUESS` function takes a `GUESS` string and a `TARGET` string and returns a string
encoding the colors that Wordle would assign to that `GUESS` if the goal were the `TARGET` string.

    (score-guess guess target)

The `MAKE-GAME-ITERATOR` function takes in an initial `DICTIONARY` list of words.
You can specify a `GUESSER`, an `INITIAL-GUESS`, and a list of `ANSWERS` if desired.
This function returns an iterator function.
When called with no arguments, the iterator function resets to using the entire dictionary and
returns the `INITIAL-GUESS` if specified or the `GUESSER`'s guess from the entire dictionary.
When called with a string argument, the iterator function interprets the argument as an encoding
of the coloring that Wordle gave to the previous guess.
The string argument must be five letters and made up of the characters `#\g` (to indicate green),
`#\y` (to indicate yellow), and `#\b` (to indicate black).
The iterator function then uses this information to filter down the dictionary and ask the
`GUESSER` for a new guess from the filtered dictionary.

    (make-game-iterator dictionary &key (guesser 'guess) initial-guess answers)

The `INITIAL-GUESS` option can make the compute-intensive `#'GREEDY-GUESS` guesser a viable option.
If you use the `"lares"` guess specified above as the `INITIAL-GUESS`,
then you will likely reduce my 9330 word dictionary to just a few hundred words.
With only a few hundred words, the `#'GREEDY-GUESS` will come up with a guess in under a minute.

The `PLAY-GAME` function takes a list of `WORDS` and a `TARGET` word.
You can specify a `GUESSER` and a list of `ANSWERS` if desired.
This function then uses the `GUESSER` to try to guess the `TARGET` from the given `WORDS`.
This function returns two values.
The first value is the number of guesses required for the guesser to find the target or `NIL`
if the guesser failed to find the target.
The second value is a list of `(GUESS RESULT)` lists of the guesses taken
along with the result of playing that guess against the `TARGET` word.

    (play-game words target &optional (guesser 'guess) answers)


METHOD
------

To play the game, this algorithm starts with a dictionary keeping only the
five-letter, all-lowercase words.
It invokes a guessing function to pick one of the words from the dictionary.
From there, it checks the result that would be shown in Wordle:
which letters are green, which are yellow, and which are black.
Then, it eliminates all words from the dictionary that are ruled out by that result.
Now, it invokes the guessing function again with the new, smaller dictionary.

For example, suppose the dictionary is just these five words:
  `"bills"`, `"billy"`, `"skill"`, `"wills"`, and `"willy"`

Let us suppose the target word is `"billy"`
and the guesser chose `"skill"` for its first guess.
Wordle would score that `"bbygy"`.
With this information, we can reduce the dictionary down to only `"billy"` and `"willy"`.
Now, the guesser will be asked to guess again from this smaller dictionary.

The original guessing function written was `#'entropy-guess`.
It tries to apply Shannon entropy to what it means for a word in the dictionary to have
a particular letter in a particular position and what it means for a word in the dictionary
to be made up of the set of letters it is.

The next guessing function written was `#'elimination-guess` which tries to determine
which letters in particular positions would most drastically reduce the size of the
dictionary. It then assumes the information about each letter position is independent
(which, it isn't, for the record).

Other valid choices for guessing functions are `#'cl:car` and `#'alexandria:random-elt`, or
rather wrapping those in a short lambda to ignore keyword arguments:

    (lambda (ws &key &allow-other-keys) (car ws))
    (lambda (ws &key &allow-other-keys) (alexandria:random-elt ws))

In fact, those simplistic guessing functions do almost as well as the more computationally
expensive functions above.
Here is how the above algorithms compare using the Wordle list of acceptable guesses and
the list of Wordle answers.

Average guesses required to find a word that is in the answer list:

  * `entropy-guess`: 3.72
  * `elimination-guess`: 4.08
  * `car`: 4.89
  * `random-elt`: 4.90
  * `greedy-guess`: 3.66

Maximum guesses required to find a word that is in the answer list:

  * `entropy-guess`: 8 (e.g. `"mower"`)
  * `elimination-guess`: 10 (e.g. `"wacky"`)
  * `car`: 13 (e.g. `"wight"`)
  * `random-elt`: 11 (e.g. `"bunny"`) (your results may vary)
  * `greedy-guess`: 8 (e.g. `"goner"`)

There is another guessing function `#'greedy-guess` which brute-forces through the whole
dictionary looking for the word which actually minimizes the expected size of the remaining
dictionary by trying each word against every other word in the dictionary.
It is prohibitively time-consuming.
It takes more than a day to make the first guess with my default dictionary.
That said, the initial guess only depends on the dictionary, so you can precompute it.
You can take advantage of the compute time that I spent and start with the guess:
`"lares"` which is expected to keep only 2.2% of my dictionary (assuming the target
word was chosen by uniform, random selection from the words in my dictionary).
With a smaller dictionary of more human words, the greedy guess is `"tares"`, which
is expected to keep only 2.3% of that smaller dictionary. The even more human guess
`"rates"` does almost as well (expecting to keep only 2.4% of either dictionary).
For the Wordle dictionary, the best first guess it `"raise"`.

### ENTROPY-GUESS IMPLEMENTATION

For this example, let us again suppose the dictionary is just these five words:
  `"bills"`, `"billy"`, `"skill"`, `"wills"`, and `"willy"`

We go through each letter position (1st, 2nd, 3rd, 4th, 5th) and
determine the Shannon entropy of a given letter showing up in that position.
In our dictionary, the letter `b` appears in the 1st position 40% of the time.
entropy of a `b` in the 1st position is -2/5 log(2/5).
The entropy of an `l` in the 4th position is zero since all words have an `l` in that position.

Next, we go through each word in the dictionary and sort the letters to keep track of the multiset of letters
that make up this word.
And, we determine the Shannon entropy of each multiset.
So, for example, the multiset for `"willy"` is just `{ i + 2l + w + y }`.

Then, our guess is the word in the dictionary that maximizes the sum of the entropy gained from the
position of its letters and a multiple of the entropy gained from the multiset.
The multiple used here is the square root of the number of words in the current dictionary.
For example, the word `"billy"` would have be the sum of these six terms:

  * 1st position: -2/5 log(2/5)
  * 2nd position: -4/5 log(4/5)
  * 3rd position: -4/5 log(4/5)
  * 4th position: 0
  * 5th position: -2/5 log(2/5)
  * sqrt(5) * multiset: -sqrt(5)/5 log(1/5)

Note: there are no anagrams in our dictionary, so the multiset factor is the same for
all of the words in our dictionary.

The word with the maximum entropy is our guess. The scaling factor of the square root of the
number of words in the dictionary was empircally chosen to make the contribution from the
multiset about the same as the contribution from the positions. It is definitely a fudge
factor. It has no justification in the mathematics.

### ELIMINATION-GUESS IMPLEMENTATION

For this example, let us again suppose the dictionary is just these five words:
  `"bills"`, `"billy"`, `"skill"`, `"wills"`, and `"willy"`

This guess implementation tries to determine what letters in given positions
would most drastically eliminate words from the dictionary. It does this by
determining how many words would be eliminated by guessing a particular letter
in a particular position as if Wordle let you enter a letter in any position
and then immediately colored it green, yellow, or black.

The math is done in terms of what percentage of words will be kept from the dictionary
and then inverted (the percent of the dictionary eliminated is 100% minus the
percent of the dictionary retained) at the end.
This is done because it makes the code and equations much easier to read and reason about.

For the above dictionary, let us calculate the value this guesser would give to
the word `"skill"`.

An `s` in the 1st position would be colored green for one word
in the dictionary (`"skill"`), yellow for two more words (`"bills"`, `"wills"`), and
black for the remaining two words (`"billy"`, `"willy"`). So, the expected number
of words retained in the dictionary is the sum of these terms:

 * probability of green * retentions when green: `1/5 * 1/5`
 * probability of yellow * retentions when yellow: `2/5 * 2/5`
 * probability of black * retentions when black: `2/5 * 2/5`

This means that an `s` in the 1st position is expected to result in `9/25`ths of the
dictionary being retained. We can do this for each position in the word `"skill"`:

  * `s` in 1st position: `9/25`
  * `k` in 2nd position: `17/25`
  * `i` in 3rd position: `17/25`
  * `l` in 4th position: `1`
  * `l` in 5th position: `17/25`

Now, we pretend that those probabilities are independent (which they are definitely not).
So, the `s` retains `9/25`ths of the dictionary, the `k` goes on to retain only `17/25`th
of what remains after the `s`. The `i` goes on to retain `17/25`ths of what remains after
the `s` and `k`, etc. This means that the total retention (with the assumption that
the probabilities are independent) is the product of those five positional retentions.

For the word `"skill"`, this then means 11% is expected to be retained.
So, we expect that 89% will be eliminated if we guess `"skill"`.
This is, of course, an overestimate. Much of that is because of our tiny dictionary.
The remainder of that overestimation was our assumption that the probabilities are independent.

### GREEDY-GUESS IMPLEMENTATION

For this example, let us suppose the dictionary is just these five words:
  `"bills"`, `"kills"`, `"pills"`, `"skill"`, and `"twins"`

This guess implementation tries to determine which word in the list of choices
eliminates the most words from the dictionary. The elimination guesser above
did this in a sort of one-letter-at-a-time approach. This does brute-force
try each word in the dictionary as a guess and see which word results in
the smallest expected resulting dictionary.

So, this guess would score the guess `"kills"` as follows.
It would go through each word in the dictionary and assume the word is the target.
It would then find out how many words in the dictionary would get the same
result that `"kills"` did on that target.
It would then average these numbers across all possible targets.

So, for example, if the target were `"kills"`, the result of guessing
`"bills"` would be `"bgggg"`. Getting a result of `"bgggg"` would rule
out `"skill"` and `"bills"` leaving the two words `"kills"` and `"pills"`
in the dictionary. Now, for all possible targets in the dictionary, guessing
`"bills"` leads to:

* target: `"bills"`, result: `"ggggg"`, dictionary: `("bills")`
* target: `"kills"`, result: `"bgggg"`, dictionary: `("kills" "pills")`
* target: `"pills"`, result: `"bgggg"`, dictionary: `("kills" "pills")`
* target: `"skill"`, result: `"byygy"`, dictionary: `("skills")`
* target: `"twins"`, result: `"bybbg"`, dictionary: `("twins")`

So, upon guessing `"bills"`, one would expect the dictionary to be reduced
to 1.4 words, on average. However, if we had guessed `"twins"` we could
only expect the dictionary to be reduced to 2.2 words. So, `"bills"` is
the better guess.

PRECOMPUTING GUESSES
--------------------

If your guesser is deterministic and your dictionary is fixed, you can
precompute an entire guess tree. This trades processor time in exchange
for memory.

    (defparameter *tree* (precompute-guesser-tree words guesser :answers answers))
    (defparameter *it* (make-guess-tree-iterator *tree*))

From there, you can use the iterator just as if you had done:

    (defparameter *it* (make-game-iterator words :guesser guesser :answers answers))
