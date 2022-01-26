WORDLE-SOLVE v0.4.20220126
==========================

This is meant to take in a list of words and play the game [Wordle][1] (in hard mode).

The simplest way to use this is to create a game iterator and give it a list of words to guess from.

    (defparameter *w* (read-words "/path/to/word/list/one/word/per/line"))

    (defparameter *it* (make-game-iterator *w*))

The `#'make-game-iterator` function can also take a `:guesser` parameter to specify a guesser.
The available guessers are described in the next section.

To make an initial guess, invoke the iterator with no argument:

    (funcall *it*) => "pares"

Note: Your guesses will differ depending on what words are in your word list.
I was using the list of five letter words provided at [Word Game Dictionary][2].

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


For the dictionary of 9330 words that I am using:
  * the average word takes just over 4.79 guesses,
  * the hardest word to guess is `"zills"` which requires 16 guesses, and
  * the first guess is always `"sores"`.

 [1]: https://www.powerlanguage.co.uk/wordle/
 [2]: https://www.wordgamedictionary.com/word-lists/

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

Other valid choices for guessing functions are `#'cl:car` and `#'alexandria:random-elt`.
In fact, those simplistic guessing functions do almost as well as the more computationally
expensive functions above.
Here is how the above algorithms compare using the dictionary from Word Game Dictionary.

Average guesses required to find a word that is in the dictionary:

  * `entropy-guess`: 4.57
  * `elimination-guess`: 5.03
  * `car`: 5.32
  * `random-elt`: 4.90

Maximum guesses required to find a word that is in the dictionary:

  * `entropy-guess`: 15
  * `elimination-guess`: 16
  * `car`: 15
  * `random-elt`: 14

ENTROPY-GUESS IMPLEMENTATION
----------------------------

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

ELIMINATION-GUESS IMPLEMENTATION
--------------------------------

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
