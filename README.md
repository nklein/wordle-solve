WORDLE-SOLVE
============

This is meant to take in a list of words and play the game [Wordle][1] (in hard mode).

For example, here is how you might run it for a game where the answer is `"bigly"`:

    (defparameter *w* (read-words "/path/to/dictionary/with/one/word/per/line"))

    (guess *w*) => "pares"

Note: Your guesses will differ depending on what words are in your word list.
I was using the list of five letter words provided at [Word Game Dictionary][2].

Entering this guess in the game results in all letters marked black.
So, then filtering on the new information:

    (guess (filter *w* '("pares" "bbbbb"))) => "cunit"

Entering this guess results in the `i` marked yellow while everything else is marked black.
So, then filtering on that information, too:

    (guess (filter *w* '("pares" "bbbbb")
                       '("cunit" "bbbyb"))) => "dilly"

Entering this guess results in the `i`, the second `l`, and the `y` marked green while the other letters are marked black.
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

The method used here is multi-step.

First, we read in the dictionary keeping only the five-letter, all-lowercase words.

For our purposes, let's assume the dictionary is these four words:
`"aaabb"`, `"ababa"`, `"bbaaa"`, and `"aaabc"`.

Now, it is time to make our first guess.

We go through each letter position (1st, 2nd, 3rd, 4th, 5th) and
determine the Shannon entropy of a given letter showing up in that position.
In our dictionary, the letter `b` appears in the 2nd position half the time so the
entropy of a `b` in the 2nd position is -1/2 log(1/2).
The entropy of an `a` in the 3rd position is zero since all words have an `a` in that position.

Next, we go through each word in the dictionary and sort the letters to keep track of the multiset of letters
that make up this word.
And, we determine the Shannon entropy of each multiset.
For our dictionary, the multiset `{ 3a + 2b }` would have entropy -3/4 log(3/4)
and the multiset `{ 3a + b + c }` would have entropy -1/4 log(1/4).

Then, our guess it the word in the dictionary that maximizes the total entropy gained from the position of its
letters and its multiset. For example, the word `"aaabc"` would have be the sum of the following:

  * 1st position: -3/4 log(3/4)
  * 2nd position: -1/2 log(1/2)
  * 3rd position: 0
  * 4th position: -3/4 log(3/4)
  * 5th position: -1/4 log(1/4)
  * multiset: -1/4 log(1/4)

The word with the maximum entropy is our guess.

Once we see how well the guess matched, we can filter down the dictionary to only those
words that are still possible and guess again (including recalculating the entropies)
from the new, smaller dictionary.
