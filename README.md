WORDLE-SOLVER
=============

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



 [1]: https://www.powerlanguage.co.uk/wordle/
 [2]: https://www.wordgamedictionary.com/word-lists/
