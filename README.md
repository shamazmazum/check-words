check-words
==========

This is a tool which is helpful in learning foreign words. You can create a text
file with words and their translations in the following format:

    <word> === <translation>
    # A line starting with # is a comment
    { <word1> = <translation1> ; .... ; <wordN = translationN> }
    # There is a group of words on the previous line
    { <annotation>; <word1> = <translation1> ; .... ; <wordN = translationN> }
    # This is an annotated group.
    # The annotation will be printed as is without any questions.

You can use any amount of equals signs to separate a word from its
translation. Then you can call `check-words:check-dictionary` with the name of
that file. It will shuffle word-translation pairs in the dictionary, print a
translation and ask you for a corresponding word for each pair. Pairs which form
a group (enclosured in braces) are not shuffled and printed in that order in
which they appear in the dictionary.

You can also make a standalone console application running `make` from this
directory. Currently, this works only with sbcl.
