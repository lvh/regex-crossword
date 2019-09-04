# regex-crossword

I'm too lazy to do https://regexcrossword.com/ so I taught a computer how to do
them instead.

Here's how it works:

1. Inspect the crossword and figure out how many variables need to be filled in
2. For each regex, generate a bunch of matching strings
3. Constrain associated variables to generated values

There are many flaws to this approach but this is the thing I could get going
quickly.

## Installation

Download from https://github.com/lvh/regex-crossword

## Usage

FIXME: explanation

Run the project directly:

    $ clj -m lvh.regex-crossword

Run the project's tests (they'll fail until you edit them):

    $ clj -A:test

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

* This is woefully inefficient: I could also directly constrain some lvars using
  the parse tree of the regex.

* This doesn't know how to deal with backrefs, since backrefs are constraints
  across lvars. (Of course the logic engine knows how to do that, I just haven't
  implemented it yet. The real trick is getting the regex part to grok it --
  that will probably involve using the regex parse tree to directly constrain
  lvars, as above.)

## License

Copyright © Laurens Van Houtven

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
