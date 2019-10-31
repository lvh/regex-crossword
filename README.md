# regex-crossword

I'm too lazy to do https://regexcrossword.com/ so I taught a computer how to do
them instead.

It works by parsing the regular expression and recursively translating parts of
the regular expression to logic constraints, and then asking a solver to find
the answer(s).

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

* This doesn't know how to deal with backrefs (yet), since backrefs are
  constraints across lvars. (The logic engine knows how to do that, I just
  haven't implemented it yet. The real trick is getting the regex part to grok
  it -- that will probably involve using the regex parse tree to directly
  constrain lvars, as above.)

## License

Copyright © Laurens Van Houtven

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
