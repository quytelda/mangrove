# mangrove

Mangrove is a library for writing command line argument parsers using
Haskell's Applicative interface.

## A Metaphor

Imagine the roots of a tree. The roots branch out as they grow
downward, eventually dipping into a stream. As the stream flows by,
the roots collect water and nutrients which they pass back up the root
structure. Along the way, the nutrient flows combine and consolidate
until they reach a single source - the plant.

That is roughly how this argument parsing library works. Argument
parsers are expression trees whose leaves are mostly either values or
specific parsers for things like options or subcommands. The leaf
parsers consume and parse inputs they recognize. Once they complete,
they are removed from the tree and replaced with whatever result value
they generated. Meanwhile, nonterminal nodes contain information about
how to combine results from their children. Thus, the tree represents
a kind of suspended computation that can be evaluated once enough
input has been absorbed.

## Example

Suppose we are writing a simple program that creates new user
accounts - we'll call it "mkuser". The goal will be to provide a
command line interface with the following syntax:

```
mkuser [--uid=INT] [--system] [--groups=GROUP...] USERNAME
```

First, let's create a new record that captures the program's runtime
configuration.

```haskell
data Settings = Settings
  { userId     :: Maybe Int -- ^ An optional target user ID
  , userSystem :: Bool -- ^ Is this a system user?
  , userGroups :: [Text] -- ^ Groups the new user will be in
  , userName   :: Text  -- ^ Username for the new user
  } deriving (Show)
```

Let's also pretend that our program's logic lives inside a function
`run :: Settings -> IO ()`. We pass it the settings we want, and it
runs the program accordingly. However, since this is just an example
program, we won't actually create any user accounts; instead we'll
just have the program print its settings to `stdout`.

```haskell
run :: Settings -> IO ()
run = print
```

Now we need to construct a parser that reads a list of arguments and
yields a `Settings`. Our parser will have the type `UnixParser
Settings`. `UnixParser` is just a convenient type synonym for
`ParseTree UnixScheme`, so what we are actually constructing here is
an expression tree, built up by combining "UNIX-flavored" parsers
using the `Applicative` interface.

__Note__: This example uses the language extensions `OverloadedLists`
and `OverloadedStrings` since we need to write lots of `NonEmpty` list
and `Text` literals.

## Parameters

A "parameter" is a positional input that accepts a free argument. For
example, consider a program called `substring` whose command line
syntax is `substring START END STRING`. `START`, `END`, and `STRING`
are parameters. If we invoked `substring 1 3 "example"`, we know that
`START` is `1`, `END` is `3`, and `STRING` is `"example"` because of
the order in which they appear.

Our program will have just one parameter: a username. Here is how we
define a parser for it:

```haskell
prm_name :: UnixParser Text
prm_name = parameter defaultParser
```

The `parameter` function creates a parameter parser out of a
`TextParser` (see below).

### TextParsers

A `TextParser r` is just a wrapper around a function that parses raw
`Text` and yields a value of type `r`. It also contains a "hint" for
displaying usage information. The library provides parsers for many
common types of input (such as `Text`, `Bool`, `Int`, etc.) via the
polymorphic `defaultParser :: DefaultParser a => TextParser a`, which
selects an appropriate parser based on the type.

## Options

An "option" is a construct that represents a labelled input. Options
are triggered by the presence of a particular flag followed by any
subarguments.

A "flag" is special symbol that signals the presence of an option. Per
UNIX tradition there are long flags (e.g. `--foo`) and short flags
(e.g `-f`).

Sometimes, to prevent ambiguity, an option's long flag is separated
from its subarguments by an equals sign instead of a space. For
example, `--uid=1000` is an option that begins with the `--uid` flag
and is followed by the subargument `1000`. Similarly, an option's
short flag can be directly concatenated with its argument, e.g. `-u
1000` can be written `-u1000`.

Let's define a parser for the `--uid` option:

```haskell
opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"] 
          "Specify a user ID" 
		  $ subparameter defaultParser
```

The `option` function creates a parser for CLI options. It takes three
arguments:

1. A list of `Flag`s that trigger the option, in this case "--uid" and
   "-u". The list has type `NonEmpty Flag`, so we can't use an empty
   list. `Flag` is an instance of `IsString`, so we can just write the
   string representation for convenience.
2. A human readable help text. This will be displayed when help output
   is triggered.
3. A subparser tree that handles any subparameters or suboptions. In
   this case, we declare a single subparameter (an integer). There is
   a `DefaultParser` instance for `Int`, so we can use `defaultParser`
   instead of manually writing a `TextParser`.

You might notice building our `Settings` record requires a `Maybe
Int`, not an `Int`. However, since `UnixParser` (and more generally
`ParseTree s` for any functor `s`) is an instance of `Alternative`, we
can use `optional` from `Control.Applicative`, yielding `optional
opt_uid :: UnixParser (Maybe Int)`. This means if the `--uid` flag
is absent, the parser will produce `Nothing`.

### Switches

The `--system` option is simpler because because it doesn't accept any
subarguments - it is either present (`True`) or absent (`False`). This
special type of option is a "switch", and we can use the `switch`
function to create a parser:

```haskell
opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"

-- If we defined this without 'switch' it would look like this:
-- opt_system = option ["--system", "-s"] 
--              "Create a system user"
--              (pure True)
--              <|> pure False

```

### Options with Multiple Subparameters

Let's deal with the `--groups` option. This option is a bit different
from the `--uid` option because we want the user to be able to specify
a list of groups for the new user to join. Thus, we want to create an
option that accepts one or more subarguments. Situations like this are
the reason options have an entire subparser tree. We can use `some`
(from `Control.Applicative`) to convert a parser that yields an `a`
into a parser that yields a list of one or more `a`s.

```haskell
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser
```

Mangrove automatically recognizes option parsers that can consume
multiple subarguments and comma-separates their inputs. That means we
can specify multiple groups like so: `--groups wheel,audio,input` and
we will get `["wheel","audio","input"]`.

## Applicative

We are now ready to construct our `Settings` parser using `<$>` and
`<*>`:

```haskell
parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name
```

Now we can inspect the automatically generated usage information for
our parser in GHCi using `render` from `Mangrove.Text`:

```
ghci> render parseSettings
"[--uid=INT] [--system] [--groups=STRING...] STRING"
```

This output indicates that our parser accepts (but does not require) a
`--uid` option with an integer subargument, a `--system` option, and a
`--groups` option with a list of string subarguments. Finally, it
requires a single parameter, which is a string. We'll see how to
improve the type hints later.

## Running the Parser

We can run parsers using the `parseArguments` function, which will run
our parser with the arguments passed to our program by the operating
system.

```haskell
main :: IO ()
main = parseArguments parseSettings "mkuser" "Create user accounts" run
```

`parseArguments` takes four arguments: a `UnixParser r`, the name of
the program (for help output), a description of the program (also for
help output), and a function of type `r -> IO a`. When the parser
completes successfully, this function will be called with the result.
Otherwise, the parser will print error messages or help information as
appropriate.

If you want to run an argument parser without using `IO`, or you want
to pass your own argument list, check out `runArgumentParser` from
`Mangrove.ArgumentParser`.

Now we have a complete program we can build and run to show the
argument parser in action!

```
$ ghc -o mkuser MkUser.hs
[1 of 2] Compiling Main             ( MkUser.hs, MkUser.o )
[2 of 2] Linking mkuser

$ ./mkuser --system --groups audio,input bilbo
Settings {userId = Nothing, userSystem = True, userGroups = ["audio","input"], userName = "bilbo"}

$ ./mkuser --badinput
unexpected --badinput

$ ./mkuser --system
expected: STRING

$ ./mkuser --uid=InvalidNumber bilbo
--uid=InvalidNumber: InvalidNumber: input does not start with a digit
```

## Help Options

Currently, our CLI interface is missing something important: an option
for displaying help and usage information. Let's create a new
`Settings` parser that recognizes `--help` as a request for help
information.

```haskell
parseSettings' :: UnixParser Settings
parseSettings' = addHelpOptions ["--help"]
                 "Display help and usage information"
                 parseSettings

main :: IO ()
main = parseArguments parseSettings' "mkuser" "Create user accounts" run
```

Now if we invoke our program with the `--help` option, it will display
a nice summary of how to use it:

```
./mkuser --help
Usage: mkuser --help|[--uid=INT] [--system] [--groups=STRING...] STRING

Create user accounts

    --help               Display help and usage information
-g  --groups  STRING...  Specify what groups the user is part of
-s  --system             Create a system user
-u  --uid     INT        Specify a user ID
```

## Hints

Type hints are displayed as placeholders for parameters in help and
usage information and act as a hint to the user about what kind of
information is expected by that input. For example, `--uid=INT`
indicates the `--uid` option expects an integer as a subargument.
Hints stored inside `TextParser` records as the `parserHint` field.

Our program makes use of the default hints associated with the
implementations of `defaultParser` for `Int` and `Text`, which is
reasonable. However, we can also tailor the hints more specifically to
our use case:

```
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser {parserHint = "GROUP"}

prm_name :: UnixParser Text
prm_name = parameter defaultParser {parserHint = "USERNAME"}
```

Now our help output looks like this:

```
./mkuser --help
Usage: mkuser --help|[--uid=INT] [--system] [--groups=GROUP...] USERNAME

Create user accounts

    --help              Display help and usage information
-g  --groups  GROUP...  Specify what groups the user is part of
-s  --system            Create a system user
-u  --uid     INT       Specify a user ID
```

## Full Example

Here is the full example program:

```haskell
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Text           (Text)
import           Mangrove.Unix

data Settings = Settings
  { userId     :: Maybe Int -- ^ An optional target user ID
  , userSystem :: Bool -- ^ Is this a system user?
  , userGroups :: [Text] -- ^ Groups the new user will be in
  , userName   :: Text  -- ^ Username for the new user
  } deriving (Show)

run :: Settings -> IO ()
run = print

opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"]
          "Specify a user ID"
          $ subparameter defaultParser

opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"

opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser

prm_name :: UnixParser Text
prm_name = parameter $ defaultParser

parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name

main :: IO ()
main = parseArguments parseSettings "mkuser" "Create user accounts" run
```

Build with `ghc -o mkuser Example.hs` and run:

```
$ ./mkuser --system --groups audio,input foo
Settings {userId = Nothing, userSystem = True, userGroups = ["audio","input"], userName = "foo"}
```
