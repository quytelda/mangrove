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

## An Example

Suppose we are writing a simple program that creates new user
accounts - we'll call it "mkuser". The goal will be to provide a
command line interface with the following syntax:

```
mkuser [--uid=INT] [--system] [--groups=GROUP...] USERNAME
```

First, let's create a new record that captures the program's runtime
configuration.

```
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

```
run :: Settings -> IO ()
run = print
```

Now, we want to convert the list of arguments passed to the program
into a `Settings` record. To do this, we need construct a `UnixParser
Settings`. `UnixParser` is just a convenient type synonym for
`ParseTree UnixScheme`, so what we are actually constructing here is a
Unix-style parse tree that produces a `Settings` when fed the proper
inputs and evaluated. We will build up the tree from smaller parsers
using the power of `Applicative`.

Note: This example uses the language extensions `OverloadedLists` and
`OverloadedStrings` since we need to write lots of `NonEmpty` list and
`Text` literals.

## Options

Let's start by defining the `--uid` option:

```
opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"] 
          "Specify a user ID" 
		  $ subparameter defaultParser
```

The `option` function (which defines a CLI option parser) takes three
arguments:

1. A list of `Flag`s that trigger the option, in this case "--uid" and
   "-u". This is a `NonEmpty` list, so passing `[]` would not
   typecheck. Flag is an instance of `IsString`, so we can just write
   the string representation for convenience.
2. A human readable help string. This will be displayed when help
   output is triggered.
3. A subtree parser that defines any subparameters or suboptions. In
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

The `--system` option will simpler because because it doesn't accept
any subarguments - it is either present (`True`) or absent (`False`).
This special type of option is a "switch", and we can use the `switch`
function to create it:

```
opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"

-- If defined this without 'switch' it would look like this:
-- opt_system = option ["--system", "-s"] 
--              "Create a system user"
--              (pure True)
--              <|> pure False

```

### Options with Multiple Subparameters

Let's deal with the `--groups` option. This option is a bit different
than the `--uid` option because we want the user to be able to specify
a list of groups for the new user to join. Situations like this are
the reason options have an entire subparser tree. We can use `some`
(from `Control.Applicative`) to convert a parser that yields an `a`
into a parser that yields a list of one or more `a`s.

```
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser
```

Mangrove automatically recognizes option parsers that can consume
multiple subarguments, and comma-separates their inputs. That means we
can specify multiple groups like so: `--groups wheel,audio,input` and
we will get `["wheel","audio","input"]`.

## Parameters

Finally, our program needs one last input: the username. Since we
always require this input, we define it as a "parameter" instead of an
option. We create it using the `parameter` function:

```
prm_name :: UnixParser Text
prm_name = parameter defaultParser
```

## Applicative

We are now ready to construct our `Settings` parser using `<$>` and
`<*>`:

```
parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name
```

Now we can inspect our parser in GHCi using `render` from
`Mangrove.Text`:

```
ghci> render parseSettings
"[--uid=INT] [--system] [--groups=STRING...] STRING"
```

## Running the Parser

We can run parsers using the `parseArguments` function, which will run
our parser with the arguments passed to our program by the operating
system.

```
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

## Full Example

Here is the full example program:

```
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

## Definitions

A "flag" is special symbol that signals the presence of an option. Per
UNIX tradition there are long flags (e.g. `--foo`) and short flags
(e.g `-f`).

An "option" is a construct that represents a named input. Options
are triggered by the presence of a particular flag, and might accept
further input.
