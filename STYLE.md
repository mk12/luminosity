Haskell Style Guide
===================

(Adapted from [Johan Tibell's style guide][1]).

This document describes the preferred coding style for this project. When something isn't covered by this guide, you should stay consistent with the style used in existing code.

[1]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md

Formatting
----------

### Line Length

Maximum line length is **80 characters**. No exceptions.

### Indentation

Tabs are illegal. Use **4 spaces** for indentation.

#### Guards

For functions with short names, put the first guard on the same line and align subsequent guards underneath. Otherwise, put the each guard on a separate line indented by *4 spaces*. Line up the `=` of each guard unless it creates too much whitespace and looks bad. Use your judgement.

#### Where Bindings

If a single `where` binding is needed for a short function, put it all on one line if possible. If not, indent it four spaces on the next line. If there are multiple `where` bindings, indent the `where` keyword **2 spaces** on its own line, and each binding on its own line indented by *4 spaces*.

If there are multiple equations (patterns) for a function and one has multiple `where` bindings, use the same style for every equation (`where` on a line by itself, even for just one binding).

---

Some examples of correct indentation:

```haskell
chisqr :: (Fractional a) => [a] -> [a] -> a
chisqr = (sum .) . zipWith chi where chi e o = (o - e) ^ 2 / e

sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * x ** (1 / 2.4) - 0.055

intersect (Plane n d) (Ray x v)
    | vn >= 0 || t < 0 = Nothing
    | otherwise        = Just t
  where
    vn = v <.> n
    t  = (vnegate x <.> n + d) / vn
```

### Blank Lines

Insert one blank line between top-level definitions. No blank lines between type signatures and function definitions. Add one blank line between functions in a type class instance declaration only if the functions bodies are large. Use your judgement.

### Whitespace

**Always** surround binary operators with a single space on each side. This includes sections (i.e., `(+ 1)` rather than `(+1)`). Don't insert a space between a lambda backslash and the first parameter.

### Function Type Signatures

**Always** provide a type signature for top-level functions. They may be omitted for short locally defined functions. Always enclose class constraints in parentheses, even if there is only one.

### Data Declarations

Align the constructors in a data type definition. Align the parameter types as well if it looks better that way. Some examples:

```haskell
data Tree a  = Leaf | Branch a (Tree a) (Tree a) deriving (Eq, Show)

data Shape a = Circle    Double 
             | Rectangle Double Double
             | Triangle  Double Double Double
             deriving (Eq, Show)
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
    | ...
```

#### Records

Use the prefix `m` for names in records to make them easily identifiable and to help avoid name clashing. Align the types for each field in the record. An example:

```haskell
data Person = Person
    { mFirstName :: String
    , mLastName  :: String
    , mAge       :: Int
    } deriving (Eq, Show)
```

When there are multiple constructors for record data types, line up the `|`s with the `=`:

```haskell
data Foo = Bar
    { mOne   :: Int
    , mTwo   :: Int
    , mThree :: String
    }    | Baz
    { mFour  :: String
    , mFive  :: String
    } deriving (Eq, Show)
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
( Set
, empty
, singleton
, member
) where
```

The types and functions in the export list should generally be in the same order that they are defined in the module, not alphabetized.

### List Declarations

Align the elements in the list. Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError ]
```

Optionally, you can skip the first newline. Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West ]
```

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda. Use your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else clauses, where possible. Short cases should usually be put on a single line (when line length allows it).

When writing non-monadic code (i.e. when not using `do`) and guards and pattern matches can't be used, you can align if-then-else clauses you like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, you should be consistent with the 4-spaces indent rule, and the `then` and the `else` keyword should be aligned. Examples:

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

The same rule applies to nested do blocks:

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports. The imports in each group should be sorted alphabetically, by module name. All the `qualified` imports should be at the end of the group, also sorted.

Always use explicit import lists or `qualified` imports for standard and third party libraries. This makes the code more robust against changes in these libraries. Exception: The Prelude.

Use explicit import lists for local imports if only a few things are being imported. If one specific function is being used, definitely use an import list. If nearly every function is used or the functionality of the module as a whole is needed or could be needed (e.g. `import Vector` to use vector math operators), then do not use an import list. Use your judgement.

Comments
--------

Write proper sentences: start with a capital letter and use proper punctuation. Wrap to the same line length as the code, and do not use hyphenation.

When referring to data types which are a common noun, do *not* capitalize them (e.g., "Add two vectors", not "Add two Vectors").

### Top-Level Definitions

Comment every top-level function (particularly exported functions). For function comments, use the imperative tense, e.g. "Send the ..." rather than "Send**s** the ...". Always use `-- ` comments, not block comments. *Use plain comments*: this project does not use Haddock syntax, for now.

Function documentation should give enough information to apply the function without looking at its definition.

Comments for data type definitions and top-level CAFs (constant applicative forms, i.e. values with no arguments) should typically begin with "A", "An", or "The".

Notes or special considerations should be on a separate line beginning with "Note: ". There should be a blank line separating (preceding) the note.

### End-of-Line Comments

Separate end-of-line comments from the code using *2 spaces*. Align comments on consecutive lines unless it looks bad. Use your judgement.

In general, avoid end-of-line comments in functions. If you need comments inside a function, it's probably complex enough to be split up into multiple functions.

Naming
------

Use mixed-case when naming functions and camel-case when naming data
types.

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of
`HTTPServer`. Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Avoid large modules. Each module should accomplish a specific task. Modules should be named and given responsibilities logically.

The functions within modules should be organized in whichever way makes it easier to follow when reading top to bottom. This usually means functions should be declared before used, but not necessarily. Short functions usually look better near the top of the file, while the real work of the module is done further down.

Misc
----

### Warnings

Be sure to compile with `-Wall`. There should be no warnings unless you have a good reason.
