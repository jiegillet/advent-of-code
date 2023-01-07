# AoC 2022 with Gleam

This is a brief report on my experience on solving some Advent of Code problems using Gleam 0.25.

I have no prior experience with Gleam, but I have experience with Elixir, Elm and Haskell.
I solved about 18 problems.

## Things I love

I really love some design decisions.

### Labelled arguments

This feature is really awesome.
I used it systematically for `list.fold` and other functions of more than 2 arguments, because I can never remember the order of the arguments, but I can always remember `over`, `from` and `with`.

### Alternative patterns in case expressions

It's really cool to be able to group some patterns with `|`.
It's not always the most readable option, but when it fits, it's awesome.

### Matching on multiple values

I really like the shortcut `case x, y` for pattern matching several values, since I find the tuple notation `#(x, y)` more noisy.



## Wishlist

### More complex expressions in `case` statements

Quite often, I wanted to use expressions like

```gleam
  case x { 
    _ if x == y + 1 -> Ok(x)
    _ -> Error(Nil)
```
but that gives the warning
```
7 │     _ if x == y + 1 -> Ok(x)
  │                 ^ I was not expecting this.

Expected one of: "->"
Hint: Did you mean to wrap a multi line clause in curly braces?
```
I tried curly braces like the hint suggested, but it didn't work.

### Using `use` after pipes

What made me want to try Gleam was the blog post about `use` which I thought was really nice.
I kept trying to find a place to use it, unfortunately it is not a good fit for something like the AoC where I mostly used data pipes and not enough helper functions like I would in production code.

A couple of times I wanted to do
```gleam
use x <- "hello"
  |> string.to_graphemes()
  |> list.map()

string.uppercase(x)
```
but I couldn't
```
15 │     |> list.map()
   │        ^^^^^^^^^^ Expected 2 arguments, got 0
```

The formatter also doesn't handle the code well.

```gleam
  use 
    x
  <- "hello"
    |> string.to_graphemes()
    |> list.map()
```

### Destructuring in function arguments

Something I really missed as compared to Elm was the ability to destructure values in function elements.
It should work for tuples and custom types with one constructor.

### Syntax to import types and type constructors

I found the syntax to import types and their constructors a bit unpleasant.
```gleam
import gleam/order.{Eq, Gt, Lt, Order}
```
I think it's because it's mixing types and constructors, that don't feel to me like they should be on the same level.
It's also a bit tedious if there are many constructors and I think a shortcut would be welcome, maybe the Elm style
```gleam
import gleam/order.{Order{..}}
```

### Would love to have `cond`

Influenced by Elixir's `cond`, I sometimes used the pattern
```gleam
case Nil {
    _ if x == y -> 1
    _ if x == z -> -1
    _ -> 0
}
```
but it could be cleaner.

### `Result(a, Nil)` versus `Option(a)`?

Many `list` functions use `Result(a, Nil)`, which is fine, because `Result` is automatically in scope, but `map.update` uses `Option(a)`, which is isomorphic, but much more of a pain to import.
I don't really have a suggestion here other than upgrading `Option` to the prelude and changing the `list` API, but that's a huge breaking change.



## Pain points in getting started

There were some pain points with getting started.

### File I/O

It was quite difficult to find how to do file IO.
I looked everywhere I could on gleam.run, but I couldn't find any mention on how to do that.
I assumed I would have to roll out my own functions using Erlang/Elixir interop until a friend pointed me out to `gleam/erlang`.

I think it should be mentioned somewhere in the getting started material or the Gleam Book.

### Nibble Parsing library

I like parser combinators, so I found `hayleigh-dot-dev/gleam-nibble` and it's great, but it took me a long time to find it.
It should be added to Awesome Gleam, so I just opened a PR to do that.

### Empty docsets in Dash

I use [Dash](https://kapeli.com/dash) to aggregate language documentations.
It usually works well with hexdocs packages, but for some reason it doesn't work with `gleam_stdlib`, it says "Empy docset" after installing it.




## Bugs

### gleam format removes braces after `!`

The expression
```gleam
  !{ True && False}
```
gets formatted to
```gleam
  !True && False
```
which changes the value from true to false.

### compile message for missing arguments in a pipe

For this code
```gleam
[1, 2, 3]
  |> list.map()
```
the compiler message is
```
15 │     |> list.map()
   │        ^^^^^^^^^^ Expected 2 arguments, got 0
```

But since it is in a pipe, it should expect only one more, not 2, and it is also not really receiving 0.
Maybe it should say something like "Expected 2 arguments, got 1 piped in" or for larger functions "Expected 3 arguments, got 1 plus 1 piped in".

### Order of definition matters for generics

```
$ gleam --version
gleam 0.25.0
```

The code
```gleam
type Bar(a) { Bar(a)  }
type Foo(a) = Map(a, Bar(a))
```
compiles, but 
```gleam
type Foo(a) = Map(a, Bar(a))
type Bar(a) { Bar(a)  }
```
doesn't:
```
19 │ type Foo(a) = Map(a, Bar(a))
   │                      ^^^^^^ Did you mean `Map`?
```

Other functions don't seem to mind the order of definition.

### Parsing of negative numbers

```
$ gleam --version
gleam 0.25.0
```

I'm not too sure to call that a bug or not because it's a tricky thing but `x-1` is parsed as `x` minus one, but `x -1` (with a space) is parsed as `{ x } { -1 }`.

### Imprecise compiler error in case expressions

```
$ gleam --version
gleam 0.25.0
```

This code
```gleam
type Bar {
  Bar(bar: Int)
}

fn foo() {
case Bar(1) {
  x if x.bar == 1 -> Ok(1)
  }
}
```
gives the compiler error
```
12 │   x if x.bar == 1 -> Ok(1)
   │          ^ This integer is not valid for tuple access.

Hint: Only non negative integer literals like 0, or 1_000 can be used.
```
but `bar` isn't an integer and `x` isn't a tuple.
I'm guessing that only tuples are allowed in `if`s, maybe that should be added in the hint.

### Erlang warning for unused ignored pattern match

```
$ gleam --version
gleam 0.25.0
```

The following function
```gleam
fn foo() {
  assert [a, ..] as result = [1, 2]  
 #(a, result) 
}
```
produces the Erlang (not Gleam) warning
```
  Compiling hello
build/dev/erlang/hello/_gleam_artefacts/hello.erl:14:36: Warning: variable '_@1' is already bound. If you mean to ignore this value, use '_' or a different underscore-prefixed name
%   14|         [A | _@1] = Result -> [A | _@1] = Result;
%     |                                    ^
```

### Type inference

```
$ gleam --version
gleam 0.25.0
```

I expected the following code to compile
```gleam
fn foo() {
  let x = Bar(1)
  [fn (b) { b.bar }]
  |> List.map(fn (unbar) { unbar(x)})
}
```
but I got
```
14 │   [fn (b) { b.bar }]
   │             ^ I don't know what type this is

In order to access a record field we need to know what type it is,
but I can't tell the type here. Try adding type annotations to your
function and try again.
```
Type annotations do fix the problem, but I thought the type could be inferred from its use in `unbar(x)`.
