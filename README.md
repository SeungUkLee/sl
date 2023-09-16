# `slang`

> [!NOTE]
> 
> `slang` is experimental language.
> 
> This language was created while studying Haskell and PLT, so it may contain the wrong code. 
>
> Please, don't use it for anything important.
>

`slang` is a **s**imple ML dialect **lang**uage with let-ploymorphic type system, written in Haskell.

## Features

- Let-polymorphic type inference
- Recursion
- First-class function
- GHCi-like REPL
- Command-line tool
 
## Build and Run

You can build and run `slang` using `stack`:

```
$ stack build
```

```
$ stack run
```

## Test (Golden test)

```
$ stack test
```

## Usage

### Command line

You can see the commands available with the `-h` (`--help`) option.

```
$ stack run -- --help
SLang - a simple language

Usage: slang-exe [COMMAND]

  Command line for SLang

Available options:
  -h,--help                Show this help text

Available commands:
  interpret                Interpret a SLang file
  parse                    Parse a SLang file
  type                     Type Inference a SLang file
  repl                     Enter a REPL for SLang
```

#### `interpret`

Parse, type-check, and evaluate the given text and output a value result.

You can use the `interpret` subcommand for interpreting standard input:

```
$ stack run -- interpret
let x = 1 in x
- : int = 1
```

and you can use `-i` (`--input`) option for interpreting a single file:

```
(* example.sl *)
let x = 1 in x
```

```
$ stack run -- interpret -i ./example.sl
- : int = 1
```

If you want to output the interpret result to a file, you can use the `-o` (`--output`) option.

```
$ stack run -- interpret -o ./example.out
let x = 1 in x
```

```
(* example.out *)
- : int = 1
```

It is also available with `-i` and `-o` options.

```
$ stack run -- interpret -i ./example.sl -o ./example.out
```

The `-i` and `-o` options are also used in the `parse` and `type` subcommands.

#### `parse`

Parses the given text and output the AST result.

```
$ stack run -- parse
let x = 1 in x
ELet (LBVal "x" (EConst (CInt 1))) (EVar "x")
```

It is also available with `-i` and `-o` options.

#### `type`

Type check and infer the given text and output the type result of that text.

```
$ stack run -- type
let x = 1 in x
- : int
```

It is also available with `-i` and `-o` options.

#### `repl`

You can use REPL with the `repl` subcommand.

```
$ stack run -- repl

   _____ _                         Welcome to SLang REPL!
  / ____| |
 | (___ | |     __ _ _ __   __ _   Author:  Seunguk Lee
  \___ \| |    / _` | '_ \ / _` |  GitHub:  https://github.com/seunguklee/slang
  ____) | |___| (_| | | | | (_| |  Issues:  https://github.com/seunguklee/issues
 |_____/|______\__,_|_| |_|\__, |  About:   ML dialect language with let-ploymorphic type system
                            __/ |  License: MIT
                           |___/

Type ':help' for available commands

sl>
```

### REPL

#### `:help`

Show list of commands

```
sl> :help
```

#### `:load`

Load file and interpret 

```
(* example.sl *)
let id a = a in id 3
```

```
sl> :load ./example.sl
- : int = 3
```

#### `:parse`

Parse the given text and show AST

```
sl> :parse let x = 1 in x
ELet (LBVal "x" (EConst (CInt 1))) (EVar "x")
```

#### `:type`

Show the type of given text

```
sl> :type let x = 1 in x
let x = 1 in x : int
```

#### `:quit`

Exit REPL

```
sl> :quit
```

## Note

For example:

```
let add x y = x + y in
add 1 2
```

is semantically equivalent to:

```
let add = fun x -> fun y -> x + y in
add 1 2
```

and

```
let add = fun x y -> x + y in
add 1 2
```

## Examples

### polymorphic function

```
let id a = a in
let const n = 10 in
(id 3) + (const 1) + (const true) + (const false)
```

### first-class function

```
let square x = x * x in
let squareFunc = square in
squareFunc 3
```

```
let op f a b = f a b in
let sum a b = a + b in
let mul a b = a * b in
op sum (op sum 1 3) (op mul 2 3)
```

### let rec expression (recursion)

```
let rec iter acc n =
  if n == 0
    then acc
    else n + (iter acc (n - 1)) in
let sum n = iter 0 n in
sum 10
```

### let expression

```
let add x y = x + y in
add 1 3
```

### if expression

```
if 1 == 0
  then true
  else false
```

```
(* type error *)
if 1 == 0
  then 1
  else true
```

## Inspired & Credits

Sincere appreciation to the following repositories and courses for making the development of `slang` possible. 

- [4190.310 Programming Languages](http://kwangkeunyi.snu.ac.kr/4190.310/22/)
- [COSE212 Programming Languages](http://prl.korea.ac.kr/~pronto/home/courses/cose212/2022/)
- [Stephen Diehl's Write You a Haskell](https://github.com/sdiehl/write-you-a-haskell)
- [Adam Wespiser's Write You A Scheme, Version 2](https://github.com/write-you-a-scheme-v2/scheme)

## License

[LICENSE](./LICENSE)
