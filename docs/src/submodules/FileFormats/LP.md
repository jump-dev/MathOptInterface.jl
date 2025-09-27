# The LP file format

The purpose of this page is to document the LP file format, and the various
differences that occur between solvers.

## Resources

There are a bunch of different descriptions of the LP file format on the
internet.

 * [CPLEX](https://www.ibm.com/docs/en/icos/22.1.0?topic=cplex-lp-file-format-algebraic-representation)
 * [FICO](https://www.fico.com/fico-xpress-optimization/docs/dms2021-01/solver/optimizer/HTML/chapter10_sec_section102.html)
 * [Gurobi](https://docs.gurobi.com/projects/optimizer/en/current/reference/fileformats/modelformats.html#lp-format)
 * [lpsolve](https://lpsolve.sourceforge.net/5.5/CPLEX-format.htm)
 * [Mosek](https://docs.mosek.com/11.0/capi/lp-format.html)
 * [QSopt](https://www.math.uwaterloo.ca/~bico/qsopt/hlp/ff_lp_format.htm)

## Grammar

This section gives the grammar of an LP file as we implement it. This grammar
may be different to that of particular solvers.

The syntax rules for the grammar are:

 * `<name>`: the name of a symbol in the grammar
 * `A :== B`: A is equivalent to B
 * `A | B`: either A or B
 * `i""`: case insensitive string
 * `[A]`: A is optional
 * `(A)*`: there are 0 or more repeats of A
 * `(A)+`: there is at least one or more repeats of A

In addition to the grammar, there are the following rules:

 * Comments begin with `\`, and run until the next `\n` character
 * Whitespace is ignored
 * Newlines are ignored, except where explicitly described

```
<lp-file> :==
    <keyword-objective>\n
    [<section-objective>\n]
    <keyword-constraints>\n
    (<constraint>)*
    [<keyword-bounds>\n (<bound-expression>)+]
    [<keyword-general>\n (<identifier>)+]
    [<keyword-binary>\n (<identifier>)+]
    [<keyword-sos>\n (<constraint-sos>)+]
    [<keyword-end>]

<keyword-objective> :==
    i"min" | i"minimum" | i"minimize" | i"minimise"
  | i"max" | i"maximum" | i"maximize" | i"maximise"

<keyword-constraints> :==
    (i"subject to" | i"st" | i"st." | i"s.t." | i"such that")[":"]

<keyword-bounds> :== i"bound" | i"bounds"

<keyword-general> :== i"gen" | i"general" | i"generals"

<keyword-binary> :== i"bin" | i"binary" | i"binaries"

<keyword-sos> :== i"sos"

<keyword-end> :== i"end"

<digit> :== "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<char> :== a-z | A-Z | !"#$%&()/,;?@_'`|~

<identifier> :== <char> (<char> | <digit> | ".")*

<number> :==
    "+" <number>
  | "-" <number>
  | <digit>+[.(<digit>)*][("e" | "E")("+" | "-")(<digit>)+]
  | i"inf"
  | i"infinity"

<quadratic-term> :==
    "+" <quadratic-term>
  | "-" <quadratic-term>
  | [<number> ["*"]] <identifier> "^" "2"
  | [<number> ["*"]] <identifier> "*" <identifier>

<quadratic-expression> :==
    "[" <quadratic-term> (("+" | "-") <quadratic-term>)* "]" ["/" "2"]

<term> :==
    "+" <term>
  | "-" <term>
  | <identifier>
  | <number>
  | <number> <identifier>
  | <number> "*" <identifier>
  | <quadratic-expression>

<expression> :== <term> (("+" | "-") <term>)*

<name> :== [<identifier>":"]

<objective> :== <name> [<expression>]

<inequality> :== "<" | "<=" | "=<" | ">" | ">=" | "=>" | "=" | "=="

<set-suffix> := <inequality> <number>

<set-prefix> := <number> <inequality>

<constraint-indicator> :== <identifier> "=" (0 | 1) "->" <expression> <set-suffix>

<constraint-sos> :==
    "S1::" (<identifier>":"<number>)+\n
  | "S2::" (<identifier>":"<number>)+\n

<constraint> :==
    <name> <expression> <set-suffix>
  | <name> <constraint-indicator>
  | <name> <sos-constraint>

<bound-expression> :==
    <identifier> i"free"
  | <identifier> <set-suffix>
  | <set-prefix> <identifier>
  | <set-prefix> <identifier> <set-suffix>
```

## Differences

There are many differences in how solvers parse an LP file.

### The "integer" section

Consider the section:
```
integers
x
```
Gurobi will interpret this as `x in MOI.Integer()`. FICO Xpress will interpret
this as `x in MOI.ZeroOne()`.

FICO document this behavior, but they're an outlier.

**We choose to interpret `integers` as `MOI.Integer()`.**

### Whether variables can have the same name as a keyword

Consider the file
```
min
st
st
st >= 0
end
```
or even the horrific
```
min st st st >= 0 end
```
Gurobi will complain up front that the keyword `st` appears twice, whereas
Xpress will read the file as equivalent to :
```
minimize
x
subject to
x >= 0
end
```

**We choose to allow variables to be named as keywords, and we use context to
disambiguate.**

### Whitespace

Consider the file
```
minimize
  2x
end
```

Gurobi will interpret this as a single variable with the name `2x`, where as
Xpress will interpret this as the expression `2 * x`.

Gurobi document this behavior, saying that they require whitespace around all
tokens, but they're an outlier.

**We choose to allow juxtaposed tokens without whitespace.**

### Identifiers

In general, an identifier may contain the letters a-z, A-Z, the digits 0-9, and
the characters ```!"#\$%&()/,.;?@_'`|~```.

Additional solvers put additional restrictions:

 * In (all?) solvers except Gurobi, the identifier must not start with a digit
   or a `.` (in Gurobi, identifiers must be separated by whitespace)
 * Identifiers in Mosek and lpsolve may not start with the letter e or E
 * Keywords must not be used as names in Gurobi or Mosek, but they may in Xpress
 * Many solvers _actually_ support reading any UTF-8 string as the identifier,
   but they will normalize to the legal letters on write

**We choose to allow any valid UTF-8 names.**
