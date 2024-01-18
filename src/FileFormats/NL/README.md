# Variable ordering in NL files

This concept continues to befuddle me every time I go to investigate the code,
so here is an attempt to write out how variables are ordered in an NL file.

There are two key resources:

 1. Gay, D. (2005). Writing .nl files. https://ampl.github.io/nlwrite.pdf
 2. Gay, D. (2017). Hooking your solver to AMPL. https://ampl.com/REFS/hooking3.pdf

## A textualist's interpretation

Despite being called "Writing .nl files," the first resource is very sparse in
describing how to write an NL file. It contains the following paragraph:

> Variables are ordered as described in Tables 3 and 4 of [5], because some
> solvers treat linear constraints and variables specially. You only need to
> worry about such permutations if you are using a solver that cares about them.

where [5] means "Hooking your solver to AMPL."

Following the citation leads to a single paragraph and two tables:

> When writing stub.nl, AMPL orders the variables as shown in Tables 3 and 4 and
> the constraints as shown in Table 5. These tables also give expressions for
> how many entities are in each category. Table 4 applies to AMPL versions ≥
> 19930630; `nlvb = – 1` signifies earlier versions. For all versions, the first
> `nlvc` variables appear nonlinearly in at least one constraint. If
> `nlvo > nlvc`, the first `nlvc` variables may or may not appear nonlinearly in
> an objective, but the next `nlvo – nlvc` variables do appear nonlinearly in at
> least one objective. Otherwise all of the first `nlvo` variables appear
> nonlinearly in an objective.

This is a little confusing, but it suggests that the order of the variables
depends on whether `nlvo` is greater than `nlvc`. What are these acronyms?

The three most important are:
 * `nlvc` the number of variables that appear nonlinearly in a constraint
 * `nlvo` the number of variables that appear nonlinearly in an objective
 * `nlvb` the number of variables that appear nonlinearly in an objective and a
   constraint.
These can all appear with a `i` suffix, which means that they are integer. We'll
also see `nwv`, `niv`, and `nbv` for linear network variables, linear integer
variables, and linear binary variables.

Following the citations, Tables 3 and 4 are (Table 5 is unimportant):

Table 3: ordering of variables

| Category                    | Count                                       |
| --------------------------- | ------------------------------------------- |
| nonlinear                   | `max{nlvc, nlvo}`; see Table 4.             |
| linear arcs                 | `nwv`                                       |
| other linear                | `n_var - (max{nlvc,nlvo} + niv + nbv + nwv)`|
| linearly used binary        | `nbv`                                       |
| linearly used other integer | `niv`                                       |

Table 4: ordering of Nonlinear Variables.

| Smoothness | Appearance           | Count                         |
| ---------- | -------------------- | ------------------------------|
| continuous | in an objective and in a constraint | `nlvb - nlbvi` |
| integer    | in an objective and in a constraint | `nlvbi`        |
| continuous | just in constraints  | `nlvc - (nlvb + nlvci)`       |
| integer    | just in constraints  | `nlvci`                       |
| continuous | just in objectives   | `max{0, nlvo - nlvc}`         |
| integer    | just in objectives   | `nlvoi`                       |

## Putting the tables together

The two tables are clearly asking that we do something like this.

| Smoothness | Appearance           | Count                         |
| ---------- | -------------------- | ------------------------------|
| continuous | in an objective and in a constraint | `nlvb - nlbvi` |
| integer    | in an objective and in a constraint | `nlvbi`        |
| continuous | just in constraints  | `nlvc - (nlvb + nlvci)`       |
| integer    | just in constraints  | `nlvci`                       |
| continuous | just in objectives   | `max{0, nlvo - nlvc}`         |
| integer    | just in objectives   | `nlvoi`                       |
| linear arcs                 |     | `nwv`                         |
| other linear                |     | `n_var - (max{nlvc,nlvo} + niv + nbv + nwv)`|
| linearly used binary        |     | `nbv`                         |
| linearly used other integer |     | `niv`                         |

We can drop the "linear arcs" row and `nwv` because MOI doesn't represent
network problems, which leaves us with:

| Smoothness | Appearance           | Count                         |
| ---------- | -------------------- | ------------------------------|
| continuous | in an objective and in a constraint | `nlvb - nlbvi` |
| integer    | in an objective and in a constraint | `nlvbi`        |
| continuous | just in constraints  | `nlvc - (nlvb + nlvci)`       |
| integer    | just in constraints  | `nlvci`                       |
| continuous | just in objectives   | `max{0, nlvo - nlvc}`         |
| integer    | just in objectives   | `nlvoi`                       |
| other linear                |     | `n_var - (max{nlvc,nlvo} + niv + nbv)`|
| linearly used binary        |     | `nbv`                         |
| linearly used other integer |     | `niv`                         |

But does this table make sense? No. Here's the header from `geartrain.nl`:
```
g3 0 1 0	# problem geartrain
 4 0 1 0 0	# vars, constraints, objectives, ranges, eqns
 0 1	# nonlinear constraints, objectives
 0 0	# network constraints: nonlinear, linear
 0 4 0	# nonlinear vars in constraints, objectives, both
 0 0 0 1	# linear network variables; functions; arith, flags
 0 0 0 0 4	# discrete variables: binary, integer, nonlinear (b,c,o)
 0 4	# nonzeros in Jacobian, gradients
 0 0	# max name lengths: constraints, variables
 0 0 0 0 0	# common exprs: b,c,o,c1,o1
```

It has `n_var = 4`, `nlvo = 4`, and `nlvoi = 4` (all others are `0`).

| Smoothness | Appearance           | Count                         | `geartrain.nl`  |
| ---------- | -------------------- | ------------------------------| - |
| continuous | in an objective and in a constraint | `nlvb - nlbvi` | 0 |
| integer    | in an objective and in a constraint | `nlvbi`        | 0 |
| continuous | just in constraints  | `nlvc - (nlvb + nlvci)`       | 0 |
| integer    | just in constraints  | `nlcvi`                       | 0 |
| continuous | just in objectives   | `max{0, nlvo - nlvc}`         | 4 |
| integer    | just in objectives   | `nlvoi`                       | 4 |
| other linear                |     | `n_var - (max{nlvc,nlvo} + niv + nbv)`| 0 |
| linearly used binary        |     | `nbv`                         | 0 |
| linearly used other integer |     | `niv`                         | 0 |

But the rows would sum to give `8`, not `4`!

I think `max{0, nlvo - nlvc}` should be `max{0, nlvo - nlvc - nlvoi}`.

## The meaning of `nlvo`

Even then, the table we have doesn't make sense. Why is the number of nonlinear
variables `max{nlvc, nlvo}` in Table 3? Taken literally, this means that the
number of nonlinear variables is the max of the number of nonlinear variables
that appear in the constraints and the number of nonlinear variables that appear
in the objectives. That can't be true, which means that something is wrong.

Here's a simple AMPL model:
```
var x;
var y;
minimize obj: x^2;
subject to c: y^2 <= 1;
```
It produces the following header:
```
g3 0 1 0	# problem test_simple
 2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
 1 1	# nonlinear constraints, objectives
 0 0	# network constraints: nonlinear, linear
 1 2 0	# nonlinear vars in constraints, objectives, both
 0 0 0 1	# linear network variables; functions; arith, flags
 0 0 0 0 0	# discrete variables: binary, integer, nonlinear (b,c,o)
 1 1	# nonzeros in Jacobian, gradients
 0 0	# max name lengths: constraints, variables
 0 0 0 0 0	# common exprs: b,c,o,c1,o1
```
This clearly says that there are _two_ nonlinear variables in the objectives,
even though there is very clearly only one.

If we tweak the model very slightly:
```
var x;
var y;
minimize obj: x^2;
subject to c: x^2 + y^2 <= 1;
```
we get the following header:
```
g3 0 1 0	# problem test_simple
 2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
 1 1	# nonlinear constraints, objectives
 0 0	# network constraints: nonlinear, linear
 2 1 1	# nonlinear vars in constraints, objectives, both
 0 0 0 1	# linear network variables; functions; arith, flags
 0 0 0 0 0	# discrete variables: binary, integer, nonlinear (b,c,o)
 2 1	# nonzeros in Jacobian, gradients
 0 0	# max name lengths: constraints, variables
 0 0 0 0 0	# common exprs: b,c,o,c1,o1
```
This looks better. There are two nonlinear variables in the constraints, one in
the objective, and one that appears in both.

So what is going on with `nlvo`?

The answer is that despite all claims to the contrary, it does not represent the
number of nonlinear variables that appear in the objective.

If all variables that appear nonlinearly in the objective also appear in a
constraint, then `nlvo = nlvb`, and we can interpret the table as usual.

But, **if a variable appears nonlinearly in the objective but not in a**
**constraint**, then `nlvo` is the total number of variables that appear
nonlinearly in the objective or a constraint.

This solves the issue or `max{nlvc, nlvo}`, because it is now correct in both
cases. It also lets you puzzle out the text description in the papers:

> For all versions, the first `nlvc` variables appear nonlinearly in at least
> one constraint.

Yes, always true in both scenarios.

> If `nlvo > nlvc`, the first `nlvc` variables may or may not appear nonlinearly
> in an objective,

This must be the second scenario. In which case it is true. The first `nlvb`
will appear nonlinearly in an objective, and then `nlvc - nlvb` will appear
nonlinearly in a constraint.

> but the next `nlvo – nlvc` variables do appear nonlinearly in at least one
> objective.

Again, yes, because these are now the variables which appear nonlinearly in an
objective but _not_ in a constraint.

> Otherwise all of the first `nlvo` variables appear nonlinearly in an
> objective.

That means `nlvo <= nlvc`, in which case `nlvo = nlvb` and so the first `nlvo`
variables must appear nonlinearly in an objective.
