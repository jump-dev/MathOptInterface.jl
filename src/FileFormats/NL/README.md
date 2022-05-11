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

Table 3: Ordering of variables

| Category                    | Count                                       |
| --------------------------- | ------------------------------------------- |
| nonlinear                   | `max{nlvc, nlvo}`; see Table 4.             |
| linear arcs                 | `nwv`                                       |
| other linear                | `n_var - (max{nlvc,nlvo} + niv + nbv + nwv)`|
| linearly used binary        | `nbv`                                       |
| linearly used other integer | `niv`                                       |

Table 4: Ordering of Nonlinear Variables.

| Smoothness | Appearance           | Count                         |
| ---------- | -------------------- | ------------------------------|
| continous  | in an objective and in a constraint | `nlvb - nlbvi` |
| integer    | in an objective and in a constraint | `nlvbi`        |
| continous  | just in constraints  | `nlvc - (nlvb + nlvci)`       |
| integer    | just in constraints  | `nlcvi`                       |
| continuous | just in objectives   | `max{0, nlvo - nlvc}`         |
| integer    | just in objectives   | `nlvoi`                       |
