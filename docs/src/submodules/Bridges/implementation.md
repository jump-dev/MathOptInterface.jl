```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementing a bridge

The easiest way to implement a bridge is to follow an existing example. There
are three locations of bridges in the source code:

 * Constraint bridges are stored in `src/Bridges/Constraint/bridges`
 * Objective bridges are stored in `src/Bridges/Objective/bridges`
 * Variable bridges are stored in `src/Bridges/Variable/bridges`

The [Implementing a constraint bridge](@ref) tutorial has a more detailed guide
on what is required to implement a bridge.

When opening a pull request that adds a new bridge, use the checklist
[Adding a new bridge](@ref).

If you need help or advice, please contact the [Developer Chatroom](https://jump.dev/chatroom/).

## SetMap bridges

For constraint and variable bridges, a common reformulation is that ``f(x) \in F``
is reformulated to ``g(x) \in G``. In this case, no additional variables and
constraints are added, and the bridge needs only a way to map between the
functions `f` and `g` and the sets `F` and `G`.

To implementation a bridge of this form, subtype the abstract type
[`Bridges.Constraint.SetMapBridge`](@ref) or
[`Bridges.Variable.SetMapBridge`](@ref) and implement the API described in the
docstring of each type.

## `final_touch`

Some bridges require information from other parts of the model. One set of
examples are the various combinatorial `ToMILP` bridges, such as
[`Bridges.Constraint.SOS1ToMILPBridge`](@ref), which require knowledge of the
variable bounds.

Bridges requiring information from other parts of the model should implement
[`Bridges.final_touch`](@ref) and [`Bridges.needs_final_touch`](@ref).

During the bridge's construction, store the function and set and make no changes
to the underlying model. Then, in [`Bridges.final_touch`](@ref), query the
additional information and add the reformulated problem to the `model`.

When implementing, you must consider that:

 * [`Bridges.final_touch`](@ref) may be called multiple times, so that your
   reformulation should be applied only if necessary. Sometimes the additional
   data will be the same, and sometimes it may be different.
 * We do not currently support `final_touch` bridges that introduce constraints
   which also require a `final_touch` bridge. Therefore, you should implement
   `final_touch` only if necessary, and we recommend that you contact the
   [Developer Chatroom](https://jump.dev/chatroom/) for advice before doing so.

## Testing

Use the [`Bridges.runtests`](@ref) function to test a bridge. It takes three
arguments: the type of the bridge, the input model as a string, and the output
model as a string.

Here is an example:
```jldoctest; filter=[r"[0-9.]+s", r"\s+Time"]
julia> MOI.Bridges.runtests(
           MOI.Bridges.Constraint.GreaterToLessBridge,
           """
           variables: x
           x >= 1.0
           """,
           """
           variables: x
           -1.0 * x <= -1.0
           """,
       )
Test Summary:    | Pass  Total  Time
Bridges.runtests |   30     30  0.0s
```

There are a number of other useful keyword arguments.

 * `eltype` can be used to specify the element type of the model (and bridge).
   It defaults to `Float64`.
 * `variable_start` and `constraint_start` are used as the values to set the
   [`VariablePrimalStart`](@ref) and [`ConstraintPrimalStart`](@ref) attributes
   to. They default to `1.2`. If you use a different `eltype`, you must set
   appropriate starting values of the same type. The default `1.2` was chosen to
   minimize the risk that the starting point is undefined, which could happen
   for common situations like `0.0` and `1.0`. The tests associated with the
   starting values do not necessarily check for correctness, only that they can
   be `set` and `get` to produce the same result.
 * `print_inner_model` can be used to print the reformulated output model from
   the bridge. This is especially helpful during debugging to see what the
   bridge is doing, and to spot mistakes. It defaults to `false`.

Here is an example:

```jldoctest; filter=[r"[0-9.]+s", r"\s+Time"]
julia> MOI.Bridges.runtests(
           MOI.Bridges.Constraint.GreaterToLessBridge,
           """
           variables: x
           x >= 1
           """,
           """
           variables: x
           ::Int: -1 * x <= -1
           """;
           eltype = Int,
           print_inner_model = true,
           variable_start = 2,
           constraint_start = 2,
       )
Feasibility

Subject to:

ScalarAffineFunction{Int64}-in-LessThan{Int64}
 (0) - (1) x <= (-1)
Test Summary:    | Pass  Total  Time
Bridges.runtests |   30     30  0.0s
```
