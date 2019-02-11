"""
    Callbacks(; lazy = nothing, heuristic = nothing)

Set MIP-based callbacks. Two basic types are offered: `lazy` and `heuristic`.

### The lazy callback

The `lazy` callback can be used to add *lazy* constraints which are added on
demand at integer nodes in the branch and bound tree.

- The lazy constraint callback *may* be called when the solver has an integer
  primal solution
- You can only access the primal solution through `VariablePrimal`. That is,
  `ConstraintDual` etc will not work
- When the solver terminates, it will provide a solution that satisfies all
  lazy constraints
- We make no claims about what happens to solutions during the B&B tree

See also [`add_lazy_constraint`](@ref).

### The heuristic callback

The `heuristic` callback can be used to provide the solver with heuristically
obtained integer-feasible solutions at fractional nodes in the branch and bound
tree.

- The heuristic callback *may* be called when the solver has a fractional (i.e.,
  non-integer) solution
- The solver may silently reject the provided solution
- Some solvers require a complete solution, others only partial solutions. It's
  up to you to provide the appropriate one. If in doubt, give a complete
  solution.

See also [`add_heuristic_solution`](@ref).
"""
struct Callbacks <: AbstractModelAttribute
    lazy_callback::Union{Function, Nothing}
    heuristic_callback::Union{Function, Nothing}
    function Callbacks(; lazy = nothing, heuristic = nothing)
        return new(lazy, heuristic)
    end
end

"""
    add_lazy_constraint(
        model::ModelLike, cb_data, func::AbstractFunction, set::AbstractSet)

Add a lazy constraint `func`-in-`set` to `model`.

`cb_data` is a solver-specific callback type.

This can be called only from a lazy callback set by the attribute `Callbacks()`.
"""
function add_lazy_constraint(model, cb_data, func, set)
    error("add_lazy_constraint not supported by this solver.")
end

"""
    add_heuristic_solution(
        model::ModelLike, cb_data, sol::Dict{VariableIndex, Float64})

Provide the heuristic solution given by the variable-value mapping of `sol` to
`model`.

This can be called only from a heuristic callback set by the attribute
`Callbacks()`.
"""
function add_heuristic_solution(model, cb_data, sol)
    error("add_heuristic_solution not supported by this solver.")
end
