# This file contains default implementations for the `MOI.copy_to` function that
# can be used by a model.

@deprecate automatic_copy_to default_copy_to false
@deprecate supports_default_copy_to MOI.supports_incremental_interface false

include("copy/index_map.jl")

"""
    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
    )

Pass the model attributes from the model `src` to the model `dest`.
"""
function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
)
    for attr in MOI.get(src, MOI.ListOfModelAttributesSet())
        if !MOI.supports(dest, attr)
            if attr == MOI.Name()
                continue  # Skipping names is okay.
            end
        end
        _pass_attribute(dest, src, index_map, attr)
    end
    return
end

function _pass_attribute(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    attr::MOI.AbstractModelAttribute,
)
    value = MOI.get(src, attr)
    if value !== nothing
        MOI.set(dest, attr, map_indices(index_map, attr, value))
    end
    return
end

"""
    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        vis_src::Vector{MOI.VariableIndex},
    )

Pass the variable attributes from the model `src` to the model `dest`.
"""
function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    vis_src::Vector{MOI.VariableIndex},
)
    for attr in MOI.get(src, MOI.ListOfVariableAttributesSet())
        if !MOI.supports(dest, attr, MOI.VariableIndex)
            if attr == MOI.VariableName() || attr == MOI.VariablePrimalStart()
                continue  # Skipping names and start values is okay.
            end
        end
        _pass_attribute(dest, src, index_map, vis_src, attr)
    end
    return
end

function _pass_attribute(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    vis_src::Vector{MOI.VariableIndex},
    attr::MOI.AbstractVariableAttribute,
)
    for x in vis_src
        value = MOI.get(src, attr, x)
        if value !== nothing
            MOI.set(
                dest,
                attr,
                index_map[x],
                map_indices(index_map, attr, value),
            )
        end
    end
    return
end

"""
    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{F,S}},
    ) where {F,S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to
the model `dest`.
"""
function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{F,S}},
) where {F,S}
    for attr in MOI.get(src, MOI.ListOfConstraintAttributesSet{F,S}())
        if !MOI.supports(dest, attr, MOI.ConstraintIndex{F,S})
            if (
                attr == MOI.ConstraintName() ||
                attr == MOI.ConstraintPrimalStart() ||
                attr == MOI.ConstraintDualStart()
            )
                continue  # Skipping names and start values is okay.
            end
        end
        _pass_attribute(dest, src, index_map, cis_src, attr)
    end
    return
end

function _pass_attribute(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{F,S}},
    attr::MOI.AbstractConstraintAttribute,
) where {F,S}
    for ci in cis_src
        value = MOI.get(src, attr, ci)
        if value !== nothing
            MOI.set(
                dest,
                attr,
                index_map[ci],
                map_indices(index_map, attr, value),
            )
        end
    end
    return
end

"""
    _try_constrain_variables_on_creation(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        ::Type{S},
    ) where {S<:MOI.AbstractVectorSet}

Copy the constraints of type `MOI.VectorOfVariables`-in-`S` from the model `src`
to the model `dest` and fill `index_map` accordingly. The copy is only done when
the variables to be copied are not already keys of `index_map`.

It returns a list of the constraints that were not added.
"""
function _try_constrain_variables_on_creation(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    ::Type{S},
) where {S<:MOI.AbstractVectorSet}
    not_added = MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
    for ci_src in
        MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}())
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        if !allunique(f_src.variables)
            # Can't add it because there are duplicate variables
            push!(not_added, ci_src)
        elseif any(vi -> haskey(index_map, vi), f_src.variables)
            # Can't add it because it contains a variable previously added
            push!(not_added, ci_src)
        else
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vis_dest, ci_dest = MOI.add_constrained_variables(dest, set)
            index_map[ci_src] = ci_dest
            for (vi_src, vi_dest) in zip(f_src.variables, vis_dest)
                index_map[vi_src] = vi_dest
            end
        end
    end
    return not_added
end

"""
    _try_constrain_variables_on_creation(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        ::Type{S},
    ) where {S<:MOI.AbstractScalarSet}

Copy the constraints of type `MOI.VariableIndex`-in-`S` from the model `src` to
the model `dest` and fill `index_map` accordingly. The copy is only done when the
variables to be copied are not already keys of `index_map`.

It returns a list of the constraints that were not added.
"""
function _try_constrain_variables_on_creation(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    ::Type{S},
) where {S<:MOI.AbstractScalarSet}
    not_added = MOI.ConstraintIndex{MOI.VariableIndex,S}[]
    for ci_src in
        MOI.get(src, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        if haskey(index_map, f_src)
            # Can't add it because it contains a variable previously added
            push!(not_added, ci_src)
        else
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vi_dest, ci_dest = MOI.add_constrained_variable(dest, set)
            index_map[ci_src] = ci_dest
            index_map[f_src] = vi_dest
        end
    end
    return not_added
end

"""
    _copy_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        cis_src::Vector{<:MOI.ConstraintIndex},
    )

Copy the constraints `cis_src` from the model `src` to the model `dest` and fill
`index_map` accordingly. Note that the attributes are not copied; call
[`pass_attributes`] to copy the constraint attributes.
"""
function _copy_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
)
    for ci in cis_src
        f = MOI.get(src, MOI.ConstraintFunction(), ci)
        s = MOI.get(src, MOI.ConstraintSet(), ci)
        index_map[ci] = MOI.add_constraint(dest, map_indices(index_map, f), s)
    end
    return
end

function pass_nonvariable_constraints_fallback(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    constraint_types,
)
    for (F, S) in constraint_types
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        _copy_constraints(dest, src, index_map, cis_src)
    end
    return
end

"""
    pass_nonvariable_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        constraint_types,
    )

For all tuples `(F, S)` in `constraint_types`, copy all constraints of type
`F`-in-`S` from `src` to `dest` mapping the variables indices with `index_map`.

The default implementation calls `pass_nonvariable_constraints_fallback`.
A method can be implemented to use a specialized copy for a given type of
`dest`.
"""
function pass_nonvariable_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    constraint_types,
)
    pass_nonvariable_constraints_fallback(
        dest,
        src,
        index_map,
        constraint_types,
    )
    return
end

function _pass_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    variable_constraints_not_added::Vector,
)
    for cis in variable_constraints_not_added
        _copy_constraints(dest, src, index_map, cis)
    end
    all_constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    nonvariable_constraint_types = Any[
        (F, S) for (F, S) in all_constraint_types if !_is_variable_function(F)
    ]
    pass_nonvariable_constraints(
        dest,
        src,
        index_map,
        nonvariable_constraint_types,
    )
    for (F, S) in all_constraint_types
        pass_attributes(
            dest,
            src,
            index_map,
            MOI.get(src, MOI.ListOfConstraintIndices{F,S}()),
        )
    end
    return
end

function _copy_free_variables(dest::MOI.ModelLike, index_map::IndexMap, vis_src)
    if length(vis_src) == length(index_map.var_map)
        return  # All variables already added
    end
    x = MOI.add_variables(dest, length(vis_src) - length(index_map.var_map))
    i = 1
    for vi in vis_src
        if !haskey(index_map, vi)
            index_map[vi] = x[i]
            i += 1
        end
    end
    @assert i == length(x) + 1
    return
end

_is_variable_function(::Type{MOI.VariableIndex}) = true
_is_variable_function(::Type{MOI.VectorOfVariables}) = true
_is_variable_function(::Any) = false

function _cost_of_bridging(
    dest::MOI.ModelLike,
    ::Type{S},
) where {S<:MOI.AbstractScalarSet}
    x = MOI.get(dest, MOI.VariableBridgingCost{S}())
    y = MOI.get(dest, MOI.ConstraintBridgingCost{MOI.VariableIndex,S}())
    return !iszero(x), x - y, true
end

function _cost_of_bridging(
    dest::MOI.ModelLike,
    ::Type{S},
) where {S<:MOI.AbstractVectorSet}
    x = MOI.get(dest, MOI.VariableBridgingCost{S}())
    y = MOI.get(dest, MOI.ConstraintBridgingCost{MOI.VectorOfVariables,S}())
    return !iszero(x), x - y, false
end

"""
    sorted_variable_sets_by_cost(dest::MOI.ModelLike, src::MOI.ModelLike)

Return a `Vector{Type}` of the set types corresponding to `VariableIndex` and
`VectorOfVariables` constraints in the order in which they should be added.

## How the order is computed

The sorting happens in the `_cost_of_bridging` function and has three main
considerations:

1. First add sets for which the `VariableBridgingCost` is `0`. This ensures that
   we minimize the number of variable bridges that get added.
2. Then add sets for which the VariableBridgingCost is smaller than the
   `ConstraintBridgingCost` so they can get added with
   `add_constrained_variable(s)`.
3. Finally, break any remaining ties in favor of `AbstractVectorSet`s. This
   ensures we attempt to add large blocks of variables (e.g., such as PSD
   matrices) before we add things like variable bounds.

## Why the order is important

The order is important because some solvers require variables to be added in
particular order, and the order can also impact the bridging decisions.

We favor adding first variables that won't use variables bridges because then
the variable constraints on the same variable can still be added as
`VariableIndex` or `VectorOfVariables` constraints.

If a variable does need variable bridges and is part of another variable
constraint, then the other variable constraint will be force-bridged into affine
constraints, so there is a hidden cost in terms of number of additional number
of bridges that will need to be used.

In fact, if the order does matter (in the sense that changing the order of the
vector returned by this function leads to a different formulation), it means the
variable is in at least one other variable constraint. Thus, in a sense we
could do `x - y + sign(x)`` but `!iszero(x), x - y` is fine.

## Example

A key example is Pajarito. It supports `VariableIndex`-in-`Integer` and
`VectorAffine`-in-`Nonnegatives`. If the user writes:
```julia
@variable(model, x >= 1, Int)
```
then we need to add two variable-related constraints:
 * `VariableIndex`-in-`Integer`
 * `VariableIndex`-in-`GreaterThan`
The first is natively supported and the variable and constraint bridging cost is
0. The second must be bridged to  `VectorAffineFunction`-in-`Nonnegatives` via
`x - 1 in Nonnegatives(1)`, and the variable and constraint bridging cost is
`1` in both cases.

If the order is `[Integer, GreaterThan]`, then we add `x ∈ Integer` and
`x - 1 in Nonnegatives`. Both are natively supported and it only requires a
single constraint bridge.

If the order is `[GreaterThan, Integer]`, then we add a new variable constrained
to `y ∈ Nonnegatives` and end up with an expression from the variable bridge of
`x = y + 1`. Then when we add the Integer constraint, we get `y + 1 in Integer`,
which is not natively supported. Therefore, we need to add `y + 1 - z ∈ Zeros`
and `z ∈ Integer`. Oops! This cost an extra variable, a variable bridge of
`x = y + 1`and a `Zeros` constraint.

Unfortunately, we don't have a good way of computing the updated costs for other
constraints if a variable bridge is chosen.
"""
function sorted_variable_sets_by_cost(dest::MOI.ModelLike, src::MOI.ModelLike)
    constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    sets = Type[S for (F, S) in constraint_types if _is_variable_function(F)]
    sort!(sets; by = S::Type -> _cost_of_bridging(dest, S))
    return sets
end

"""
    function final_touch(model::MOI.ModelLike, index_map) end

This is called at the end of [`default_copy_to`](@ref) to inform the model that
the copy is finished. This allows `model` to perform thats that should be done
only once all the model information is gathered.
"""
function final_touch(::MOI.ModelLike, index_map) end

function default_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    ::Bool,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    @warn(
        "The `copy_names` and `filter_constraints` arguments to " *
        "`default_copy_to` have been removed.",
        maxlog = 1,
    )
    return default_copy_to(dest, src)
end

"""
    default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike)

A default implementation of `MOI.copy_to(dest, src)` for models that implement
the incremental interface, i.e., [`MOI.supports_incremental_interface`](@ref)
returns `true`.
"""
function default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike)
    if !MOI.supports_incremental_interface(dest)
        error("Model $(typeof(dest)) does not support copy_to.")
    end
    MOI.empty!(dest)
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    index_map = IndexMap()
    # The `NLPBlock` assumes that the order of variables does not change (#849)
    # Therefore, all VariableIndex and VectorOfVariable constraints are added
    # seprately, and no variables constrained-on-creation are added.
    has_nlp = MOI.NLPBlock() in MOI.get(src, MOI.ListOfModelAttributesSet())
    constraints_not_added = if has_nlp
        Any[
            MOI.get(src, MOI.ListOfConstraintIndices{F,S}()) for
            (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent()) if
            _is_variable_function(F)
        ]
    else
        Any[
            _try_constrain_variables_on_creation(dest, src, index_map, S)
            for S in sorted_variable_sets_by_cost(dest, src)
        ]
    end
    _copy_free_variables(dest, index_map, vis_src)
    # Copy variable attributes
    pass_attributes(dest, src, index_map, vis_src)
    # Copy model attributes
    pass_attributes(dest, src, index_map)
    # Copy constraints
    _pass_constraints(dest, src, index_map, constraints_not_added)
    final_touch(dest, index_map)
    return index_map
end

"""
    ModelFilter(filter::Function, model::MOI.ModelLike)

A layer to filter out various components of `model`.

The filter function takes a single argument, which is eacy element from the list
returned by the attributes below. It returns `true` if the element should be
visible in the filtered model and `false` otherwise.

The components that are filtered are:

 * Entire constraint types via:
   * `MOI.ListOfConstraintTypesPresent`
 * Individual constraints via:
   * `MOI.ListOfConstraintIndices{F,S}`
 * Specific attributes via:
   * `MOI.ListOfModelAttributesSet`
   * `MOI.ListOfConstraintAttributesSet`
   * `MOI.ListOfVariableAttributesSet`

!!! warning
    The list of attributes filtered may change in a future release. You should
    write functions that are generic and not limited to the five types listed
    above. Thus, you should probably define a fallback `filter(::Any) = true`.

See below for examples of how this works.

!!! note
    This layer has a limited scope. It is intended by be used in conjunction
    with `MOI.copy_to`.

## Example: copy model excluding integer constraints

Use the `do` syntax to provide a single function.

```julia
filtered_src = MOI.Utilities.ModelFilter(src) do item
    return item != (MOI.VariableIndex, MOI.Integer)
end
MOI.copy_to(dest, filtered_src)
```

## Example: copy model excluding names

Use type dispatch to simplify the implementation:

```julia
my_filter(::Any) = true  # Note the generic fallback!
my_filter(::MOI.VariableName) = false
my_filter(::MOI.ConstraintName) = false
filtered_src = MOI.Utilities.ModelFilter(my_filter, src)
MOI.copy_to(dest, filtered_src)
```

## Example: copy irreducible infeasible subsystem

```julia
my_filter(::Any) = true  # Note the generic fallback!
function my_filter(ci::MOI.ConstraintIndex)
    status = MOI.get(dest, MOI.ConstraintConflictStatus(), ci)
    return status != MOI.NOT_IN_CONFLICT
end
filtered_src = MOI.Utilities.ModelFilter(my_filter, src)
MOI.copy_to(dest, filtered_src)
```
"""
struct ModelFilter{T,F} <: MOI.ModelLike
    inner::T
    filter::F
    function ModelFilter(filter::Function, model::MOI.ModelLike)
        return new{typeof(model),typeof(filter)}(model, filter)
    end
end

function MOI.get(
    model::ModelFilter,
    attr::Union{
        MOI.ListOfConstraintAttributesSet,
        MOI.ListOfConstraintIndices,
        MOI.ListOfConstraintTypesPresent,
        MOI.ListOfModelAttributesSet,
        MOI.ListOfVariableAttributesSet,
    },
)
    return filter(model.filter, MOI.get(model.inner, attr))
end

function MOI.get(model::ModelFilter, attr::MOI.AbstractModelAttribute)
    return MOI.get(model.inner, attr)
end

# !!! warning
#     Slow implementations, but we need to report the number of constraints in
#     the filtered model, not in the `.inner`.

function MOI.get(model::ModelFilter, ::MOI.NumberOfConstraints{F,S}) where {F,S}
    return length(MOI.get(model, MOI.ListOfConstraintIndices{F,S}()))
end

# These just forward the attributes into the inner model.

function MOI.get(
    model::ModelFilter,
    attr::MOI.AbstractVariableAttribute,
    x::MOI.VariableIndex,
)
    return MOI.get(model.inner, attr, x)
end

function MOI.get(
    model::ModelFilter,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    return MOI.get(model.inner, attr, ci)
end

# !!! warning
#     This is a slow implementation. But we need to check if we filtered
#     everything.
function MOI.is_empty(model::ModelFilter)
    if MOI.is_empty(model.inner)
        return true
    elseif MOI.get(model.inner, MOI.NumberOfVariables()) > 0
        return false
    elseif length(MOI.get(model, MOI.ListOfModelAttributesSet())) > 0
        return false
    end
    return true
end

MOI.empty!(model::ModelFilter) = MOI.empty!(model.inner)
