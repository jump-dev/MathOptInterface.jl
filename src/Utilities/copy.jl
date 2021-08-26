# This file contains default implementations for the `MOI.copy_to` function that
# can be used by a model.

@deprecate automatic_copy_to default_copy_to
@deprecate supports_default_copy_to MOI.supports_incremental_interface

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
    if attr == MOI.Name() && !MOI.supports(dest, attr)
        return  # Skipping names is okay.
    end
    value = MOI.get(src, attr)
    if value !== nothing
        MOI.set(dest, attr, map_indices(index_map, value))
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
    if (attr == MOI.VariableName() || attr == MOI.VariablePrimalStart()) &&
       !MOI.supports(dest, attr, MOI.VariableIndex)
        return  # Skipping names and start values is okay.
    end
    for x in vis_src
        value = MOI.get(src, attr, x)
        if value !== nothing
            MOI.set(dest, attr, index_map[x], map_indices(index_map, value))
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
    filter_constraints::Union{Nothing,Function} = nothing,
) where {F,S}
    if filter_constraints !== nothing
        filter!(filter_constraints, cis_src)
    end
    for attr in MOI.get(src, MOI.ListOfConstraintAttributesSet{F,S}())
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
    !MOI.supports(dest, attr, MOI.ConstraintIndex{F,S})
    if (
        attr == MOI.ConstraintName() ||
        attr == MOI.ConstraintPrimalStart() ||
        attr == MOI.ConstraintDualStart()
    ) && !MOI.supports(dest, attr, MOI.ConstraintIndex{F,S})
        return  # Skipping names and start values is okay
    end
    for ci in cis_src
        value = MOI.get(src, attr, ci)
        if value !== nothing
            MOI.set(dest, attr, index_map[ci], map_indices(index_map, value))
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
        filter_constraints::Union{Nothing,Function} = nothing,
    )

Copy the constraints `cis_src` from the model `src` to the model `dest` and fill
`index_map` accordingly. Note that the attributes are not copied; call
[`pass_attributes`] to copy the constraint attributes.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function _copy_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
    filter_constraints::Union{Nothing,Function} = nothing,
)
    if filter_constraints !== nothing
        filter!(filter_constraints, cis_src)
    end
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
    constraint_types;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    for (F, S) in constraint_types
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        _copy_constraints(dest, src, index_map, cis_src, filter_constraints)
    end
    return
end

"""
    pass_nonvariable_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        constraint_types;
        filter_constraints::Union{Nothing,Function} = nothing,
    )

For all tuples `(F, S)` in `constraint_types`, copy all constraints of type
`F`-in-`S` from `src` to `dest` mapping the variables indices with `index_map`.
If `filter_constraints` is not nothing, only indices `ci` such that
`filter_constraints(ci)` is true are copied.

The default implementation calls `pass_nonvariable_constraints_fallback`.
A method can be implemented to use a specialized copy for a given type of
`dest`.
"""
function pass_nonvariable_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    constraint_types;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    pass_nonvariable_constraints_fallback(
        dest,
        src,
        index_map,
        constraint_types;
        filter_constraints = filter_constraints,
    )
    return
end

function _pass_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    variable_constraints_not_added::Vector,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    for cis in variable_constraints_not_added
        _copy_constraints(dest, src, index_map, cis, filter_constraints)
    end
    all_constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    nonvariable_constraint_types = Any[
        (F, S) for (F, S) in all_constraint_types if !_is_variable_function(F)
    ]
    pass_nonvariable_constraints(
        dest,
        src,
        index_map,
        nonvariable_constraint_types;
        filter_constraints = filter_constraints,
    )
    for (F, S) in all_constraint_types
        pass_attributes(
            dest,
            src,
            index_map,
            MOI.get(src, MOI.ListOfConstraintIndices{F,S}()),
            filter_constraints,
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
    return (
        MOI.get(dest, MOI.VariableBridgingCost{S}()) -
        MOI.get(dest, MOI.ConstraintBridgingCost{MOI.VariableIndex,S}()),
        # In case of ties, we give priority to vector sets. See issue #987.
        false,
    )
end

function _cost_of_bridging(
    dest::MOI.ModelLike,
    ::Type{S},
) where {S<:MOI.AbstractVectorSet}
    return (
        MOI.get(dest, MOI.VariableBridgingCost{S}()) -
        MOI.get(dest, MOI.ConstraintBridgingCost{MOI.VectorOfVariables,S}()),
        # In case of ties, we give priority to vector sets. See issue #987
        true,
    )
end

"""
    sorted_variable_sets_by_cost(dest::MOI.ModelLike, src::MOI.ModelLike)

Returns a `Vector{Type}` of the set types corresponding to `VariableIndex` and
`VectorOfVariables` constraints in the order in which they should be added.
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
        "`default_copy_to` are now keyword arguments.",
        maxlog = 1,
    )
    return default_copy_to(dest, src; filter_constraints = filter_constraints)
end

"""
    default_copy_to(
        dest::MOI.ModelLike,
        src::MOI.ModelLike;
        filter_constraints::Union{Nothing,Function} = nothing,
    )

A default implementation of `MOI.copy_to(dest, src)` for models that implement
the incremental interface, i.e., [`MOI.supports_incremental_interface`](@ref)
returns `true`.

If `filter_constraints` is a `Function`, only constraints for which
`filter_constraints(ci)` returns `true` will be copied, where `ci` is the
[`MOI.ConstraintIndex`](@ref) of the constraint.
"""
function default_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike;
    filter_constraints::Union{Nothing,Function} = nothing,
)
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
    _pass_constraints(
        dest,
        src,
        index_map,
        constraints_not_added,
        filter_constraints,
    )
    final_touch(dest, index_map)
    return index_map
end
