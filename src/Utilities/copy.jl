# This file contains default implementations for the `MOI.copy_to` function that
# can be used by a model.

"""
    automatic_copy_to(
        dest::MOI.ModelLike,
        src::MOI.ModelLike;
        copy_names::Bool=true,
        filter_constraints::Union{Nothing,Function} = nothing,
    )

Use [`MathOptInterface.supports_incremental_interface`](@ref) and
[`Utilities.supports_allocate_load`](@ref) to automatically choose between
[`Utilities.default_copy_to`](@ref) or [`Utilities.allocate_load`](@ref) to
apply the copy operation.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function automatic_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike;
    copy_names::Bool = true,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    if MOI.supports_incremental_interface(dest, copy_names)
        return default_copy_to(dest, src, copy_names, filter_constraints)
    elseif supports_allocate_load(dest, copy_names)
        return allocate_load(dest, src, copy_names, filter_constraints)
    else
        error(
            "Model $(typeof(dest)) does not support copy",
            copy_names ? " with names" : "",
            ".",
        )
    end
end

@deprecate supports_default_copy_to MOI.supports_incremental_interface

include("copy/index_map.jl")

"""
    pass_model_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        index_map::IndexMap,
        pass_attributes::Function = MOI.set,
    )

Pass the model attributes from the model `src` to the model `dest` using
`pass_attributes`.

Does not copy [`Name`](@ref) if `copy_names` is `false`.
"""
function pass_model_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    index_map::IndexMap,
    pass_attributes::Function = MOI.set,
)
    for attr in MOI.get(src, MOI.ListOfModelAttributesSet())
        if attr == MOI.Name() && !copy_names
            continue
        end
        value = MOI.get(src, attr)
        if value !== nothing
            pass_attributes(dest, attr, map_indices(index_map, value))
        end
    end
    return
end

"""
    pass_variable_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        index_map::IndexMap,
        src_variables::Vector{MOI.VariableIndex},
        pass_attributes::Function = MOI.set,
    )

Pass the variable attributes from the model `src` to the model `dest` using
`pass_attributes`.

Does not copy [`VariableName`](@ref) if `copy_names` is `false`.
"""
function pass_variable_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    index_map::IndexMap,
    src_variables::Vector{MOI.VariableIndex},
    pass_attributes::Function = MOI.set,
)
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    if isempty(attrs)
        return
    end
    dest_variables = map(vi -> index_map[vi], src_variables)
    for attr in attrs
        if attr == MOI.VariableName() && !copy_names
            continue
        elseif attr == MOI.VariablePrimalStart()
            # As starting values are simply *hints* for the optimization, not
            # supporting them gives a warning, not an error
            if !MOI.supports(dest, attr, MOI.VariableIndex)
                @warn(
                    "$attr is not supported by $(typeof(dest)). This " *
                    "information will be discarded."
                )
                continue
            end
        end
        value = MOI.get(src, attr, src_variables)
        if value !== nothing
            mapped_value = map_indices(index_map, value)
            pass_attributes(dest, attr, dest_variables, mapped_value)
        end
    end
    return
end

"""
    pass_constraint_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        index_map::IndexMap,
        src_constraints::Vector{MOI.ConstraintIndex{F,S}},
        pass_attributes::Function = MOI.set,
    ) where {F,S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to
the model `dest` using `pass_attributes`.

Does not copy [`ConstraintName`](@ref) if `copy_names` is `false`.
"""
function pass_constraint_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    index_map::IndexMap,
    src_constraints::Vector{MOI.ConstraintIndex{F,S}},
    pass_attributes::Function = MOI.set,
) where {F,S}
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F,S}())
    if isempty(attrs)
        return
    end
    dest_constraints = map(ci -> index_map[ci], src_constraints)
    for attr in attrs
        if attr == MOI.ConstraintName() && !copy_names
            continue
        elseif attr == MOI.ConstraintPrimalStart() ||
               attr == MOI.ConstraintDualStart()
            # As starting values are simply *hints* for the optimization, not
            # supporting them gives a warning, not an error
            if !MOI.supports(dest, attr, MOI.ConstraintIndex{F,S})
                @warn(
                    "$attr is not supported by $(typeof(dest)). This " *
                    "information will be discarded."
                )
                continue
            end
        end
        value = MOI.get(src, attr, src_constraints)
        if value !== nothing
            mapped_value = map_indices(index_map, value)
            pass_attributes(dest, attr, dest_constraints, mapped_value)
        end
    end
    return
end

struct _VariableConstraintCache{F,S}
    indices::Vector{MOI.ConstraintIndex{F,S}}
    added::Vector{Bool}
end

"""
    _copy_constrained_variables(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        map::IndexMap,
        ::Type{S},
        filter_constraints::Union{Nothing,Function},
        copy_constrained_variables::Function = MOI.add_constrained_variables,
    ) where {S<:MOI.AbstractVectorSet}

Copy the constraints, using `copy_constrained_variables` of type
[`MOI.VectorOfVariables`](@ref)-in-`S` from `src` to `dest` and fill `index_map`
accordingly.

The copy is only done when the variables to be copied are not already keys of
`index_map`, the function contains unique variables, and
`filter_constraints(ci) == true` for the corresponding constraint index `ci`.
"""
function _copy_constrained_variables(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    map::IndexMap,
    ::Type{S},
    filter_constraints::Union{Nothing,Function},
    copy_constrained_variables::Function = MOI.add_constrained_variables,
) where {S<:MOI.AbstractVectorSet}
    src_constraints =
        MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}())
    if filter_constraints !== nothing
        filter!(filter_constraints, src_constraints)
    end
    added = fill(false, length(src_constraints))
    for (i, ci_src) in enumerate(src_constraints)
        f = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        # We can add this constraint via `copy_constrained_varaibles` under two
        # conditions:
        #  1. None of the variables have been added so far
        #  2. All of the variables in the function are unique
        if any(x -> haskey(map, x), f.variables) || !allunique(f.variables)
            continue
        end
        set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
        vis_dest, ci_dest = copy_constrained_variables(dest, set)
        map[ci_src] = ci_dest
        for (vi_src, vi_dest) in zip(f.variables, vis_dest)
            map[vi_src] = vi_dest
        end
        added[i] = true
    end
    return _VariableConstraintCache{MOI.VectorOfVariables,S}(
        src_constraints,
        added,
    )
end

"""
    _copy_constrained_variable(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        map::IndexMap,
        ::Type{S},
        filter_constraints::Union{Nothing,Function},
        copy_constrained_variable::Function = MOI.add_constrained_variable,
    ) where {S<:MOI.AbstractScalarSet}

Copy the constraints, using `copy_constrained_variables` of type
[`MOI.SingleVariable`](@ref)-in-`S` from `src` to `dest` and fill `index_map`
accordingly.

The copy is only done when the variables to be copied are not already keys of
`index_map`, the function contains unique variables, and
`filter_constraints(ci) == true` for the corresponding constraint index `ci`.
"""
function _copy_constrained_variable(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    map::IndexMap,
    ::Type{S},
    filter_constraints::Union{Nothing,Function},
    copy_constrained_variable::Function = MOI.add_constrained_variable,
) where {S<:MOI.AbstractScalarSet}
    src_constraints =
        MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable,S}())
    if filter_constraints !== nothing
        filter!(filter_constraints, src_constraints)
    end
    added = fill(false, length(src_constraints))
    for (i, ci_src) in enumerate(src_constraints)
        f = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        if haskey(map, f.variable)
            continue
        end
        set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
        vi_dest, ci_dest = copy_constrained_variable(dest, set)
        map[ci_src] = ci_dest
        map[f.variable] = vi_dest
        added[i] = true
    end
    return _VariableConstraintCache{MOI.SingleVariable,S}(
        src_constraints,
        added,
    )
end

"""
    _pass_free_variables(
        dest::MOI.ModelLike,
        map::IndexMap,
        variables::Vector{MOI.VariableIndex},
        copy_variables::Function = MOI.add_variables,
    )

When this is called, we have already dealt with variables appearing in
SingleVariable and VectorOfVariables constraints. However, there may be some
variables left that are free.
"""
function _pass_free_variables(
    dest::MOI.ModelLike,
    map::IndexMap,
    src_variables::Vector{MOI.VariableIndex},
    copy_variables::Function = MOI.add_variables,
)
    if length(src_variables) == length(map.var_map)
        return  # All variables have already been added. Nothing to do here.
    end
    new_variables =
        copy_variables(dest, length(src_variables) - length(map.var_map))
    i = 1
    for x in src_variables
        if !haskey(map, x)
            map[x] = new_variables[i]
            i += 1
        end
    end
    @assert length(src_variables) == length(map.var_map)
    return
end

"""
    _default_copy_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        index_map::IndexMap,
        cis_src::Vector{<:MOI.ConstraintIndex},
    )

Copy the constraints `cis_src` from the model `src` to the model `dest` and fill
`index_map` accordingly.
"""
function _default_copy_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{F,S}},
) where {F,S}
    f_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)
    s_src = MOI.get(src, MOI.ConstraintSet(), cis_src)
    f_dest = map_indices.(Ref(index_map), f_src)
    cis_dest = MOI.add_constraints(dest, f_dest, s_src)
    for (ci_src, ci_dest) in zip(cis_src, cis_dest)
        index_map[ci_src] = ci_dest
    end
    return
end

function _pass_constraint_cache(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    index_map::IndexMap,
    cache::_VariableConstraintCache{F,S},
    pass_constraints::Function,
    pass_attributes::Function,
) where {F,S}
    pass_constraints(
        dest,
        src,
        index_map,
        [i for (i, a) in zip(cache.indices, cache.added) if !a],
    )
    pass_constraint_attributes(
        dest,
        src,
        copy_names,
        index_map,
        cache.indices,
        pass_attributes,
    )
    return
end

function _pass_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    index_map::IndexMap,
    variable_constraint_cache::Vector{<:_VariableConstraintCache},
    filter_constraints::Union{Nothing,Function},
    pass_constraints::Function = _default_copy_constraints,
    pass_attributes::Function = MOI.set,
)
    # Copy MOI.SingleVariable and MOI.VectorOfVariables constraints.
    for cache in variable_constraint_cache
        _pass_constraint_cache(
            dest,
            src,
            copy_names,
            index_map,
            cache,
            pass_constraints,
            pass_attributes,
        )
    end
    # Copy remaining constraint types.
    for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
        if F == MOI.SingleVariable || F == MOI.VectorOfVariables
            continue
        end
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        pass_constraints(dest, src, index_map, cis_src)
        pass_constraint_attributes(
            dest,
            src,
            copy_names,
            index_map,
            cis_src,
            pass_attributes,
        )
    end
    return
end

function default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike)
    Base.depwarn(
        "default_copy_to(dest, src) is deprecated, use default_copy_to(dest, src, true) instead or default_copy_to(dest, src, false) if you do not want to copy names.",
        :default_copy_to,
    )
    return default_copy_to(dest, src, true)
end

function _bridge_cost(dest, ::Type{F}, ::Type{S}) where {F,S}
    # We give priority for sets such that there is a big cost reduction
    # constraining the variable on creation.
    cost =
        MOI.get(dest, MOI.VariableBridgingCost{S}()) -
        MOI.get(dest, MOI.ConstraintBridgingCost{F,S}())
    # In case of ties, we give priority to vector sets. See issue #987.
    return cost, F === MOI.SingleVariable
end

function sorted_variable_sets_by_cost(dest::MOI.ModelLike, src::MOI.ModelLike)
    constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    filter!(constraint_types) do (F, S)
        return F === MOI.SingleVariable || F === MOI.VectorOfVariables
    end
    sort!(constraint_types; by = x -> _bridge_cost(dest, x[1], x[2]))
    return constraint_types
end

function _try_constrain_variables_on_creation(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    index_map,
    copy_constrained_variables::Function,
    copy_constrained_variable::Function,
    filter_constraints::Union{Nothing,Function},
)
    variable_constraint_cache = _VariableConstraintCache[]
    for (F, S) in sorted_variable_sets_by_cost(dest, src)
        if F === MOI.VectorOfVariables
            push!(
                variable_constraint_cache,
                _copy_constrained_variables(
                    dest,
                    src,
                    index_map,
                    S,
                    filter_constraints,
                    copy_constrained_variables,
                ),
            )
        else
            @assert F === MOI.SingleVariable
            push!(
                variable_constraint_cache,
                _copy_constrained_variable(
                    dest,
                    src,
                    index_map,
                    S,
                    filter_constraints,
                    copy_constrained_variable,
                ),
            )
        end
    end
    return variable_constraint_cache
end

"""
    default_copy_to(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        filter_constraints::Union{Nothing,Function} = nothing,
    )

Implements `MOI.copy_to(dest, src)` by adding the variables and then the
constraints and attributes incrementally. The function
[`MathOptInterface.supports_incremental_interface`](@ref) can be used to check
whether `dest` supports the copying a model incrementally.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function default_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    MOI.empty!(dest)
    variables = MOI.get(src, MOI.ListOfVariableIndices())
    index_map = _index_map_for_variable_indices(variables)
    variable_constraint_cache = _VariableConstraintCache[]
    # The `NLPBlock` assumes that the order of variables does not change (#849)
    if MOI.NLPBlock() in MOI.get(src, MOI.ListOfModelAttributesSet())
        for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
            if !(F == MOI.SingleVariable || F == MOI.VectorOfVariables)
                continue
            end
            indices = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
            if filter_constraints !== nothing
                filter!(filter_constraints, indices)
            end
            added = fill(false, length(indices))
            push!(
                variable_constraint_cache,
                _VariableConstraintCache{F,S}(indices, added),
            )
        end
    else
        variable_constraint_cache = _try_constrain_variables_on_creation(
            dest,
            src,
            index_map,
            MOI.add_constrained_variables,
            MOI.add_constrained_variable,
            filter_constraints,
        )
    end

    _pass_free_variables(dest, index_map, variables)
    pass_variable_attributes(dest, src, copy_names, index_map, variables)
    pass_model_attributes(dest, src, copy_names, index_map)
    _pass_constraints(
        dest,
        src,
        copy_names,
        index_map,
        variable_constraint_cache,
        filter_constraints,
    )

    return index_map
end

include("copy/allocate_load.jl")
