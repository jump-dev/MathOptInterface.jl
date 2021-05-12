# TODO deprecate this in MOI v0.7
"""
    needs_allocate_load(model::MOI.ModelLike)::Bool

Return a `Bool` indicating whether `model` does not support `add_variables`/
`add_constraint`/`set` but supports `allocate_variables`/`allocate_constraint`
/`allocate`/`load_variables`/`load_constraint`/`load`.
That is, the allocate-load interface need to be used to copy an model to `model`.
"""
needs_allocate_load(::MOI.ModelLike) = false

"""
    supports_allocate_load(model::MOI.ModelLike, copy_names::Bool)::Bool

Return a `Bool` indicating whether `model` supports
[`allocate_load(model, src, copy_names=copy_names)`](@ref) if all the
attributes set to `src` and constraints added to `src` are supported by `model`.
"""
supports_allocate_load(::MOI.ModelLike, copy_names::Bool) = false

"""
    allocate_variables(model::MOI.ModelLike, nvars::Integer)

Creates `nvars` variables and returns a vector of `nvars` variable indices.
"""
function allocate_variables end

"""
    allocate_constrained_variable(
        model::MOI.ModelLike,
        set::MOI.AbstractScalarSet,
    )

Returns a tuple with the variable index and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variable(
    model::MOI.ModelLike,
    set::MOI.AbstractScalarSet,
)
    variables = allocate_variables(model, 1)
    variable = variables[1]
    func = MOI.SingleVariable(variable)
    constraint = allocate_constraint(model, func, set)
    return variable, constraint
end

"""
    allocate_constrained_variables(
        model::MOI.ModelLike,
        set::MOI.AbstractVectorSet,
    )

Returns a tuple with the variable indices and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variables(
    model::MOI.ModelLike,
    set::MOI.AbstractVectorSet,
)
    variables = allocate_variables(model, MOI.dimension(set))
    func = MOI.VectorOfVariables(variables)
    constraint = allocate_constraint(model, func, set)
    return variables, constraint
end

const ALLOCATE_LOAD_NOT_IMPLEMENTED = ErrorException(
    "The Allocate-Load interface is" * " not implemented by the model",
)

"""
    allocate(
        model::ModelLike,
        attr::ModelLikeAttribute,
        value,
    )

    allocate(
        model::ModelLike,
        attr::AbstractVariableAttribute,
        v::VariableIndex,
        value,
    )

    allocate(
        model::ModelLike,
        attr::AbstractConstraintAttribute,
        c::ConstraintIndex,
        value,
    )

Informs `model` that `load` will be called with the same arguments after
`load_variables` is called.
"""
function allocate(model::MOI.ModelLike, args...)
    return MOI.throw_set_error_fallback(
        model,
        args...;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function allocate(
    model::MOI.ModelLike,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    indices::Vector,
    values::Vector,
)
    for (index, value) in zip(indices, values)
        allocate(model, attr, index, value)
    end
    return
end

"""
    allocate_constraint(
        model::MOI.ModelLike,
        f::MOI.AbstractFunction,
        s::MOI.AbstractSet,
    )

Returns the index for the constraint to be used in `load_constraint` that will
be called after `load_variables` is called.
"""
function allocate_constraint(
    model::MOI.ModelLike,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.throw_add_constraint_error_fallback(
        model,
        func,
        set;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

"""
    load_variables(model::MOI.ModelLike, nvars::Integer)

Prepares `model` for [`load`](@ref) and [`load_constraint`](@ref).
"""
function load_variables end

"""
    load_constrained_variable(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.SingleVariable},
        set::MOI.AbstractScalarSet,
    )

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variable(
    model::MOI.ModelLike,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{MOI.SingleVariable},
    set::MOI.AbstractScalarSet,
)
    func = MOI.SingleVariable(vi)
    load_constraint(model, ci, func, set)
    return
end

"""
    load_constrained_variables(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
        set::MOI.AbstractVectorSet,
    )

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variables(
    model::MOI.ModelLike,
    vis::Vector{MOI.VariableIndex},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    set::MOI.AbstractVectorSet,
)
    func = MOI.VectorOfVariables(vis)
    load_constraint(model, ci, func, set)
    return
end

"""
    load(
        model::ModelLike,
        attr::ModelLikeAttribute,
        value,
    )

    load(
        model::ModelLike,
        attr::AbstractVariableAttribute,
        v::VariableIndex,
        value,
    )

    load(
        model::ModelLike,
        attr::AbstractConstraintAttribute,
        c::ConstraintIndex,
        value,
    )

This has the same effect that `set` with the same arguments except that
`allocate` should be called first before `load_variables`.
"""
function load(model::MOI.ModelLike, args...)
    return MOI.throw_set_error_fallback(
        model,
        args...;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function load(
    model::MOI.ModelLike,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    indices::Vector,
    values::Vector,
)
    for (index, value) in zip(indices, values)
        load(model, attr, index, value)
    end
    return
end

"""
    load_single_variable(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable, S}},
    ) where {S}

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_single_variable(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable,S}},
) where {S}
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.SingleVariable}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vi_dest = idxmap[f_src.variable]
        ci_dest = idxmap[ci_src]
        load_constrained_variable(dest, vi_dest, ci_dest, set)
    end
    return
end

"""
    load_vector_of_variables(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables, S}},
    ) where {S}

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_vector_of_variables(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables,S}},
) where {S}
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.VectorOfVariables}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vis_dest = [idxmap[vi] for vi in f_src.variables]
        ci_dest = idxmap[ci_src]
        load_constrained_variables(dest, vis_dest, ci_dest, set)
    end
    return
end

"""
    load_constraint(
        model::MOI.ModelLike,
        ci::MOI.ConstraintIndex,
        f::MOI.AbstractFunction,
        s::MOI.AbstractSet,
    )

Sets the constraint function and set for the constraint of index `ci`.
"""
function load_constraint(
    model::MOI.ModelLike,
    ::MOI.ConstraintIndex,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.throw_add_constraint_error_fallback(
        model,
        func,
        set;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function allocate_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
)
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        ci_dest = allocate_constraint(dest, f_dest, s)
        idxmap[ci_src] = ci_dest
    end
end

function load_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
)
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        load_constraint(dest, ci_dest, f_dest, s)
    end
    return
end

"""
    allocate_load(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        filter_constraints::Union{Nothing,Function} = nothing,
    )

Implements `MOI.copy_to(dest, src)` using the Allocate-Load API. The function
[`supports_allocate_load`](@ref) can be used to check whether `dest` supports
the Allocate-Load API.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function allocate_load(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    MOI.empty!(dest)

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    idxmap = _index_map_for_variable_indices(vis_src)
    constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    single_variable_types =
        [S for (F, S) in constraint_types if F === MOI.SingleVariable]
    vector_of_variables_types =
        [S for (F, S) in constraint_types if F === MOI.VectorOfVariables]

    # Allocate variables
    vector_of_variables_types,
    vector_of_variables_allocated,
    vector_of_variables_not_allocated,
    single_variable_type,
    single_variable_allocated,
    single_variable_not_allocated = try_constrain_variables_on_creation(
        dest,
        src,
        idxmap,
        allocate_constrained_variables,
        allocate_constrained_variable,
    )

    copy_free_variables(dest, idxmap, vis_src, allocate_variables)

    # Allocate variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src, allocate)

    # Allocate model attributes
    pass_attributes(dest, src, copy_names, idxmap, allocate)

    # Allocate constraints
    pass_constraints(
        dest,
        src,
        copy_names,
        idxmap,
        single_variable_types,
        single_variable_not_allocated,
        vector_of_variables_types,
        vector_of_variables_not_allocated,
        allocate_constraints,
        allocate,
        filter_constraints = filter_constraints,
    )

    # Load variables
    load_variables(dest, length(vis_src))
    for cis_src in vector_of_variables_allocated
        load_vector_of_variables(dest, src, idxmap, cis_src)
    end
    for cis_src in single_variable_allocated
        load_single_variable(dest, src, idxmap, cis_src)
    end

    # Load variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src, load)

    # Load model attributes
    pass_attributes(dest, src, copy_names, idxmap, load)

    # Load constraints
    pass_constraints(
        dest,
        src,
        copy_names,
        idxmap,
        single_variable_types,
        single_variable_not_allocated,
        vector_of_variables_types,
        vector_of_variables_not_allocated,
        load_constraints,
        load,
        filter_constraints = filter_constraints,
    )

    return idxmap
end
