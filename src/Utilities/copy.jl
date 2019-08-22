# This file contains default implementations for the `MOI.copy_to` function that can be used by a model.

"""
    automatic_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike;
                      copy_names::Bool=true)

Use [`Utilities.supports_default_copy_to`](@ref) and
[`Utilities.supports_allocate_load`](@ref) to automatically choose between
[`Utilities.default_copy_to`](@ref) or [`Utilities.allocate_load`](@ref) to
apply the copy operation.
"""
function automatic_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike;
                           copy_names::Bool=true)
    if supports_default_copy_to(dest, copy_names)
        default_copy_to(dest, src, copy_names)
    elseif supports_allocate_load(dest, copy_names)
        allocate_load(dest, src, copy_names)
    else
        error("Model $(typeof(dest)) does not support copy",
              copy_names ? " with names" : "", ".")
    end
end

"""
    supports_default_copy_to(model::ModelLike, copy_names::Bool)

Return a `Bool` indicating whether the model `model` supports
[`default_copy_to(model, src, copy_names=copy_names)`](@ref) if all the
attributes set to `src` and constraints added to `src` are supported by `model`.

This function can be used to determine whether a model can be loaded into
`model` incrementally or whether it should be cached and copied at once instead.
This is used by JuMP to determine whether to add a cache or not in two
situations:
1. A first cache can be used to store the model as entered by the user as well
   as the names of variables and constraints. This cache is created if this
   function returns `false` when `copy_names` is `true`.
2. If bridges are used, then a second cache can be used to store the bridged
   model with unnamed variables and constraints. This cache is created if this
   function returns `false` when `copy_names` is `false`.

## Examples

If [`MathOptInterface.set`](@ref), [`MathOptInterface.add_variable`](@ref) and
[`MathOptInterface.add_constraint`](@ref) are implemented for a model of type
`MyModel` and names are supported, then [`MathOptInterface.copy_to`](@ref) can
be implemented as
```julia
MOI.Utilities.supports_default_copy_to(model::MyModel, copy_names::Bool) = true
function MOI.copy_to(dest::MyModel, src::MOI.ModelLike; kws...)
    return MOI.Utilities.automatic_copy_to(dest, src; kws...)
end
```
The [`Utilities.automatic_copy_to`](@ref) function automatically redirects to
[`Utilities.default_copy_to`](@ref).

If names are not supported, simply change the first line by
```julia
MOI.supports_default_copy_to(model::MyModel, copy_names::Bool) = !copy_names
```
The [`Utilities.default_copy_to`](@ref) function automatically throws an helpful
error in case `copy_to` is called with `copy_names` equal to `true`.
"""
supports_default_copy_to(model::MOI.ModelLike, copy_names::Bool) = false

struct IndexMap <: AbstractDict{MOI.Index, MOI.Index}
    varmap::Dict{MOI.VariableIndex, MOI.VariableIndex}
    conmap::Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}
end
IndexMap() = IndexMap(Dict{MOI.VariableIndex, MOI.VariableIndex}(),
                      Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}())

Base.getindex(idxmap::IndexMap, vi::MOI.VariableIndex) = idxmap.varmap[vi]
function Base.getindex(idxmap::IndexMap, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    idxmap.conmap[ci]::MOI.ConstraintIndex{F, S}
end

Base.setindex!(idxmap::IndexMap, vi1::MOI.VariableIndex, vi2::MOI.VariableIndex) = Base.setindex!(idxmap.varmap, vi1, vi2)
function Base.setindex!(idxmap::IndexMap, ci1::MOI.ConstraintIndex{F, S}, ci2::MOI.ConstraintIndex{F, S}) where {F, S}
    Base.setindex!(idxmap.conmap, ci1, ci2)
end

Base.delete!(idxmap::IndexMap, vi::MOI.VariableIndex) = delete!(idxmap.varmap, vi)
Base.delete!(idxmap::IndexMap, ci::MOI.ConstraintIndex) = delete!(idxmap.conmap, ci)

Base.haskey(idxmap::IndexMap, ci::MOI.ConstraintIndex) = haskey(idxmap.conmap, ci)
Base.haskey(idxmap::IndexMap, vi::MOI.VariableIndex) = haskey(idxmap.varmap, vi)

Base.keys(idxmap::IndexMap) = Iterators.flatten((keys(idxmap.varmap), keys(idxmap.conmap)))

Base.length(idxmap::IndexMap) = length(idxmap.varmap) + length(idxmap.conmap)
Base.iterate(idxmap::MOIU.IndexMap, args...) = iterate(Base.Iterators.flatten((idxmap.varmap, idxmap.conmap)), args...)

"""
    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, pass_attr::Function=MOI.set)

Pass the model attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `Name` if `copy_names` is `false`.

    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, vis_src::Vector{MOI.VariableIndex}, pass_attr::Function=MOI.set)

Pass the variable attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `VariableName` if `copy_names` is `false`.

    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, cis_src::Vector{MOI.ConstraintIndex{F, S}}, pass_attr::Function=MOI.set) where {F, S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `ConstraintName` if `copy_names` is `false`.
"""
function pass_attributes end

function pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, pass_attr::Function=MOI.set)
    # Copy model attributes
    attrs = MOI.get(src, MOI.ListOfModelAttributesSet())
    _pass_attributes(dest, src, copy_names, idxmap, attrs, tuple(), tuple(), tuple(), pass_attr)
end
function pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, vis_src::Vector{VI}, pass_attr::Function=MOI.set)
    # Copy variable attributes
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    vis_dest = map(vi -> idxmap[vi], vis_src)
    _pass_attributes(dest, src, copy_names, idxmap, attrs, (VI,), (vis_src,), (vis_dest,), pass_attr)
end
function pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, cis_src::Vector{CI{F, S}}, pass_attr::Function=MOI.set) where {F, S}
    # Copy constraint attributes
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
    cis_dest = map(ci -> idxmap[ci], cis_src)
    _pass_attributes(dest, src, copy_names, idxmap, attrs, (CI{F, S},), (cis_src,), (cis_dest,), pass_attr)
end

function _pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike,
                          copy_names::Bool, idxmap::IndexMap, attrs,
                          supports_args, get_args, set_args,
                          pass_attr!::Function=MOI.set)
    for attr in attrs
        @assert MOI.is_copyable(attr)
        if !copy_names && (attr isa MOI.Name || attr isa MOI.VariableName || attr isa MOI.ConstraintName)
            continue
        end
        if attr isa MOI.VariablePrimalStart || attr isa MOI.ConstraintPrimalStart || attr isa MOI.ConstraintDualStart
            # As starting values are simply *hints* for the optimization, not
            # supporting them gives a warning, not an error
            if !MOI.supports(dest, attr, supports_args...)
                @warn("$attr is not supported by $(typeof(dest)). This ",
                             "information will be discarded.")
                continue
            end
        end
        value = MOI.get(src, attr, get_args...)
        if value !== nothing
            mapped_value = map_indices(idxmap, value)
            pass_attr!(dest, attr, set_args..., mapped_value)
        end
    end
end

"""
    copy_vector_of_variables(dest::MOI.ModelLike, src::MOI.ModelLike,
                             idxmap::IndexMap,
                             S::Type{<:MOI.AbstractSet})

Copy the constraints of type `MOI.VectorOfVariables`-in-`S` from the model `src`
to the model `dest` and fill `idxmap` accordingly. The copy is only done when
the variables to be copied are not already keys of `idxmap`. It returns a list
of the constraints not copied.
"""
function copy_vector_of_variables(dest::MOI.ModelLike, src::MOI.ModelLike,
                                  idxmap::IndexMap,
                                  S::Type{<:MOI.AbstractSet})
    not_added = MOI.ConstraintIndex{MOI.VectorOfVariables, S}[]
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables, S}())
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.VectorOfVariables}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if all(vi -> !haskey(idxmap, vi), f_src.variables)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vis_dest, ci_dest = MOI.add_constrained_variables(dest, set)
            idxmap[ci_src] = ci_dest
            for (vi_src, vi_dest) in zip(f_src.variables, vis_dest)
                idxmap[vi_src] = vi_dest
            end
        else
            push!(not_added, ci_src)
        end
    end
    return not_added
end

"""
    copy_single_variable(dest::MOI.ModelLike, src::MOI.ModelLike,
                          idxmap::IndexMap,
                          S::Type{<:MOI.AbstractSet})

Copy the constraints of type `MOI.SingleVariable`-in-`S` from the model `src` to
the model `dest` and fill `idxmap` accordingly. The copy is only done when the
variables to be copied are not already keys of `idxmap`. It returns a list of
the constraints not copied.
"""
function copy_single_variable(dest::MOI.ModelLike, src::MOI.ModelLike,
                               idxmap::IndexMap,
                               S::Type{<:MOI.AbstractSet})
    not_added = MOI.ConstraintIndex{MOI.SingleVariable, S}[]
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}())
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.SingleVariable}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if !haskey(idxmap, f_src.variable)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vi_dest, ci_dest = MOI.add_constrained_variable(dest, set)
            idxmap[ci_src] = ci_dest
            idxmap[f_src.variable] = vi_dest
        else
            push!(not_added, ci_src)
        end
    end
    return not_added
end

"""
    copy_constraints(dest::MOI.ModelLike, src::MOI.ModelLike,
                     idxmap::IndexMap,
                     cis_src::Vector{<:MOI.ConstraintIndex})

Copy the constraints `cis_src` from the model `src` to the model `dest` and fill
`idxmap` accordingly. Note that the attributes are not copied; call
[`pass_attributes`] to copy the constraint attributes.
"""
function copy_constraints(dest::MOI.ModelLike, src::MOI.ModelLike,
                          idxmap::IndexMap,
                          cis_src::Vector{<:MOI.ConstraintIndex})
    f_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)
    f_dest = map_indices.(Ref(idxmap), f_src)
    s = MOI.get(src, MOI.ConstraintSet(), cis_src)
    cis_dest = MOI.add_constraints(dest, f_dest, s)
    for (ci_src, ci_dest) in zip(cis_src, cis_dest)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function pass_constraints(
    dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap,
    single_variable_types, single_variable_indices,
    vector_of_variables_types, vector_of_variables_indices,
    pass_cons=copy_constraints, pass_attr=MOI.set
)
    for (S, cis_src) in zip(single_variable_types,
                            single_variable_indices)
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{
            MOI.SingleVariable, S}())
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end

    for (S, cis_src) in zip(vector_of_variables_types,
                            vector_of_variables_indices)
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{
            MOI.VectorOfVariables, S}())
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end

    nonvariable_constraint_types = [
        (F, S) for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        if F != MOI.SingleVariable && F != MOI.VectorOfVariables
    ]
    for (F, S) in nonvariable_constraint_types
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end
end

function default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike)
    Base.depwarn("default_copy_to(dest, src) is deprecated, use default_copy_to(dest, src, true) instead or default_copy_to(dest, src, false) if you do not want to copy names.", :default_copy_to)
    default_copy_to(dest, src, true)
end

"""
    default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool)

Implements `MOI.copy_to(dest, src)` by adding the variables and then the
constraints and attributes incrementally. The function
[`supports_default_copy_to`](@ref) can be used to check whether `dest` supports
the copying a model incrementally.
"""
function default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool)
    MOI.empty!(dest)

    idxmap = IndexMap()

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    constraint_types = MOI.get(src, MOI.ListOfConstraints())
    single_variable_types = [S for (F, S) in constraint_types
                             if F == MOI.SingleVariable]
    vector_of_variables_types = [S for (F, S) in constraint_types
                                 if F == MOI.VectorOfVariables]

    vector_of_variables_not_added = [
        copy_vector_of_variables(dest, src, idxmap, S)
        for S in vector_of_variables_types
    ]
    single_variable_not_added = [
        copy_single_variable(dest, src, idxmap, S)
        for S in single_variable_types
    ]

    if length(vis_src) != length(keys(idxmap.varmap))
        # Copy free variables
        variables_not_added = setdiff(Set(vis_src), keys(idxmap.varmap))
        vars = MOI.add_variables(dest, length(variables_not_added))
        for (vi, var) in zip(variables_not_added, vars)
            idxmap.varmap[vi] = var
        end
    end

    # Copy variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src)

    # Copy model attributes
    pass_attributes(dest, src, copy_names, idxmap)

    # Copy constraints
    pass_constraints(dest, src, copy_names, idxmap,
                     single_variable_types, single_variable_not_added,
                     vector_of_variables_types, vector_of_variables_not_added)

    return idxmap
end

# Allocate-Load Interface: 2-pass copy of a MathOptInterface model
# Some solver wrappers (e.g. SCS, ECOS, SDOI) do not supporting copying an optimization model using `MOI.add_constraints`, `MOI.add_variables` and `MOI.set`
# as they first need to figure out some information about a model before being able to pass the problem data to the solver.
#
# During the first pass (called allocate) : the model collects the relevant information about the problem so that
# on the second pass (called load), the constraints can be loaded directly to the solver (in case of SDOI) or written directly into the matrix of constraints (in case of SCS and ECOS).

# To support `MOI.copy_to` using this 2-pass mechanism, implement the allocate-load interface defined below and do:
# MOI.copy_to(dest::ModelType, src::MOI.ModelLike) = MOIU.allocate_load(dest, src)
# In the implementation of the allocate-load interface, it can be assumed that the different functions will the called in the following order:
# 1) `allocate_variables`
# 2) `allocate` and `allocate_constraint`
# 3) `load_variables`
# 4) `load` and `load_constraint`
# The interface is not meant to be used to create new constraints with `allocate_constraint` followed by `load_constraint` after a solve, it is only meant for being used in this order to implement `MOI.copy_to`.

# TODO deprecate this in MOI v0.7
"""
    needs_allocate_load(model::MOI.ModelLike)::Bool
Return a `Bool` indicating whether `model` does not support `add_variables`/`add_constraint`/`set` but supports `allocate_variables`/`allocate_constraint`/`allocate`/`load_variables`/`load_constraint`/`load`.
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
    allocate_constrained_variable(model::MOI.ModelLike,
                                  set::MOI.AbstractScalarSet)

Returns a tuple with the variable index and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variable(model::MOI.ModelLike,
                                       set::MOI.AbstractScalarSet)
    variables = allocate_variables(model, 1)
    variable = variables[1]
    func = MOI.SingleVariable(variable)
    constraint = allocate_constraint(model, func, set)
    return variable, constraint
end

"""
    allocate_constrained_variables(model::MOI.ModelLike,
                                   set::MOI.AbstractVectorSet)

Returns a tuple with the variable indices and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variables(model::MOI.ModelLike,
                                        set::MOI.AbstractVectorSet)
    variables = allocate_variables(model, MOI.dimension(set))
    func = MOI.VectorOfVariables(variables)
    constraint = allocate_constraint(model, func, set)
    return variables, constraint
end

const ALLOCATE_LOAD_NOT_IMPLEMENTED = ErrorException("The Allocate-Load interface is" *
                                                     " not implemented by the model")

"""
    allocate(model::ModelLike, attr::ModelLikeAttribute, value)
    allocate(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    allocate(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Informs `model` that `load` will be called with the same arguments after `load_variables` is called.
"""
function allocate(model::MOI.ModelLike, args...)
    MOI.throw_set_error_fallback(model, args...;
                                 error_if_supported=ALLOCATE_LOAD_NOT_IMPLEMENTED)
end

function allocate(model::MOI.ModelLike, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector, values::Vector)
    for (index, value) in zip(indices, values)
        allocate(model, attr, index, value)
    end
end

"""
    allocate_constraint(model::MOI.ModelLike, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Returns the index for the constraint to be used in `load_constraint` that will be called after `load_variables` is called.
"""
function allocate_constraint(model::MOI.ModelLike, func::MOI.AbstractFunction,
                             set::MOI.AbstractSet)
    MOI.throw_add_constraint_error_fallback(model, func, set;
                                            error_if_supported=ALLOCATE_LOAD_NOT_IMPLEMENTED)
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
        set::MOI.AbstractScalarSet)

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variable(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.SingleVariable},
        set::MOI.AbstractScalarSet)
    func = MOI.SingleVariable(vi)
    load_constraint(model, ci, func, set)
end

"""
    load_constrained_variables(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
        set::MOI.AbstractVectorSet)

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variables(
        model::MOI.ModelLike, vis::Vector{MOI.VariableIndex},
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
        set::MOI.AbstractVectorSet)
    func = MOI.VectorOfVariables(vis)
    load_constraint(model, ci, func, set)
end

"""
    load(model::ModelLike, attr::ModelLikeAttribute, value)
    load(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    load(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

This has the same effect that `set` with the same arguments except that `allocate` should be called first before `load_variables`.
"""
function load(model::MOI.ModelLike, args...)
    MOI.throw_set_error_fallback(model, args...;
                                 error_if_supported=ALLOCATE_LOAD_NOT_IMPLEMENTED)
end

function load(model::MOI.ModelLike, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector, values::Vector)
    for (index, value) in zip(indices, values)
        load(model, attr, index, value)
    end
end

"""
    allocate_single_variable(dest::MOI.ModelLike, src::MOI.ModelLike,
                             idxmap::IndexMap, S::Type{<:MOI.AbstractSet})

Allocate the constraints of type `MOI.SingleVariable`-in-`S` from the model
`src` to the model `dest` and fill `idxmap` accordingly. The copy is only done
when the variables to be copied are not already keys of `idxmap`. It returns a
list of the constraints allocated and those not allocated.
"""
function allocate_single_variable(dest::MOI.ModelLike, src::MOI.ModelLike,
                                  idxmap::IndexMap, S::Type{<:MOI.AbstractSet})
    allocated = MOI.ConstraintIndex{MOI.SingleVariable, S}[]
    not_allocated = MOI.ConstraintIndex{MOI.SingleVariable, S}[]
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}())
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.SingleVariable}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if !haskey(idxmap, f_src.variable)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vi_dest, ci_dest = MOI.add_constrained_variable(dest, set)
            idxmap[ci_src] = ci_dest
            idxmap[f_src.variable] = vi_dest
            push!(allocated, ci_src)
        else
            push!(not_allocated, ci_src)
        end
    end
    return allocated, not_allocated
end

"""
    allocate_vector_of_variables(dest::MOI.ModelLike, src::MOI.ModelLike,
                                 idxmap::IndexMap, S::Type{<:MOI.AbstractSet})

Allocate the constraints of type `MOI.VectorOfVariables`-in-`S` from the model
`src` to the model `dest` and fill `idxmap` accordingly. The copy is only done
when the variables to be copied are not already keys of `idxmap`. It returns a
list of the constraints allocated and a list of those not allocated.
"""
function allocate_vector_of_variables(dest::MOI.ModelLike, src::MOI.ModelLike,
                                      idxmap::IndexMap,
                                      S::Type{<:MOI.AbstractSet})
    allocated = MOI.ConstraintIndex{MOI.VectorOfVariables, S}[]
    not_allocated = MOI.ConstraintIndex{MOI.VectorOfVariables, S}[]
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables, S}())
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.VectorOfVariables}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if all(vi -> !haskey(idxmap, vi), f_src.variables)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vis_dest, ci_dest = allocate_constrained_variables(dest, set)
            idxmap[ci_src] = ci_dest
            for (vi_src, vi_dest) in zip(f_src.variables, vis_dest)
                idxmap[vi_src] = vi_dest
            end
            push!(allocated, ci_src)
        else
            push!(not_allocated, ci_src)
        end
    end
    return allocated, not_allocated
end

"""
    load_single_variable(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable, S}}) where S

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_single_variable(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable, S}}) where S
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.SingleVariable}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vi_dest = idxmap[f_src.variable]
        ci_dest = idxmap[ci_src]
        load_constrained_variable(dest, vi_dest, ci_dest, set)
    end
end

"""
    load_vector_of_variables(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables, S}}) where S

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_vector_of_variables(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables, S}}) where S
    fs_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)::Vector{MOI.VectorOfVariables}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vis_dest = [idxmap[vi] for vi in f_src.variables]
        ci_dest = idxmap[ci_src]
        load_constrained_variables(dest, vis_dest, ci_dest, set)
    end
end

"""
    load_constraint(model::MOI.ModelLike, ci::MOI.ConstraintIndex, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Sets the constraint function and set for the constraint of index `ci`.
"""
function load_constraint(model::MOI.ModelLike, ::MOI.ConstraintIndex,
                         func::MOI.AbstractFunction, set::MOI.AbstractSet)
    MOI.throw_add_constraint_error_fallback(model, func, set;
                                            error_if_supported=ALLOCATE_LOAD_NOT_IMPLEMENTED)
end

function allocate_constraints(dest::MOI.ModelLike, src::MOI.ModelLike,
                              idxmap::IndexMap, cis_src::Vector{<:MOI.ConstraintIndex})
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        ci_dest = allocate_constraint(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function load_constraints(dest::MOI.ModelLike, src::MOI.ModelLike,
                          idxmap::IndexMap, cis_src::Vector{<:MOI.ConstraintIndex})
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        load_constraint(dest, ci_dest, f_dest, s)
    end
end

"""
    allocate_load(dest::MOI.ModelLike, src::MOI.ModelLike)

Implements `MOI.copy_to(dest, src)` using the Allocate-Load API. The function
[`supports_allocate_load`](@ref) can be used to check whether `dest` supports
the Allocate-Load API.
"""
function allocate_load(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool)
    MOI.empty!(dest)

    idxmap = IndexMap()

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    constraint_types = MOI.get(src, MOI.ListOfConstraints())
    single_variable_types = [S for (F, S) in constraint_types
                             if F == MOI.SingleVariable]
    vector_of_variables_types = [S for (F, S) in constraint_types
                                 if F == MOI.VectorOfVariables]

    # Allocate variables
    vector_of_variables_allocated = Vector{Vector{<:MOI.ConstraintIndex{MOI.VectorOfVariables}}}(undef, length(vector_of_variables_types))
    vector_of_variables_not_allocated = Vector{Vector{<:MOI.ConstraintIndex{MOI.VectorOfVariables}}}(undef, length(vector_of_variables_types))
    for (i, S) in enumerate(vector_of_variables_types)
        vector_of_variables_allocated[i], vector_of_variables_not_allocated[i] =
            allocate_vector_of_variables(dest, src, idxmap, S)
    end

    single_variable_allocated = Vector{Vector{<:MOI.ConstraintIndex{MOI.SingleVariable}}}(undef, length(single_variable_types))
    single_variable_not_allocated = Vector{Vector{<:MOI.ConstraintIndex{MOI.SingleVariable}}}(undef, length(single_variable_types))
    for (i, S) in enumerate(single_variable_types)
        single_variable_allocated[i], single_variable_not_allocated[i] =
            allocate_single_variable(dest, src, idxmap, S)
    end

    if length(vis_src) != length(keys(idxmap.varmap))
        # Allocate free variables
        variables_not_added = setdiff(Set(vis_src), keys(idxmap.varmap))
        vars = allocate_variables(dest, length(variables_not_added))
        for (vi, var) in zip(variables_not_added, vars)
            idxmap.varmap[vi] = var
        end
    end

    # Allocate variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src, allocate)

    # Allocate model attributes
    pass_attributes(dest, src, copy_names, idxmap, allocate)

    # Allocate constraints
    pass_constraints(dest, src, copy_names, idxmap,
                     single_variable_types, single_variable_not_allocated,
                     vector_of_variables_types, vector_of_variables_not_allocated,
                     allocate_constraints, allocate)

    # Load variables
    vars = load_variables(dest, length(vis_src))
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
    pass_constraints(dest, src, copy_names, idxmap,
                     single_variable_types, single_variable_not_allocated,
                     vector_of_variables_types, vector_of_variables_not_allocated,
                     load_constraints, load)

    return idxmap
end
