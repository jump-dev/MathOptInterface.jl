"""
    mutable struct MatrixOfConstraints{T,AT,BT,ST} <: MOI.ModelLike
        coefficients::AT
        constants::BT
        sets::ST
        caches::Vector{Any}
        are_indices_mapped::Vector{BitSet}
    end

Represent `ScalarAffineFunction` and `VectorAffinefunction` constraints in a
matrix form where the linear coefficients of the functions are stored in the
`coefficients` field, the constants of the functions or sets are stored in the
`constants` field. Additional information about the sets are stored in the
`sets` field.

This model can only be used as the `constraints` field of a
`MOI.Utilities.AbstractModel`.

When the constraints are added, they are stored in the `caches` field. They are
only loaded in the `coefficients` and `constants` fields once
`MOI.Utilities.final_touch` is called. For this reason, `MatrixOfConstraints`
should not be used by an incremental interface. Use `MOI.copy_to` instead.

The constraints can be added in two different ways:

1) With `add_constraint`, in which case a canonicalized copy of the function is
   stored in `caches`.
2) With `pass_nonvariable_constraints`, in which case the functions and sets are
   stored themselves in `caches` without mapping the variable indices. The
   corresponding index in `caches` is added in `are_indices_mapped`. This avoids
   doing a copy of the function in case the getter of
   `CanonicalConstraintFunction` does not make a copy for the source model,
   e.g., this is the case of `VectorOfConstraints`.

We illustrate this with an example. Suppose a model is copied from a
`src::MOI.Utilities.Model` to a bridged model with a `MatrixOfConstraints`. For
all the types that are not bridged, the constraints will be copied with
`pass_nonvariable_constraints`. Hence the functions stored in `caches` are
exactly the same as the ones stored in `src`. This is ok since this is only
during the `copy_to` operation during which `src` cannot be modified. On the
other hand, for the types that are bridged, the functions added may contain
duplicates even if the functions did not contain duplicates in `src` so
duplicates are removed with `MOI.Utilities.canonical`.

## Interface

The `.coefficients::AT` type must implement:
 * `AT()`
 * `MOI.empty(::AT)!`
 * [`MOI.Utilities.add_column`](@ref)
 * [`MOI.Utilities.set_number_of_rows`](@ref)
 * [`MOI.Utilities.allocate_terms`](@ref)
 * [`MOI.Utilities.load_terms`](@ref)
 * [`MOI.Utilities.final_touch`](@ref)

The `.constants::BT` type must implement:

 * `BT()`
 * `Base.empty!(::BT)`
 * `Base.resize(::BT)`
 * [`MOI.Utilities.load_constants`](@ref)

The `.sets::ST` type must implement:

 * `ST()`
 * `MOI.is_empty(::ST)`
 * `MOI.empty(::ST)`
 * `MOI.dimension(::ST)`
 * `MOI.is_valid(::ST, ::MOI.ConstraintIndex)`
 * `MOI.get(::ST, ::MOI.ListOfConstraintTypesPresent)`
 * `MOI.get(::ST, ::MOI.NumberOfConstraints)`
 * `MOI.get(::ST, ::MOI.ListOfConstraintIndices)`
 * [`MOI.Utilities.set_types`](@ref)
 * [`MOI.Utilities.set_index`](@ref)
 * [`MOI.Utilities.add_set`](@ref)
 * [`MOI.Utilities.rows`](@ref)
"""
mutable struct MatrixOfConstraints{T,AT,BT,ST} <: MOI.ModelLike
    coefficients::AT
    constants::BT
    sets::ST
    caches::Vector{Any}
    are_indices_mapped::Vector{BitSet}
    function MatrixOfConstraints{T,AT,BT,ST}() where {T,AT,BT,ST}
        return new{T,AT,BT,ST}(AT(), BT(), ST(), Any[], BitSet[])
    end
end

###
### Interface for the .coefficients field
###

"""
    add_column(coefficients)::Nothing

Tell `coefficients` to pre-allocate datastructures as needed to store one
column.
"""
function add_column end

"""
    set_number_of_rows(coefficients, n)::Nothing

Tell `coefficients` to pre-allocate datastructures as needed to store `n` rows.
"""
function set_number_of_rows end

"""
    allocate_terms(coefficients, index_map, func)::Nothing

Tell `coefficients` that the terms of the function `func` where the variable
indices are mapped with `index_map` will be loaded with [`load_terms`](@ref).

The function `func` must be canonicalized before calling `allocate_terms`. See
[`is_canonical`](@ref).
"""
function allocate_terms end

"""
    load_terms(coefficients, index_map, func, offset)::Nothing

Loads the terms of `func` to `coefficients`, mapping the variable indices with
`index_map`.

The `i`th dimension of `func` is loaded at the `(offset + i)`th row of
`coefficients`.

The function must be allocated first with [`allocate_terms`](@ref).

The function `func` must be canonicalized, see [`is_canonical`](@ref).
"""
function load_terms end

"""
    final_touch(coefficients)::Nothing

Informs the `coefficients` that all functions have been added with `load_terms`.
No more modification is allowed unless `MOI.empty!` is called.
"""
function final_touch end

###
### Interface for the .constants field
###

"""
    load_constants(constants, offset, func_or_set)::Nothing

This function loads the constants of `func_or_set` in `constants` at an offset
of `offset`. Where `offset` is the sum of the dimensions of the constraints
already loaded. The storage should be preallocated with `resize!` before calling
this function.

This function should be implemented to be usable as storage of constants for
[`MatrixOfConstraints`](@ref).

The constants are loaded in three steps:
 1) `Base.empty!` is called.
 2) `Base.resize!` is called with the sum of the dimensions of all constraints.
 3) `MOI.Utilities.load_constants` is called for each function for vector
    constraint or set for scalar constraint.
"""
function load_constants end

###
### Interface for the .sets field
###

"""
    set_types(sets)::Vector{DataType}

Return the list of the types of the sets allowed in `sets`.
"""
function set_types end

"""
    set_index(sets, ::Type{S})::Union{Int,Nothing} where {S<:MOI.AbstractSet}

Return an integer corresponding to the index of the set type in the list given
by [`set_types`](@ref).

If `S` is not part of the list, return `nothing`.
"""
function set_index end

"""
    add_set(sets, i)::Int64

Add a scalar set of type index `i`.

    add_set(sets, i, dim)::Int64

Add a vector set of type index `i` and dimension `dim`.

Both methods return a unique `Int64` of the set that can be used to reference
this set.
"""
function add_set end

"""
    rows(sets, ci::MOI.ConstraintIndex)::Union{Int,UnitRange{Int}}

Return the rows in `1:MOI.dimension(sets)` corresponding to the set of id
`ci.value`.

For scalar sets, this returns an `Int`. For vector sets, this returns an
`UnitRange{Int}`.
"""
function rows end

###
### MatrixOfConstraints
###

MOI.is_empty(v::MatrixOfConstraints) = MOI.is_empty(v.sets)

function MOI.empty!(v::MatrixOfConstraints{T}) where {T}
    MOI.empty!(v.coefficients)
    empty!(v.constants)
    MOI.empty!(v.sets)
    v.caches =
        [Tuple{_affine_function_type(T, S),S}[] for S in set_types(v.sets)]
    v.are_indices_mapped = [BitSet() for _ in eachindex(v.caches)]
    return
end

"""
    rows(model::MatrixOfConstraints, ci::MOI.ConstraintIndex)

Return the rows in `1:MOI.dimension(sets)` corresponding to the set of id
`ci.value`.

For scalar sets, this returns an `Int`. For vector sets, this returns an
`UnitRange{Int}`.
"""
rows(model::MatrixOfConstraints, ci::MOI.ConstraintIndex) = rows(model.sets, ci)

function _affine_function_type(
    ::Type{T},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function _affine_function_type(
    ::Type{T},
    ::Type{<:MOI.AbstractVectorSet},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function MOI.supports_constraint(
    v::MatrixOfConstraints{T},
    ::Type{F},
    ::Type{S},
) where {T,F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return F == _affine_function_type(T, S) && set_index(v.sets, S) !== nothing
end

function MOI.is_valid(v::MatrixOfConstraints, ci::MOI.ConstraintIndex)
    return MOI.is_valid(v.sets, ci)
end

function MOI.get(
    v::MatrixOfConstraints,
    attr::Union{
        MOI.ListOfConstraintTypesPresent,
        MOI.NumberOfConstraints,
        MOI.ListOfConstraintIndices,
    },
)
    return MOI.get(v.sets, attr)
end

_add_set(sets, i, ::MOI.AbstractScalarFunction) = add_set(sets, i)

function _add_set(sets, i, func::MOI.AbstractVectorFunction)
    return add_set(sets, i, MOI.output_dimension(func))
end

function _add_constraint(
    model::MatrixOfConstraints,
    i::Int,
    index_map,
    func::F,
    set::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    allocate_terms(model.coefficients, index_map, func)
    # Without this type annotation, the compiler is unable to know the type
    # of `caches[i]` so this is slower and produce an allocation.
    push!(model.caches[i]::Vector{Tuple{F,S}}, (func, set))
    return MOI.ConstraintIndex{F,S}(_add_set(model.sets, i, func))
end

struct IdentityMap <: AbstractDict{MOI.VariableIndex,MOI.VariableIndex} end

Base.getindex(::IdentityMap, vi::MOI.VariableIndex) = vi

function MOI.add_constraint(
    model::MatrixOfConstraints{T},
    func::F,
    set::S,
) where {T,F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    i = set_index(model.sets, S)
    if i === nothing || F != _affine_function_type(T, S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    return _add_constraint(model, i, IdentityMap(), func, set)
end

function _allocate_constraints(
    model::MatrixOfConstraints{T},
    src,
    index_map,
    ::Type{F},
    ::Type{S},
    filter_constraints::Union{Nothing,Function},
) where {T,F,S}
    i = set_index(model.sets, S)
    if i === nothing || F != _affine_function_type(T, S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    cis_src = MOI.get(
        src,
        MOI.ListOfConstraintIndices{_affine_function_type(T, S),S}(),
    )
    if filter_constraints !== nothing
        filter!(filter_constraints, cis_src)
    end
    for ci_src in cis_src
        func = MOI.get(src, MOI.CanonicalConstraintFunction(), ci_src)
        set = MOI.get(src, MOI.ConstraintSet(), ci_src)
        push!(model.are_indices_mapped[i], length(model.caches[i]) + 1)
        index_map[ci_src] = _add_constraint(model, i, index_map, func, set)
    end
    return
end

function _load_constants(
    constants,
    offset,
    func::MOI.AbstractScalarFunction,
    set::MOI.AbstractScalarSet,
)
    MOI.throw_if_scalar_and_constant_not_zero(func, typeof(set))
    return load_constants(constants, offset, set)
end

function _load_constants(
    constants,
    offset,
    func::MOI.AbstractVectorFunction,
    ::MOI.AbstractVectorSet,
)
    return load_constants(constants, offset, func)
end

function _load_constraints(
    dest::MatrixOfConstraints,
    index_map,
    offset,
    func_sets,
    are_indices_mapped,
)
    for i in eachindex(func_sets)
        func, set = func_sets[i]
        if i in are_indices_mapped
            load_terms(dest.coefficients, index_map, func, offset)
        else
            load_terms(dest.coefficients, IdentityMap(), func, offset)
        end
        _load_constants(dest.constants, offset, func, set)
        offset += MOI.output_dimension(func)
    end
    return offset
end

_add_variable(model::MatrixOfConstraints) = add_column(model.coefficients)

function pass_nonvariable_constraints(
    dest::MatrixOfConstraints,
    src::MOI.ModelLike,
    index_map::IndexMap,
    constraint_types,
    pass_cons = copy_constraints;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    for (F, S) in constraint_types
        _allocate_constraints(dest, src, index_map, F, S, filter_constraints)
    end
    return
end

function final_touch(model::MatrixOfConstraints, index_map)
    num_rows = MOI.dimension(model.sets)
    resize!(model.constants, num_rows)
    set_number_of_rows(model.coefficients, num_rows)
    offset = 0
    for (cache, mapped_indices) in zip(model.caches, model.are_indices_mapped)
        offset =
            _load_constraints(model, index_map, offset, cache, mapped_indices)
    end
    final_touch(model.coefficients)
    empty!(model.caches)
    empty!(model.are_indices_mapped)
    return
end

# Users of `MatrixOfConstraints` assume variable indices to be `1:n` where `n`
# is the number of columns so we don't support variable deletion.
function _throw_if_cannot_delete(
    ::MatrixOfConstraints,
    ::Vector{MOI.VariableIndex},
    vi::MOI.VariableIndex,
)
    return throw(MOI.DeleteNotAllowed(vi))
end

function _throw_if_cannot_delete(
    ::MatrixOfConstraints,
    vis::Vector{MOI.VariableIndex},
    ::Set{MOI.VariableIndex},
)
    return throw(MOI.DeleteNotAllowed(first(vis)))
end

###
### .constants::Vector
###

# Base.empty! is already implemented.
# Base.resize! is already implemented.

function load_constants(
    b::Vector{T},
    offset,
    func::MOI.VectorAffineFunction{T},
) where {T}
    copyto!(b, offset + 1, func.constants)
    return
end

###
### .constants::Box
###

"""
    struct Box{T}
        lower::Vector{T}
        upper::Vector{T}
    end

Stores the constants of scalar constraints with the lower bound of the set in
`lower` and the upper bound in `upper`.
"""
struct Box{T}
    lower::Vector{T}
    upper::Vector{T}
end

Box{T}() where {T} = Box{T}(T[], T[])

Base.:(==)(a::Box, b::Box) = a.lower == b.lower && a.upper == b.upper

function Base.empty!(b::Box)
    empty!(b.lower)
    empty!(b.upper)
    return b
end

function Base.resize!(b::Box, n)
    resize!(b.lower, n)
    resize!(b.upper, n)
    return
end

function load_constants(
    b::Box{T},
    offset,
    set::SUPPORTED_VARIABLE_SCALAR_SETS{T},
) where {T}
    flag = single_variable_flag(typeof(set))
    if iszero(flag & LOWER_BOUND_MASK)
        b.lower[offset+1] = typemin(T)
    else
        b.lower[offset+1] = extract_lower_bound(set)
    end
    if iszero(flag & UPPER_BOUND_MASK)
        b.upper[offset+1] = typemax(T)
    else
        b.upper[offset+1] = extract_upper_bound(set)
    end
    return
end
