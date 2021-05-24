# Constants: stored in a `Vector` or `Box` or any other type implementing:
# `empty!`, `resize!` and `load_constants`.

"""
    load_constants(constants, offset, func_or_set)

This function loads the constants of `func_or_set` in `constants` at an offset
of `offset`. Where `offset` is the sum of the dimensions of the constraints
already loaded. The storage should be preallocated with `resize!` before calling
this function.

This function should be implemented to be usable as storage of constants for
[`MatrixOfConstraints`](@ref).

# The constants are loaded in three steps:
# 1) `Base.empty!` is called.
# 2) `Base.resize!` is called with the sum of the dimensions of all constraints.
# 3) `MOI.Utilities.load_constants` is called for each function for vector
#    constraint or set for scalar constraint.
"""
function load_constants end

function load_constants(
    b::Vector{T},
    offset,
    func::MOI.VectorAffineFunction{T},
) where {T}
    copyto!(b, offset + 1, func.constants)
    return
end

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
    b.lower[offset+1] = if iszero(flag & LOWER_BOUND_MASK)
        typemin(T)
    else
        extract_lower_bound(set)
    end
    b.upper[offset+1] = if iszero(flag & UPPER_BOUND_MASK)
        typemax(T)
    else
        extract_upper_bound(set)
    end
    return
end

"""
    mutable struct MatrixOfConstraints{T,AT,BT,ST} <: MOI.ModelLike
        coefficients::AT
        constants::BT
        sets::ST
        caches::Vector
        are_indices_mapped::Vector{BitSet}
    end

Represents affine constraints in a matrix form where the linear coefficients
of the functions are stored in the `coefficients` field and the constants of the
functions or sets are stored in the `sets` field. Additional information
about the sets are stored in the `sets` field.

This model can only be used as the `constraints` field of a
`MOI.Utilities.AbstractModel`. When the constraints are added,
they are stored in the `caches` field. They are only loaded in
the `coefficients` and `constants` fields once `MOI.Utilities.final_touch`
is called. For this reason, this should not be used with incremental
building of the model but with a `MOI.copy_to` instead.

The constraints can be added in two different ways:
1) With `add_constraint` in which case a canonicalized copy
   of the function is stored in `caches`.
2) With `pass_nonvariable_constraints` in which case the functions and sets are
   stored themselves in `caches` without mapping the variable indices.
   The corresponding index in `caches` is added in `are_indices_mapped`.
   This allows to avoid doing a copy of the function in case
   the getter of `CanonicalConstraintFunction` does not make a copy
   for the source model, e.g., this is the case of `VectorOfConstraints`.

We illustrate this with an example. Suppose a model is copied from
a `src::MOI.Utilities.Model` to a bridged model with a `MatrixOfConstraints`.
For all the types that are not bridged, the constraints will be copied
with `pass_nonvariable_constraints` hence the functions stored in
`caches` are exactly the same as the ones stored in `src`.
This is ok since this is only during the `copy_to` operation during which `src`
cannot be modified.
On the other hand, for the types that are bridged, the functions added
may contain duplicates even if the functions did not contain duplicates in
`src` so duplicates are removed with `MOI.Utilities.canonical`.
"""
mutable struct MatrixOfConstraints{T,AT,BT,ST} <: MOI.ModelLike
    coefficients::AT
    constants::BT
    sets::ST
    caches::Vector
    are_indices_mapped::Vector{BitSet}
    function MatrixOfConstraints{T,AT,BT,ST}() where {T,AT,BT,ST}
        return new{T,AT,BT,ST}(AT(), BT(), ST(), [], BitSet[])
    end
end

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

Return the rows corresponding to the constraint of index `ci`. If it is a
vector constraint, this is a `UnitRange`, otherwise, this is an integer.
"""
function rows(model::MatrixOfConstraints, ci::MOI.ConstraintIndex)
    return indices(model.sets, ci)
end

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

_add_set(sets, i, func::MOI.AbstractScalarFunction) = add_set(sets, i)
function _add_set(sets, i, func::MOI.AbstractVectorFunction)
    return add_set(sets, i, MOI.output_dimension(func))
end
function _add_constraint(model::MatrixOfConstraints, i, index_map, func, set)
    allocate_terms(model.coefficients, index_map, func)
    # Without this type annotation, the compiler is unable to know the type
    # of `caches[i]` so this is slower and produce an allocation.
    push!(model.caches[i]::Vector{Tuple{typeof(func),typeof(set)}}, (func, set))
    return MOI.ConstraintIndex{typeof(func),typeof(set)}(
        _add_set(model.sets, i, func),
    )
end

struct IdentityMap <: AbstractDict{MOI.VariableIndex,MOI.VariableIndex} end
Base.getindex(::IdentityMap, vi::MOI.VariableIndex) = vi

function MOI.add_constraint(
    model::MatrixOfConstraints{T},
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
) where {T}
    i = set_index(model.sets, typeof(set))
    if i === nothing || typeof(func) != _affine_function_type(T, typeof(set))
        throw(MOI.UnsupportedConstraint{typeof(func),typeof(set)}())
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
    set::MOI.AbstractVectorSet,
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
    for (cache, are_indices_mapped) in
        zip(model.caches, model.are_indices_mapped)
        offset = _load_constraints(
            model,
            index_map,
            offset,
            cache,
            are_indices_mapped,
        )
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
