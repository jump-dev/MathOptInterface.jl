abstract type ProductOfSets{T} end

set_index(sets::ProductOfSets, ::Type{S}) where {S<:MOI.AbstractSet} = nothing
function set_types end

function _sets_code(esc_name, T, type_def, set_types...)
    code = Expr(:block, type_def)
    esc_types = esc.(set_types)
    for (i, esc_type) in enumerate(esc_types)
        method = push!(
            code.args,
            :(
                function $MOIU.set_index(
                    ::$esc_name{$T},
                    ::Type{$esc_type},
                ) where {$T}
                    return $i
                end
            ),
        )
    end
    push!(code.args, :(function $MOIU.set_types(::$esc_name{$T}) where {$T}
        return [$(esc_types...)]
    end))
    return code
end

abstract type MixOfScalarSets{T} <: ProductOfSets{T} end
macro mix_of_scalar_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(struct $esc_name{$T} <: $MOIU.MixOfScalarSets{$T}
        set_ids::Vector{Int}
        function $esc_name{$T}() where {$T}
            return new(Int[])
        end
    end)
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::MixOfScalarSets) = isempty(sets.set_ids)
MOI.empty!(sets::MixOfScalarSets) = empty!(sets.set_ids)
number_of_rows(sets::MixOfScalarSets) = length(sets.set_ids)
rows(::MixOfScalarSets, ci::MOI.ConstraintIndex) = ci.value
function add_set(sets::MixOfScalarSets, i, dim)
    @assert dim == 1
    push!(sets.set_ids, i)
    return length(sets.set_ids)
end
function MOI.get(
    sets::MixOfScalarSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    present = Set(sets.set_ids)
    return [
        (affine_function_type(T, S), S) for
        S in set_types(sets) if set_index(sets, S) in present
    ]
end
function MOI.get(
    sets::MixOfScalarSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    i = set_index(sets, S)
    return count(isequal(i), sets.set_ids)
end
function MOI.get(
    sets::MixOfScalarSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    i = set_index(sets, S)
    return LazyMap{MOI.ConstraintIndex{F,S}}(
        j -> MOI.ConstraintIndex{F,S}(j),
        Base.Iterators.Filter(
            j -> sets.set_ids[j] == i,
            eachindex(sets.set_ids),
        ),
    )
end
function MOI.is_valid(
    sets::MixOfScalarSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    i = set_index(sets, S)
    return i !== nothing &&
           ci.value in eachindex(sets.set_ids) &&
           sets.set_ids[ci.value] == i
end

abstract type OrderedProductOfSets{T} <: ProductOfSets{T} end
macro product_of_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(
        struct $esc_name{$T} <: $MOIU.OrderedProductOfSets{$T}
            num_rows::Vector{Int}
            dimension::Dict{Tuple{Int,Int},Int}
            function $esc_name{$T}() where {$T}
                return new(zeros(Int, $(1 + length(set_types))), Dict{Int,Int}())
            end
        end
    )
    return _sets_code(esc_name, T, type_def, set_types...)
end

abstract type OrderedProductOfScalarSets{T} <: OrderedProductOfSets{T} end
macro product_of_scalar_sets(name, set_types...)
    esc_name = esc(name)
    T = esc(:T)
    type_def = :(struct $esc_name{$T} <: $MOIU.OrderedProductOfScalarSets{$T}
        num_rows::Vector{Int}
        function $esc_name{$T}() where {$T}
            return new(zeros(Int, $(1 + length(set_types))))
        end
    end)
    return _sets_code(esc_name, T, type_def, set_types...)
end

MOI.is_empty(sets::OrderedProductOfSets) = all(iszero, sets.num_rows)
function MOI.empty!(sets::OrderedProductOfSets)
    fill!(sets.num_rows, 0)
    empty!(sets.dimension)
    return
end
function MOI.empty!(sets::OrderedProductOfScalarSets)
    fill!(sets.num_rows, 0)
    return
end

function number_of_rows(sets::OrderedProductOfSets)
    for i in 3:length(sets.num_rows)
        sets.num_rows[i] += sets.num_rows[i-1]
    end
    return sets.num_rows[end]
end
function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    return sets.num_rows[i] + ci.value + 1
end
function rows(
    sets::OrderedProductOfSets{T},
    ci::MOI.ConstraintIndex{MOI.VectorAffineFunction{T},S},
) where {T,S}
    i = set_index(sets, S)
    return (sets.num_rows[i] + ci.value) .+ (1:sets.dimension[(i, ci.value)])
end
_set_dim(::OrderedProductOfScalarSets, i, offset, dim) = nothing
function _set_dim(sets::OrderedProductOfSets, i, offset, dim)
    sets.dimension[(i, offset)] = dim
    return
end
function add_set(sets::OrderedProductOfSets, i, dim)
    offset = sets.num_rows[i+1]
    _set_dim(sets, i, offset, dim)
    sets.num_rows[i+1] = offset + dim
    return offset
end
function num_rows(sets::OrderedProductOfSets, ::Type{S}) where {S}
    i = set_index(sets, S)
    return sets.num_rows[i+1] - sets.num_rows[i]
end
function MOI.get(
    sets::OrderedProductOfSets{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    return [
        (affine_function_type(T, S), S) for
        S in set_types(sets) if !iszero(num_rows(sets, S))
    ]
end
struct UnevenIterator
    i::Int
    start::Int
    stop::Int
    dimension::Dict{Tuple{Int,Int},Int}
end
Base.IteratorSize(::UnevenIterator) = Base.SizeUnknown()
function Base.iterate(it::UnevenIterator, cur = it.start)
    if cur >= it.stop
        return nothing
    else
        return (cur, cur + it.dimension[(it.i, cur)])
    end
end
function Base.in(it::UnevenIterator, x)
    return x in it.start:(it.stop-1) && haskey(it.dimension, (it.i, x))
end
function _range_iterator(
    ::OrderedProductOfSets{T},
    ::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return start:(stop-1)
end
function _range_iterator(
    sets::OrderedProductOfSets{T},
    i::Int,
    start::Int,
    stop::Int,
    ::Type{MOI.VectorAffineFunction{T}},
) where {T}
    return UnevenIterator(i, start, stop, sets.dimension)
end
function _range(
    sets::OrderedProductOfSets{T},
    ::Type{F},
    ::Type{S},
) where {T,F,S}
    i = set_index(sets, S)
    if F != affine_function_type(T, S) || i === nothing
        return nothing
    else
        return _range_iterator(
            sets,
            i,
            0,
            sets.num_rows[i+1] - sets.num_rows[i],
            F,
        )
    end
end
_length(r::UnitRange) = length(r)
_length(r::UnevenIterator) = count(_ -> true, r)
function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    r = _range(sets, F, S)
    if r === nothing
        return 0
    else
        return _length(r)
    end
end
function _empty(
    ::OrderedProductOfSets{T},
    ::Type{MOI.ScalarAffineFunction{T}},
) where {T}
    return 1:0
end
function _empty(
    sets::OrderedProductOfSets{T},
    ::Type{MOI.VectorAffineFunction{T}},
) where {T}
    return UnevenIterator(1, 1, 0, sets.dimension)
end

function MOI.get(
    sets::OrderedProductOfSets,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    rows = _range(sets, F, S)
    if rows === nothing
        # Empty iterator
        rows = _empty(sets, F)
    end
    return LazyMap{MOI.ConstraintIndex{F,S}}(
        i -> MOI.ConstraintIndex{F,S}(i),
        rows,
    )
end
function MOI.is_valid(
    sets::OrderedProductOfSets,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    r = _range(sets, F, S)
    return r !== nothing && ci.value in r
end

function set_number_of_rows(b::Vector, num_rows)
    return resize!(b, num_rows)
end
function load_constant(
    b::Vector{T},
    offset,
    func::MOI.VectorAffineFunction{T},
) where {T}
    copyto!(b, offset + 1, func.constants)
    return
end
#function load_constant(
#    b::Vector{T},
#    offset,
#    func::MOI.ScalarAffineFunction{T},
#) where {T}
#    b[offset + 1] = func.constant
#    return
#end

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
function set_number_of_rows(b::Box, n)
    resize!(b.lower, n)
    resize!(b.upper, n)
    return
end
function load_constant(b::Box{T}, offset, set) where {T}
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
        are_indices_mapped::BitSet
        caches::Union{Nothing, Vector}
        function MatrixOfConstraints{T,AT,BT,ST}() where {T,AT,BT,ST}
            return new{T,AT,BT,ST}(AT(), BT(), ST(), BitSet(), nothing)
        end
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
    are_indices_mapped::BitSet
    caches::Union{Nothing,Vector}
    function MatrixOfConstraints{T,AT,BT,ST}() where {T,AT,BT,ST}
        return new{T,AT,BT,ST}(AT(), BT(), ST(), BitSet(), nothing)
    end
end

MOI.is_empty(v::MatrixOfConstraints) = MOI.is_empty(v.sets)
function MOI.empty!(v::MatrixOfConstraints{T}) where {T}
    MOI.empty!(v.coefficients)
    empty!(v.constants)
    MOI.empty!(v.sets)
    empty!(v.are_indices_mapped)
    v.caches =
        [Tuple{affine_function_type(T, S),S}[] for S in set_types(v.sets)]
    return
end
rows(v::MatrixOfConstraints, ci::MOI.ConstraintIndex) = rows(v.sets, ci)

function affine_function_type(
    ::Type{T},
    ::Type{<:MOI.AbstractScalarSet},
) where {T}
    return MOI.ScalarAffineFunction{T}
end
function affine_function_type(
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
    return F == affine_function_type(T, S) && set_index(v.sets, S) !== nothing
end

function MOI.is_valid(v::MatrixOfConstraints, ci::MOI.ConstraintIndex)
    return MOI.is_valid(v.sets, ci)
end

function MOI.delete(v::MatrixOfConstraints, ci::MOI.ConstraintIndex)
    MOI.throw_if_not_valid(v, ci)
    MOI.delete(v.functions, rows(v.sets, ci))
    MOI.delete(v.sets, ci)
    return
end

function MOI.get(
    v::MatrixOfConstraints,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex,
)
    MOI.throw_if_not_valid(v, ci)
    return MOI.get(v.coefficients, attr, rows(v.sets, ci))
end

function MOI.get(
    v::MatrixOfConstraints,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex,
)
    MOI.throw_if_not_valid(v, ci)
    return MOI.get(v.sets, attr, ci)
end

function MOI.set(
    v::MatrixOfConstraints,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{F},
    func::F,
) where {F}
    MOI.throw_if_not_valid(v, ci)
    MOI.set(v.functions, attr, rows(v.sets, ci), func)
    return
end

function MOI.set(
    v::MatrixOfConstraints,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{F,S},
    set::S,
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    MOI.set(v.sets, attr, ci, set)
    return
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

function MOI.modify(
    v::MatrixOfConstraints,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(v.functions, rows(v.sets, ci), change)
    return
end

function _delete_variables(
    ::Function,
    ::MatrixOfConstraints,
    ::Vector{MOI.VariableIndex},
)
    return  # Nothing to do as it's not `VectorOfVariables` constraints
end

function _add_constraint(model::MatrixOfConstraints, i, index_map, func, set)
    allocate_terms(model.coefficients, index_map, func)
    # Without this type annotation, the compiler is unable to know the type
    # of `caches[i]` so this is slower and produce an allocation.
    push!(model.caches[i]::Vector{Tuple{typeof(func),typeof(set)}}, (func, set))
    return MOI.ConstraintIndex{typeof(func),typeof(set)}(
        add_set(model.sets, i, MOI.output_dimension(func)),
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
    if i === nothing || typeof(func) != affine_function_type(T, typeof(set))
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
    if i === nothing || F != affine_function_type(T, S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    cis_src = MOI.get(
        src,
        MOI.ListOfConstraintIndices{affine_function_type(T, S),S}(),
    )
    if filter_constraints !== nothing
        filter!(filter_constraints, cis_src)
    end
    for ci_src in cis_src
        func = MOI.get(src, MOI.CanonicalConstraintFunction(), ci_src)
        set = MOI.get(src, MOI.ConstraintSet(), ci_src)
        push!(model.are_indices_mapped, length(model.caches) + 1)
        index_map[ci_src] = _add_constraint(model, i, index_map, func, set)
    end
end

function _load_constant(
    constants,
    offset,
    func::MOI.AbstractScalarFunction,
    set::MOI.AbstractScalarSet,
)
    MOI.throw_if_scalar_and_constant_not_zero(func, typeof(set))
    return load_constant(constants, offset, set)
end
function _load_constant(
    constants,
    offset,
    func::MOI.AbstractVectorFunction,
    set::MOI.AbstractVectorSet,
)
    return load_constant(constants, offset, func)
end

function _load_constraints(
    dest::MatrixOfConstraints,
    index_map,
    offset,
    func_sets,
)
    for i in eachindex(func_sets)
        func, set = func_sets[i]
        index_map = if i in dest.are_indices_mapped
            index_map
        else
            IdentityMap()
        end
        load_terms(dest.coefficients, index_map, func, offset)
        _load_constant(dest.constants, offset, func, set)
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
    set_number_of_columns(
        dest.coefficients,
        MOI.get(src, MOI.NumberOfVariables()),
    )

    for (F, S) in constraint_types
        _allocate_constraints(dest, src, index_map, F, S, filter_constraints)
    end
end

function final_touch(model::MatrixOfConstraints, index_map)
    num_rows = number_of_rows(model.sets)
    set_number_of_rows(model.constants, num_rows)
    set_number_of_rows(model.coefficients, num_rows)

    offset = 0
    for cache in model.caches
        offset = _load_constraints(model, index_map, offset, cache)
    end

    final_touch(model.coefficients)
    empty!(model.are_indices_mapped)
    empty!(model.caches)
    return
end
