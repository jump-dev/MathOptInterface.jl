function test_models_equal(args...)
    @warn(
        "`Utilities.test_models_equal` has been moved to " *
        "`Test.util_test_models_equal`",
        maxlog = 1,
    )
    return MOI.Test.util_test_models_equal(args...)
end

variable_function_type(::Type{<:MOI.AbstractScalarSet}) = MOI.VariableIndex
variable_function_type(::Type{<:MOI.AbstractVectorSet}) = MOI.VectorOfVariables

"""
    eval_variables(varval::Function, f::AbstractFunction)

Returns the value of function `f` if each variable index `vi` is evaluated as
`varval(vi)`. Note that `varval` should return a number, see
[`substitute_variables`](@ref) for a similar function where `varval` returns a
function.
"""
function eval_variables end

function eval_variables(varval::Function, f::MOI.VariableIndex)
    return varval(f)
end

function eval_variables(varval::Function, f::MOI.VectorOfVariables)
    return varval.(f.variables)
end

function eval_variables(varval::Function, f::MOI.ScalarAffineFunction)
    out = f.constant
    for t in f.terms
        out += eval_term(varval, t)
    end
    return out
end

function eval_variables(varval::Function, f::MOI.VectorAffineFunction)
    out = copy(f.constants)
    for t in f.terms
        out[t.output_index] += eval_term(varval, t.scalar_term)
    end
    return out
end

function eval_variables(varval::Function, f::MOI.ScalarQuadraticFunction)
    out = f.constant
    for a in f.affine_terms
        out += eval_term(varval, a)
    end
    for q in f.quadratic_terms
        out += eval_term(varval, q)
    end
    return out
end

function eval_variables(varval::Function, f::MOI.VectorQuadraticFunction)
    out = copy(f.constants)
    for t in f.affine_terms
        out[t.output_index] += eval_term(varval, t.scalar_term)
    end
    for t in f.quadratic_terms
        out[t.output_index] += eval_term(varval, t.scalar_term)
    end
    return out
end

function eval_term(varval::Function, t::MOI.ScalarAffineTerm)
    return t.coefficient * varval(t.variable)
end

function eval_term(varval::Function, t::MOI.ScalarQuadraticTerm)
    tval = t.coefficient * varval(t.variable_1) * varval(t.variable_2)
    return t.variable_1 == t.variable_2 ? tval / 2 : tval
end

"""
    map_indices(index_map::Function, attr::MOI.AnyAttribute, x::X)::X where {X}

Substitute any [`MOI.VariableIndex`](@ref) (resp. [`MOI.ConstraintIndex`](@ref))
in `x` by the [`MOI.VariableIndex`](@ref) (resp. [`MOI.ConstraintIndex`](@ref))
of the same type given by `index_map(x)`.

## When to implement this method for new types `X`

This function is used by implementations of [`MOI.copy_to`](@ref) on
constraint functions, attribute values and submittable values. If you define a
new attribute whose values `x::X` contain variable or constraint indices, you
must also implement this function.
"""
map_indices(f, ::MOI.AnyAttribute, x) = map_indices(f, x)

# RawOptimizerAttribute values are passed through un-changed.
map_indices(::Any, ::MOI.RawOptimizerAttribute, x) = x

"""
    map_indices(
        variable_map::AbstractDict{T,T},
        x::X,
    )::X where {T<:MOI.Index,X}

Shortcut for `map_indices(vi -> variable_map[vi], x)`.
"""
function map_indices(
    variable_map::AbstractDict{T,T},
    x::X,
)::X where {T<:MOI.Index,X}
    return map_indices(vi -> variable_map[vi], x)
end

const ObjectWithoutIndex = Union{
    Nothing,
    Type,
    Number,
    Enum,
    AbstractString,
    MOI.AnyAttribute,
    MOI.AbstractSet,
    Function,
    MOI.ModelLike,
    Symbol,
}

const ObjectOrTupleWithoutIndex =
    Union{ObjectWithoutIndex,Tuple{Vararg{ObjectWithoutIndex}}}

const ObjectOrTupleOrArrayWithoutIndex = Union{
    ObjectOrTupleWithoutIndex,
    AbstractArray{<:ObjectOrTupleWithoutIndex},
    AbstractArray{<:AbstractArray{<:ObjectOrTupleWithoutIndex}},
}

map_indices(::F, x::ObjectOrTupleOrArrayWithoutIndex) where {F<:Function} = x

function map_indices(index_map::F, vi::MOI.VariableIndex) where {F<:Function}
    return index_map(vi)
end

function map_indices(index_map::F, ci::MOI.ConstraintIndex) where {F<:Function}
    return index_map(ci)
end

function map_indices(
    index_map::F,
    array::AbstractArray{<:MOI.Index},
) where {F<:Function}
    return map(index_map, array)
end

map_indices(::F, block::MOI.NLPBlockData) where {F<:Function} = block

# Terms

function map_indices(index_map::F, t::MOI.ScalarAffineTerm) where {F<:Function}
    return MOI.ScalarAffineTerm(t.coefficient, index_map(t.variable))
end

function map_indices(index_map::F, t::MOI.VectorAffineTerm) where {F<:Function}
    return MOI.VectorAffineTerm(
        t.output_index,
        map_indices(index_map, t.scalar_term),
    )
end

function map_indices(
    index_map::F,
    t::MOI.ScalarQuadraticTerm,
) where {F<:Function}
    return MOI.ScalarQuadraticTerm(
        t.coefficient,
        index_map(t.variable_1),
        index_map(t.variable_2),
    )
end

function map_indices(
    index_map::F,
    t::MOI.VectorQuadraticTerm,
) where {F<:Function}
    return MOI.VectorQuadraticTerm(
        t.output_index,
        map_indices(index_map, t.scalar_term),
    )
end

# Functions

function map_indices(index_map::F, f::MOI.VectorOfVariables) where {F<:Function}
    return MOI.VectorOfVariables(index_map.(f.variables))
end

function map_indices(
    index_map::F,
    f::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
) where {F<:Function}
    return typeof(f)(map_indices.(index_map, f.terms), MOI.constant(f))
end

function map_indices(
    index_map::F,
    f::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
) where {F<:Function}
    affine_terms = map_indices.(index_map, f.affine_terms)
    quadratic_terms = map_indices.(index_map, f.quadratic_terms)
    return typeof(f)(quadratic_terms, affine_terms, MOI.constant(f))
end

# Function changes

function map_indices(
    ::F,
    change::Union{MOI.ScalarConstantChange,MOI.VectorConstantChange},
) where {F<:Function}
    return change
end

function map_indices(
    index_map::F,
    change::MOI.ScalarCoefficientChange,
) where {F<:Function}
    return MOI.ScalarCoefficientChange(
        index_map(change.variable),
        change.new_coefficient,
    )
end

function map_indices(
    index_map::F,
    change::MOI.MultirowChange,
) where {F<:Function}
    return MOI.MultirowChange(
        index_map(change.variable),
        change.new_coefficients,
    )
end

# For performance reason, we assume that the type of the function does not
# change in `substitute_variables`.
"""
    substitute_variables(variable_map::Function, x)

Substitute any [`MOI.VariableIndex`](@ref) in `x` by `variable_map(x)`. The
`variable_map` function returns either [`MOI.VariableIndex`](@ref) or
[`MOI.ScalarAffineFunction`](@ref), see [`eval_variables`](@ref) for a similar
function where `variable_map` returns a number.

This function is used by bridge optimizers on constraint functions, attribute
values and submittable values when at least one variable bridge is used hence it
needs to be implemented for custom types that are meant to be used as attribute
or submittable value.

WARNING: Don't use `substitude_variables(::Function, ...)` because Julia will
not specialize on this. Use instead
`substitude_variables(::F, ...) where {F<:Function}`.
"""
function substitute_variables end

function substitute_variables(
    ::F,
    x::ObjectOrTupleOrArrayWithoutIndex,
) where {F<:Function}
    return x
end

function substitute_variables(::F, block::MOI.NLPBlockData) where {F<:Function}
    return block
end

# Used when submitting `HeuristicSolution`.
function substitute_variables(
    variable_map::F,
    vis::Vector{MOI.VariableIndex},
) where {F<:Function}
    return substitute_variables.(variable_map, vis)
end

function substitute_variables(
    variable_map::F,
    vi::MOI.VariableIndex,
) where {F<:Function}
    func = variable_map(vi)
    if func != vi
        error("Cannot substitute `$vi` as it is bridged into `$func`.")
    end
    return vi
end

function substitute_variables(
    variable_map::F,
    term::MOI.ScalarQuadraticTerm{T},
) where {T,F<:Function}
    # We could have `T = Complex{Float64}` and `variable_map(term.variable)`
    # be a `MOI.ScalarAffineFunction{Float64}` with the Hermitian to PSD bridge.
    # We convert to `MOI.ScalarAffineFunction{T}` to avoid any issue.
    f1::MOI.ScalarAffineFunction{T} = variable_map(term.variable_1)
    f2::MOI.ScalarAffineFunction{T} = variable_map(term.variable_2)
    f12 = operate(*, T, f1, f2)::MOI.ScalarQuadraticFunction{T}
    coef = term.coefficient
    # The quadratic terms are evaluated as x'Qx/2 so a diagonal term should
    # be divided by 2 while an off-diagonal term appears twice in the matrix
    # and is divided by 2 so it stays the same.
    if term.variable_1 == term.variable_2
        coef /= 2
    end
    return operate!(*, T, f12, coef)
end

function substitute_variables(
    variable_map::F,
    term::MOI.ScalarAffineTerm{T},
) where {T,F<:Function}
    # See comment for `term::MOI.ScalarQuadraticTerm` for the conversion.
    func::MOI.ScalarAffineFunction{T} = variable_map(term.variable)
    return operate(*, T, term.coefficient, func)::MOI.ScalarAffineFunction{T}
end

function substitute_variables(
    variable_map::F,
    func::MOI.ScalarAffineFunction{T},
) where {T,F<:Function}
    g = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], MOI.constant(func))
    for term in func.terms
        new_term = substitute_variables(variable_map, term)
        operate!(+, T, g, new_term)
    end
    return g::MOI.ScalarAffineFunction{T}
end

function substitute_variables(
    variable_map::F,
    func::MOI.VectorAffineFunction{T},
) where {T,F<:Function}
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm{T}[],
        copy(MOI.constant(func)),
    )
    for term in func.terms
        sub = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, sub)
    end
    return g::MOI.VectorAffineFunction{T}
end

function substitute_variables(
    variable_map::F,
    func::MOI.ScalarQuadraticFunction{T},
) where {T,F<:Function}
    g = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{T}[],
        MOI.ScalarAffineTerm{T}[],
        MOI.constant(func),
    )
    for a_term in func.affine_terms
        new_a_term = substitute_variables(variable_map, a_term)
        operate!(+, T, g, new_a_term)
    end
    for q_term in func.quadratic_terms
        new_q_term = substitute_variables(variable_map, q_term)
        operate!(+, T, g, new_q_term)
    end
    return g
end

function substitute_variables(
    variable_map::F,
    func::MOI.VectorQuadraticFunction{T},
) where {T,F<:Function}
    g = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm{T}[],
        MOI.VectorAffineTerm{T}[],
        copy(MOI.constant(func)),
    )
    for term in func.affine_terms
        sub = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, sub)
    end
    for term in func.quadratic_terms
        sub = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, sub)
    end
    return g::MOI.VectorQuadraticFunction{T}
end

# Vector of constants

function constant_vector(
    f::Union{MOI.ScalarAffineFunction,MOI.ScalarQuadraticFunction},
)
    return [f.constant]
end

function constant_vector(
    f::Union{MOI.VectorAffineFunction,MOI.VectorQuadraticFunction},
)
    return f.constants
end

# Implements iterator interface
"""
    scalar_type(F::Type{<:MOI.AbstractVectorFunction})

Type of functions obtained by indexing objects obtained by calling `eachscalar`
on functions of type `F`.
"""
scalar_type(::Type{MOI.VectorOfVariables}) = MOI.VariableIndex

function scalar_type(::Type{MOI.VectorAffineFunction{T}}) where {T}
    return MOI.ScalarAffineFunction{T}
end

function scalar_type(::Type{MOI.VectorQuadraticFunction{T}}) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

"""
    ScalarFunctionIterator{F<:MOI.AbstractVectorFunction}

A type that allows iterating over the scalar-functions that comprise an
`AbstractVectorFunction`.
"""
struct ScalarFunctionIterator{F<:MOI.AbstractVectorFunction,C}
    f::F
    # Cache that can be used to store a precomputed datastructure that allows
    # an efficient implementation of `getindex`.
    cache::C
end

function ScalarFunctionIterator(func::MOI.AbstractVectorFunction)
    return ScalarFunctionIterator(func, scalar_iterator_cache(func))
end

scalar_iterator_cache(func::MOI.AbstractVectorFunction) = nothing

function output_index_iterator(terms::AbstractVector, output_dimension)
    start = zeros(Int, output_dimension)
    next = Vector{Int}(undef, length(terms))
    last = zeros(Int, output_dimension)
    for i in eachindex(terms)
        j = terms[i].output_index
        if iszero(last[j])
            start[j] = i
        else
            next[last[j]] = i
        end
        last[j] = i
    end
    for j in eachindex(last)
        if !iszero(last[j])
            next[last[j]] = 0
        end
    end
    return ChainedIterator(start, next)
end
struct ChainedIterator
    start::Vector{Int}
    next::Vector{Int}
end
struct ChainedIteratorAtIndex
    start::Int
    next::Vector{Int}
end

function ChainedIteratorAtIndex(it::ChainedIterator, index::Int)
    return ChainedIteratorAtIndex(it.start[index], it.next)
end

#TODO We could also precompute the length for each `output_index`,
# check that it's a win.
Base.IteratorSize(::ChainedIteratorAtIndex) = Base.SizeUnknown()

function Base.iterate(it::ChainedIteratorAtIndex, i = it.start)
    if iszero(i)
        return nothing
    end
    return i, it.next[i]
end

function ScalarFunctionIterator(f::MOI.VectorAffineFunction)
    return ScalarFunctionIterator(
        f,
        output_index_iterator(f.terms, MOI.output_dimension(f)),
    )
end

function ScalarFunctionIterator(f::MOI.VectorQuadraticFunction)
    return ScalarFunctionIterator(
        f,
        (
            output_index_iterator(f.quadratic_terms, MOI.output_dimension(f)),
            output_index_iterator(f.affine_terms, MOI.output_dimension(f)),
        ),
    )
end

"""
    eachscalar(f::MOI.AbstractVectorFunction)

Returns an iterator for the scalar components of the vector function.

See also [`scalarize`](@ref).
"""
eachscalar(f::MOI.AbstractVectorFunction) = ScalarFunctionIterator(f)

"""
    eachscalar(f::MOI.AbstractVector)

Returns an iterator for the scalar components of the vector.
"""
eachscalar(f::AbstractVector) = f

function Base.iterate(it::ScalarFunctionIterator, state = 1)
    if state > length(it)
        return nothing
    end
    return (it[state], state + 1)
end

function Base.length(it::ScalarFunctionIterator{<:MOI.AbstractVectorFunction})
    return MOI.output_dimension(it.f)
end

function Base.eltype(::ScalarFunctionIterator{MOI.VectorOfVariables})
    return MOI.VariableIndex
end

function Base.eltype(
    ::ScalarFunctionIterator{MOI.VectorAffineFunction{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function Base.eltype(
    ::ScalarFunctionIterator{MOI.VectorQuadraticFunction{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

Base.lastindex(it::ScalarFunctionIterator) = length(it)

# Define getindex for Vector functions

# VectorOfVariables

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorOfVariables},
    output_index::Integer,
)
    return it.f.variables[output_index]
end

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorOfVariables},
    output_indices::AbstractVector{<:Integer},
)
    return MOI.VectorOfVariables(it.f.variables[output_indices])
end

# VectorAffineFunction

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}},
    output_index::Integer,
) where {T}
    return MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}[
            it.f.terms[i].scalar_term for
            i in ChainedIteratorAtIndex(it.cache, output_index)
        ],
        it.f.constants[output_index],
    )
end

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}},
    output_indices::AbstractVector{<:Integer},
) where {T}
    terms = MOI.VectorAffineTerm{T}[]
    for (i, output_index) in enumerate(output_indices)
        for j in ChainedIteratorAtIndex(it.cache, output_index)
            push!(terms, MOI.VectorAffineTerm(i, it.f.terms[j].scalar_term))
        end
    end
    return MOI.VectorAffineFunction(terms, it.f.constants[output_indices])
end

# VectorQuadraticFunction

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorQuadraticFunction{T}},
    output_index::Integer,
) where {T}
    return MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{T}[
            it.f.quadratic_terms[i].scalar_term for
            i in ChainedIteratorAtIndex(it.cache[1], output_index)
        ],
        MOI.ScalarAffineTerm{T}[
            it.f.affine_terms[i].scalar_term for
            i in ChainedIteratorAtIndex(it.cache[2], output_index)
        ],
        it.f.constants[output_index],
    )
end

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorQuadraticFunction{T}},
    output_indices::AbstractVector{<:Integer},
) where {T}
    vat = MOI.VectorAffineTerm{T}[]
    vqt = MOI.VectorQuadraticTerm{T}[]
    for (i, output_index) in enumerate(output_indices)
        for j in ChainedIteratorAtIndex(it.cache[2], output_index)
            push!(
                vat,
                MOI.VectorAffineTerm(i, it.f.affine_terms[j].scalar_term),
            )
        end
        for j in ChainedIteratorAtIndex(it.cache[1], output_index)
            push!(
                vqt,
                MOI.VectorQuadraticTerm(i, it.f.quadratic_terms[j].scalar_term),
            )
        end
    end
    return MOI.VectorQuadraticFunction(vqt, vat, it.f.constants[output_indices])
end

"""
    zero_with_output_dimension(::Type{T}, output_dimension::Integer) where {T}

Create an instance of type `T` with the output dimension `output_dimension`.

This is mostly useful in Bridges, when code needs to be agnostic to the type of
vector-valued function that is passed in.
"""
function zero_with_output_dimension end

function zero_with_output_dimension(::Type{Vector{T}}, n::Integer) where {T}
    return zeros(T, n)
end

function zero_with_output_dimension(
    ::Type{MOI.VectorAffineFunction{T}},
    n::Integer,
) where {T}
    return MOI.VectorAffineFunction{T}(MOI.VectorAffineTerm{T}[], zeros(T, n))
end

function zero_with_output_dimension(
    ::Type{MOI.VectorQuadraticFunction{T}},
    n::Integer,
) where {T}
    return MOI.VectorQuadraticFunction{T}(
        MOI.VectorQuadraticTerm{T}[],
        MOI.VectorAffineTerm{T}[],
        zeros(T, n),
    )
end

"""
    unsafe_add(t1::MOI.ScalarAffineTerm, t2::MOI.ScalarAffineTerm)

Sums the coefficients of `t1` and `t2` and returns an output
`MOI.ScalarAffineTerm`. It is unsafe because it uses the `variable` of `t1` as
the `variable` of the output without checking that it is equal to that of `t2`.
"""
function unsafe_add(t1::MOI.ScalarAffineTerm, t2::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(t1.coefficient + t2.coefficient, t1.variable)
end

"""
    unsafe_add(t1::MOI.ScalarQuadraticTerm, t2::MOI.ScalarQuadraticTerm)

Sums the coefficients of `t1` and `t2` and returns an output
`MOI.ScalarQuadraticTerm`. It is unsafe because it uses the `variable`'s
of `t1` as the `variable`'s of the output without checking that they are
the same (up to permutation) to those of `t2`.
"""
function unsafe_add(t1::MOI.ScalarQuadraticTerm, t2::MOI.ScalarQuadraticTerm)
    return MOI.ScalarQuadraticTerm(
        t1.coefficient + t2.coefficient,
        t1.variable_1,
        t1.variable_2,
    )
end

"""
    unsafe_add(t1::MOI.VectorAffineTerm, t2::MOI.VectorAffineTerm)

Sums the coefficients of `t1` and `t2` and returns an output
`MOI.VectorAffineTerm`. It is unsafe because it uses the `output_index` and
`variable` of `t1` as the `output_index` and `variable` of the output term
without checking that they are equal to those of `t2`.
"""
function unsafe_add(
    t1::T,
    t2::T,
) where {T<:Union{MOI.VectorAffineTerm,MOI.VectorQuadraticTerm}}
    scalar_term = unsafe_add(t1.scalar_term, t2.scalar_term)
    return T(t1.output_index, scalar_term)
end

is_canonical(::MOI.AbstractFunction) = false

is_canonical(::Union{MOI.VariableIndex,MOI.VectorOfVariables}) = true

"""
    is_canonical(f::Union{ScalarAffineFunction, VectorAffineFunction})

Returns a Bool indicating whether the function is in canonical form.
See [`canonical`](@ref).
"""
function is_canonical(
    f::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    return _is_strictly_sorted(f.terms)
end

"""
    is_canonical(f::Union{ScalarQuadraticFunction, VectorQuadraticFunction})

Returns a Bool indicating whether the function is in canonical form.
See [`canonical`](@ref).
"""
function is_canonical(
    f::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
)
    return _is_strictly_sorted(f.affine_terms) &&
           _is_strictly_sorted(f.quadratic_terms)
end

function _is_strictly_sorted(x::Vector)
    if isempty(x)
        return true
    end
    @inbounds current_x = x[1]
    if iszero(MOI.coefficient(current_x))
        return false
    end
    current_fx = MOI.term_indices(current_x)
    @inbounds for i in 2:length(x)
        next_x = x[i]
        if iszero(MOI.coefficient(next_x))
            return false
        end
        next_fx = MOI.term_indices(next_x)
        if next_fx <= current_fx
            return false
        end
        current_x, current_fx = next_x, next_fx
    end
    return true
end

"""
    canonical(
        f::Union{
            ScalarAffineFunction,
            VectorAffineFunction,
            ScalarQuadraticFunction,
            VectorQuadraticFunction,
        },
    )

Returns the function in a canonical form, i.e.
 * A term appear only once.
 * The coefficients are nonzero.
 * The terms appear in increasing order of variable where there the order of the
   variables is the order of their value.
 * For a `AbstractVectorFunction`, the terms are sorted in ascending order of
   output index.

The output of `canonical` can be assumed to be a copy of `f`, even for
`VectorOfVariables`.

## Examples

If `x` (resp. `y`, `z`) is `VariableIndex(1)` (resp. 2, 3). The canonical
representation of `ScalarAffineFunction([y, x, z, x, z], [2, 1, 3, -2, -3], 5)`
is `ScalarAffineFunction([x, y], [-1, 2], 5)`.
"""
function canonical(f::MOI.AbstractFunction)
    g = copy(f)
    if !is_canonical(g)
        canonicalize!(g)
    end
    return g
end

canonicalize!(f::Union{MOI.VectorOfVariables,MOI.VariableIndex}) = f

"""
    canonicalize!(f::Union{ScalarAffineFunction, VectorAffineFunction})

Convert a function to canonical form in-place, without allocating a copy to hold
the result. See [`canonical`](@ref).
"""
function canonicalize!(
    f::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    _sort_and_compress!(f.terms)
    return f
end

"""
    canonicalize!(f::Union{ScalarQuadraticFunction, VectorQuadraticFunction})

Convert a function to canonical form in-place, without allocating a copy to hold
the result. See [`canonical`](@ref).
"""
function canonicalize!(
    f::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
)
    _sort_and_compress!(f.affine_terms)
    _sort_and_compress!(f.quadratic_terms)
    return f
end

"""
    _sort_and_compress!(x::Vector)

Sort the vector `x` in-place using `by` as the function from elements to
comparable keys, then combine all entries for which `by(x[i]) == by(x[j])` using
the function `x[i] = combine(x[i], x[j])`, and remove any entries for which
`keep(x[i]) == false`. This may result in `x` being resized to a shorter length.
"""
function _sort_and_compress!(x::Vector)
    if length(x) == 0
        return
    end
    sort!(x, QuickSort, Base.Order.ord(isless, MOI.term_indices, false))
    i = 1
    @inbounds for j in 2:length(x)
        if MOI.term_indices(x[i]) == MOI.term_indices(x[j])
            x[i] = unsafe_add(x[i], x[j])
        elseif iszero(MOI.coefficient(x[i]))
            x[i] = x[j]
        else
            x[i+1] = x[j]
            i += 1
        end
    end
    if iszero(MOI.coefficient(x[i]))
        i -= 1
    end
    resize!(x, i)
    return
end

"""
    all_coefficients(p::Function, f::MOI.AbstractFunction)

Determine whether predicate `p` returns `true` for all coefficients of `f`,
returning `false` as soon as the first coefficient of `f` for which `p`
returns `false` is encountered (short-circuiting). Similar to `all`.
"""
function all_coefficients end

function all_coefficients(p::Function, f::MOI.ScalarAffineFunction)
    return p(f.constant) && all(t -> p(MOI.coefficient(t)), f.terms)
end

function all_coefficients(p::Function, f::MOI.ScalarQuadraticFunction)
    return p(f.constant) &&
           all(t -> p(MOI.coefficient(t)), f.affine_terms) &&
           all(t -> p(MOI.coefficient(t)), f.quadratic_terms)
end

"""
    isapprox_zero(f::MOI.AbstractFunction, tol)

Return a `Bool` indicating whether the function `f` is approximately zero using
`tol` as a tolerance.

## Important note

This function assumes that `f` does not contain any duplicate terms, you might
want to first call [`canonical`](@ref) if that is not guaranteed.
For instance, given
```julia
f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, -1], [x, x]), 0)`.
```
then `isapprox_zero(f)` is `false` but `isapprox_zero(MOIU.canonical(f))` is
`true`.
"""
function isapprox_zero end

isapprox_zero(α::AbstractFloat, tol) = -tol <= α <= tol

isapprox_zero(α::Union{Integer,Rational}, tol) = iszero(α)

function isapprox_zero(f::MOI.AbstractFunction, tol)
    return all_coefficients(α -> isapprox_zero(α, tol), f)
end

_is_constant(f::MOI.ScalarAffineFunction) = isempty(f.terms)

function _is_constant(f::MOI.ScalarQuadraticFunction)
    return isempty(f.affine_terms) && isempty(f.quadratic_terms)
end

Base.iszero(::MOI.VariableIndex) = false

function Base.iszero(
    f::Union{MOI.ScalarAffineFunction,MOI.ScalarQuadraticFunction},
)
    return iszero(MOI.constant(f)) && _is_constant(canonical(f))
end

Base.isone(::MOI.VariableIndex) = false

function Base.isone(
    f::Union{MOI.ScalarAffineFunction,MOI.ScalarQuadraticFunction},
)
    return isone(MOI.constant(f)) && _is_constant(canonical(f))
end

_keep_all(keep::Function, v::MOI.VariableIndex) = keep(v)

function _keep_all(keep::Function, t::MOI.ScalarAffineTerm)
    return keep(t.variable)
end

function _keep_all(keep::Function, t::MOI.ScalarQuadraticTerm)
    return keep(t.variable_1) && keep(t.variable_2)
end

function _keep_all(
    keep::Function,
    t::Union{MOI.VectorAffineTerm,MOI.VectorQuadraticTerm},
)
    return _keep_all(keep, t.scalar_term)
end

# Removes terms or variables in `vis_or_terms` that contains the variable of index `vi`
function _filter_variables(keep::Function, variables_or_terms::Vector)
    return filter(el -> _keep_all(keep, el), variables_or_terms)
end

"""
    filter_variables(keep::Function, f::AbstractFunction)

Return a new function `f` with the variable `vi` such that `!keep(vi)` removed.

WARNING: Don't define `filter_variables(::Function, ...)` because Julia will
not specialize on this. Define instead
`filter_variables(::F, ...) where {F<:Function}`.
"""
function filter_variables end

function filter_variables(keep::Function, f::MOI.VariableIndex)
    if !keep(f)
        error(
            "Cannot remove variable from a `VariableIndex` function of the",
            " same variable.",
        )
    end
    return f
end

function filter_variables(keep::Function, f::MOI.VectorOfVariables)
    return MOI.VectorOfVariables(_filter_variables(keep, f.variables))
end

function filter_variables(
    keep::Function,
    f::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    return typeof(f)(_filter_variables(keep, f.terms), MOI.constant(f))
end

function filter_variables(
    keep::Function,
    f::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
)
    return typeof(f)(
        _filter_variables(keep, f.quadratic_terms),
        _filter_variables(keep, f.affine_terms),
        MOI.constant(f),
    )
end

"""
    remove_variable(f::AbstractFunction, vi::VariableIndex)

Return a new function `f` with the variable vi removed.
"""
function remove_variable(f::MOI.AbstractFunction, vi::MOI.VariableIndex)
    return filter_variables(v -> v != vi, f)
end

function remove_variable(
    f::MOI.AbstractFunction,
    vis::Vector{MOI.VariableIndex},
)
    # Create a `Set` to test membership in `vis` in O(1).
    set = Set(vis)
    return filter_variables(vi -> !(vi in set), f)
end

"""
    modify_function(f::AbstractFunction, change::AbstractFunctionModification)

Return a new function `f` modified according to `change`.
"""
function modify_function(
    f::MOI.ScalarAffineFunction,
    change::MOI.ScalarConstantChange,
)
    return MOI.ScalarAffineFunction(f.terms, change.new_constant)
end

function modify_function(
    f::MOI.VectorAffineFunction,
    change::MOI.VectorConstantChange,
)
    return MOI.VectorAffineFunction(f.terms, change.new_constant)
end

function modify_function(
    f::MOI.ScalarQuadraticFunction,
    change::MOI.ScalarConstantChange,
)
    return MOI.ScalarQuadraticFunction(
        f.quadratic_terms,
        f.affine_terms,
        change.new_constant,
    )
end
function modify_function(
    f::MOI.VectorQuadraticFunction,
    change::MOI.VectorConstantChange,
)
    return MOI.VectorQuadraticFunction(
        f.quadratic_terms,
        f.affine_terms,
        change.new_constant,
    )
end

function _modifycoefficient(
    terms::Vector{MOI.ScalarAffineTerm{T}},
    variable::MOI.VariableIndex,
    new_coefficient::T,
) where {T}
    terms = copy(terms)
    i = something(findfirst(t -> t.variable == variable, terms), 0)
    if iszero(i)  # The variable was not already in the function
        if !iszero(new_coefficient)
            push!(terms, MOI.ScalarAffineTerm(new_coefficient, variable))
        end
    else  # The variable was already in the function
        if iszero(new_coefficient)
            deleteat!(terms, i)
        else
            terms[i] = MOI.ScalarAffineTerm(new_coefficient, variable)
        end
        # To account for duplicates, we need to delete any other instances of
        # `variable` in `terms`.
        for j in length(terms):-1:(i+1)
            if terms[j].variable == variable
                deleteat!(terms, j)
            end
        end
    end
    return terms
end

function modify_function(
    f::MOI.ScalarAffineFunction{T},
    change::MOI.ScalarCoefficientChange{T},
) where {T}
    terms = _modifycoefficient(f.terms, change.variable, change.new_coefficient)
    return MOI.ScalarAffineFunction(terms, f.constant)
end

function modify_function(
    f::MOI.ScalarQuadraticFunction{T},
    change::MOI.ScalarCoefficientChange{T},
) where {T}
    terms = _modifycoefficient(
        f.affine_terms,
        change.variable,
        change.new_coefficient,
    )
    return MOI.ScalarQuadraticFunction(f.quadratic_terms, terms, f.constant)
end

function _modifycoefficients(
    terms::Vector{MOI.VectorAffineTerm{T}},
    variable::MOI.VariableIndex,
    new_coefficients::Vector{Tuple{Int64,T}},
) where {T}
    terms = copy(terms)
    coef_dict = Dict(k => v for (k, v) in new_coefficients)
    elements_to_delete = Int[]
    for (i, term) in enumerate(terms)
        if term.scalar_term.variable != variable
            continue
        end
        new_coef = Base.get(coef_dict, term.output_index, nothing)
        if new_coef === nothing
            continue
        elseif iszero(new_coef)
            push!(elements_to_delete, i)
        else
            terms[i] = MOI.VectorAffineTerm(
                term.output_index,
                MOI.ScalarAffineTerm(new_coef, variable),
            )
            # Set the coefficient to 0.0 so we don't add duplicates.
            coef_dict[term.output_index] = zero(T)
        end
    end
    deleteat!(terms, elements_to_delete)
    # Add elements that were not previously in `terms`.
    for (k, v) in coef_dict
        if iszero(v)
            continue
        end
        push!(terms, MOI.VectorAffineTerm(k, MOI.ScalarAffineTerm(v, variable)))
    end
    return terms
end

function modify_function(
    f::MOI.VectorAffineFunction{T},
    change::MOI.MultirowChange{T},
) where {T}
    terms =
        _modifycoefficients(f.terms, change.variable, change.new_coefficients)
    return MOI.VectorAffineFunction(terms, f.constants)
end

function modify_function(
    f::MOI.VectorQuadraticFunction{T},
    change::MOI.MultirowChange{T},
) where {T}
    terms = _modifycoefficients(
        f.affine_terms,
        change.variable,
        change.new_coefficients,
    )
    return MOI.VectorQuadraticFunction(f.quadratic_terms, terms, f.constants)
end

# Arithmetic

"""
    operate(
        op::Function,
        ::Type{T},
        args::Union{T,MOI.AbstractFunction}...,
    )::MOI.AbstractFunction where {T}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`. No argument can be
modified.
"""
function operate end

# Without `<:Number`, Julia v1.1.1 fails at precompilation with a StackOverflowError.
function operate(
    op::Function,
    ::Type{T},
    α::Union{T,AbstractVector{T}}...,
) where {T<:Number}
    return op(α...)
end

"""
    operate!(
        op::Function,
        ::Type{T},
        args::Union{T, MOI.AbstractFunction}...,
    )::MOI.AbstractFunction where {T}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`. The first argument
can be modified. The return type is the same than the method
`operate(op, T, args...)` without `!`.
"""
function operate! end

function operate!(
    op::Function,
    ::Type{T},
    α::Union{T,AbstractVector{T}}...,
) where {T}
    return op(α...)
end

"""
    operate_output_index!(
        op::Function,
        ::Type{T},
        output_index::Integer,
        func::MOI.AbstractVectorFunction
        args::Union{T, MOI.AbstractScalarFunction}...
    )::MOI.AbstractFunction where {T}

Returns an `MOI.AbstractVectorFunction` where the function at `output_index`
is the result of the operation `op` applied to the function at `output_index`
of `func` and `args`. The functions at output index different to `output_index`
are the same as the functions at the same output index in `func`. The first
argument can be modified.
"""
function operate_output_index! end

function operate_output_index!(
    op::Function,
    ::Type{T},
    i::Integer,
    x::Vector{T},
    args...,
) where {T}
    return x[i] = operate!(op, T, x[i], args...)
end

"""
    promote_operation(
        op::Function,
        ::Type{T},
        ArgsTypes::Type{<:Union{T, MOI.AbstractFunction}}...,
    ) where {T}

Returns the type of the `MOI.AbstractFunction` returned to the call
`operate(op, T, args...)` where the types of the arguments `args` are
`ArgsTypes`.
"""
function promote_operation end

# Helpers

function operate_term(
    ::typeof(+),
    term::Union{
        MOI.ScalarAffineTerm,
        MOI.ScalarQuadraticTerm,
        MOI.VectorAffineTerm,
        MOI.VectorQuadraticTerm,
    },
)
    return term
end

function operate_term(::typeof(-), term::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(-term.coefficient, term.variable)
end

function operate_term(::typeof(-), term::MOI.ScalarQuadraticTerm)
    return MOI.ScalarQuadraticTerm(
        -term.coefficient,
        term.variable_1,
        term.variable_2,
    )
end

function operate_term(::typeof(-), term::MOI.VectorAffineTerm)
    return MOI.VectorAffineTerm(
        term.output_index,
        operate_term(-, term.scalar_term),
    )
end

function operate_term(::typeof(-), term::MOI.VectorQuadraticTerm)
    return MOI.VectorQuadraticTerm(
        term.output_index,
        operate_term(-, term.scalar_term),
    )
end

function operate_term(::typeof(*), α::T, t::MOI.ScalarAffineTerm{T}) where {T}
    return MOI.ScalarAffineTerm(α * t.coefficient, t.variable)
end

# `<:Number` is a workaround for https://github.com/jump-dev/MathOptInterface.jl/issues/980
function operate_term(
    ::typeof(*),
    t::MOI.ScalarAffineTerm{T},
    β::T,
) where {T<:Number}
    return MOI.ScalarAffineTerm(t.coefficient * β, t.variable)
end

function operate_term(
    ::typeof(*),
    α::T,
    t::MOI.ScalarAffineTerm{T},
    β::T,
) where {T}
    return MOI.ScalarAffineTerm(α * t.coefficient * β, t.variable)
end

function operate_term(
    ::typeof(*),
    α::T,
    t::MOI.ScalarQuadraticTerm{T},
) where {T}
    return MOI.ScalarQuadraticTerm(
        α * t.coefficient,
        t.variable_1,
        t.variable_2,
    )
end

function operate_term(
    ::typeof(*),
    t::MOI.ScalarQuadraticTerm{T},
    β::T,
) where {T}
    return MOI.ScalarQuadraticTerm(
        t.coefficient * β,
        t.variable_1,
        t.variable_2,
    )
end

function operate_term(
    ::typeof(*),
    α::T,
    t::MOI.ScalarQuadraticTerm{T},
    β::T,
) where {T}
    return MOI.ScalarQuadraticTerm(
        α * t.coefficient * β,
        t.variable_1,
        t.variable_2,
    )
end

function operate_term(
    ::typeof(*),
    t1::MOI.ScalarAffineTerm,
    t2::MOI.ScalarAffineTerm,
)
    coef = t1.coefficient * t2.coefficient
    if t1.variable == t2.variable
        coef *= 2
    end
    return MOI.ScalarQuadraticTerm(coef, t1.variable, t2.variable)
end

function operate_term(::typeof(*), α::T, t::MOI.VectorAffineTerm{T}) where {T}
    return MOI.VectorAffineTerm(
        t.output_index,
        operate_term(*, α, t.scalar_term),
    )
end

function operate_term(::typeof(*), t::MOI.VectorAffineTerm{T}, α::T) where {T}
    return MOI.VectorAffineTerm(
        t.output_index,
        operate_term(*, t.scalar_term, α),
    )
end

function operate_term(
    ::typeof(*),
    α::T,
    t::MOI.VectorQuadraticTerm{T},
) where {T}
    return MOI.VectorQuadraticTerm(
        t.output_index,
        operate_term(*, α, t.scalar_term),
    )
end

function operate_term(
    ::typeof(*),
    t::MOI.VectorQuadraticTerm{T},
    α::T,
) where {T}
    return MOI.VectorQuadraticTerm(
        t.output_index,
        operate_term(*, t.scalar_term, α),
    )
end

function operate_term(
    ::typeof(*),
    t1::MOI.VectorAffineTerm,
    t2::MOI.VectorAffineTerm,
)
    @assert t1.output_index == t2.output_index
    return MOI.VectorQuadraticTerm(
        t1.output_index,
        operate_term(*, t1.scalar_term, t2.scalar_term),
    )
end

function operate_term(::typeof(/), t::MOI.ScalarAffineTerm{T}, α::T) where {T}
    return MOI.ScalarAffineTerm(t.coefficient / α, t.variable)
end

function operate_term(
    ::typeof(/),
    t::MOI.ScalarQuadraticTerm{T},
    α::T,
) where {T}
    return MOI.ScalarQuadraticTerm(
        t.coefficient / α,
        t.variable_1,
        t.variable_2,
    )
end

function operate_term(::typeof(/), t::MOI.VectorAffineTerm{T}, α::T) where {T}
    return MOI.VectorAffineTerm(
        t.output_index,
        operate_term(/, t.scalar_term, α),
    )
end

function operate_term(
    ::typeof(/),
    t::MOI.VectorQuadraticTerm{T},
    α::T,
) where {T}
    return MOI.VectorQuadraticTerm(
        t.output_index,
        operate_term(/, t.scalar_term, α),
    )
end

# Avoid a copy in the case of +
function operate_terms(
    ::typeof(+),
    terms::Vector{
        <:Union{
            MOI.ScalarAffineTerm,
            MOI.ScalarQuadraticTerm,
            MOI.VectorAffineTerm,
            MOI.VectorQuadraticTerm,
        },
    },
)
    return terms
end

function operate_terms!(
    ::typeof(-),
    terms::Vector{<:Union{MOI.ScalarAffineTerm,MOI.ScalarQuadraticTerm}},
)
    return map!(term -> operate_term(-, term), terms, terms)
end

function operate_terms(
    ::typeof(-),
    terms::Vector{
        <:Union{
            MOI.ScalarAffineTerm,
            MOI.ScalarQuadraticTerm,
            MOI.VectorAffineTerm,
            MOI.VectorQuadraticTerm,
        },
    },
)
    return map(term -> operate_term(-, term), terms)
end

function operate_terms(
    ::typeof(-),
    terms::Vector{<:Union{MOI.VectorAffineTerm,MOI.VectorQuadraticTerm}},
)
    return map(term -> operate_term(-, term), terms)
end

function map_terms!(
    op,
    func::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    return map!(op, func.terms, func.terms)
end

function map_terms!(
    op,
    func::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
)
    map!(op, func.affine_terms, func.affine_terms)
    return map!(op, func.quadratic_terms, func.quadratic_terms)
end

# Functions convertible to a ScalarAffineFunction
const ScalarAffineLike{T} =
    Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}}
# Functions convertible to a ScalarQuadraticFunction
const ScalarQuadraticLike{T} =
    Union{ScalarAffineLike{T},MOI.ScalarQuadraticFunction{T}}

# `ScalarLike` for which `T` is defined to avoid defining, e.g.,
# `+(::VariableIndex, ::Any)` which should rather be
# `+(::VariableIndex, ::Number)`.
const TypedScalarLike{T} =
    Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}
# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading e.g. `+(::Float64, ::Float64)`
const ScalarLike{T} = Union{MOI.VariableIndex,TypedScalarLike{T}}

# Functions convertible to a VectorAffineFunction
const VectorAffineLike{T} =
    Union{Vector{T},MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}
# Functions convertible to a VectorQuadraticFunction
const VectorQuadraticLike{T} =
    Union{VectorAffineLike{T},MOI.VectorQuadraticFunction{T}}

# `VectorLike` for which `T` is defined to avoid defining, e.g.,
# `+(::VectorOfVariables, ::Any)` which should rather be
# `+(::VectorOfVariables, ::Number)`.
const TypedVectorLike{T} =
    Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}}
# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading e.g. `+(::Float64, ::Float64)`
const VectorLike{T} = Union{MOI.VectorOfVariables,TypedVectorLike{T}}

const TypedLike{T} = Union{TypedScalarLike{T},TypedVectorLike{T}}

###################################### +/- #####################################
## promote_operation

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{<:ScalarAffineLike{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:ScalarAffineLike{T}},
    ::Type{<:ScalarAffineLike{T}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{<:ScalarQuadraticLike{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:ScalarQuadraticLike{T}},
    ::Type{<:ScalarQuadraticLike{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

## operate!
# + with at least 3 arguments
function operate!(op::typeof(+), ::Type{T}, f, g, h, args...) where {T}
    return operate!(+, T, operate!(op, T, f, g), h, args...)
end

# Unary -
function operate!(
    op::typeof(-),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return MA.operate!(-, f)
end

# Scalar Affine +/-! ...
function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T}
    return MA.operate!(op, f, g)
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VariableIndex,
    g::ScalarQuadraticLike,
) where {T}
    return operate(op, T, f, g)
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    return operate(op, T, f, g)
end

# Scalar Quadratic +/-! ...
function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T}
    return MA.operate!(op, f, g)
end

## operate
# + with at least 3 arguments, can use in-place as the user cannot use
# intermediate results
function operate(::typeof(+), ::Type{T}, f, g, h, args...) where {T}
    return operate!(+, T, operate(+, T, f, g), h, args...)
end

# Unary +
function operate(::typeof(+), ::Type{T}, f::MOI.AbstractFunction) where {T}
    return f
end

# Scalar number +/- ...
function operate(op::typeof(+), ::Type{T}, α::T, f::ScalarLike{T}) where {T}
    return operate(op, T, f, α)
end

function operate(::typeof(-), ::Type{T}, α::T, f::ScalarLike{T}) where {T}
    return operate!(+, T, operate(-, T, f), α)
end

# Scalar Variable +/- ...
function operate(::typeof(-), ::Type{T}, f::MOI.VariableIndex) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(-one(T), f)],
        zero(T),
    )
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VariableIndex,
    α::T,
) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], op(α))
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(one(T), f), MOI.ScalarAffineTerm(op(one(T)), g)],
        zero(T),
    )
end

function operate(
    op::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    g::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return operate(op, T, g, f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    g::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

# Scalar Affine +/- ...
function operate(
    op::Union{typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    return MOI.ScalarAffineFunction(operate_terms(op, f.terms), op(f.constant))
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::ScalarAffineLike{T},
) where {T}
    return operate!(op, T, copy(f), g)
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    return MOI.ScalarQuadraticFunction(
        operate_terms(op, g.quadratic_terms),
        [f.terms; operate_terms(op, g.affine_terms)],
        op(f.constant, g.constant),
    )
end

# Scalar Quadratic +/- ...
function operate(
    op::Union{typeof(-)},
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
) where {T}
    return MOI.ScalarQuadraticFunction(
        operate_terms(op, f.quadratic_terms),
        operate_terms(op, f.affine_terms),
        op(f.constant),
    )
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::ScalarQuadraticLike{T},
) where {T}
    return operate!(op, T, copy(f), g)
end

# To avoid type piracy, we add at least one `ScalarLike` outside of the `...`.
function Base.:+(arg::ScalarLike{T}, args::ScalarLike{T}...) where {T}
    return operate(+, T, arg, args...)
end

function Base.:+(
    α::T,
    arg::TypedScalarLike{T},
    args::ScalarLike{T}...,
) where {T}
    return operate(+, T, α, arg, args...)
end

function Base.:+(α::Number, f::MOI.VariableIndex)
    return operate(+, typeof(α), α, f)
end

function Base.:+(f::TypedScalarLike{T}, α::T) where {T}
    return operate(+, T, f, α)
end

function Base.:+(f::MOI.VariableIndex, α::Number)
    return operate(+, typeof(α), f, α)
end

function Base.:-(arg::ScalarLike{T}, args::ScalarLike{T}...) where {T}
    return operate(-, T, arg, args...)
end

function Base.:-(f::TypedScalarLike{T}, α::T) where {T}
    return operate(-, T, f, α)
end

function Base.:-(f::MOI.VariableIndex, α::Number)
    return operate(-, typeof(α), f, α)
end

function Base.:-(α::T, f::TypedScalarLike{T}) where {T}
    return operate(-, T, α, f)
end

function Base.:-(α::Number, f::MOI.VariableIndex)
    return operate(-, typeof(α), α, f)
end

function Base.:+(::MOI.VariableIndex, ::MOI.VariableIndex...)
    return error(
        "Unable to add VariableIndex together because no coefficient type " *
        "is specified. Instead of `x + y`, convert one of the terms to a " *
        "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
        "`T` is the coefficient type For example: `1.0 * x + y`.",
    )
end

function Base.:-(::MOI.VariableIndex, ::MOI.VariableIndex...)
    return error(
        "Unable to subtract VariableIndex together because no coefficient " *
        "type is specified. Instead of `x - y`, convert one of the terms to a " *
        "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
        "`T` is the coefficient type For example: `1.0 * x - y`.",
    )
end

function Base.:*(
    ::MOI.VariableIndex,
    ::MOI.VariableIndex,
    ::MOI.VariableIndex...,
)
    return error(
        "Unable to multiply VariableIndex together because no coefficient " *
        "type is specified. Instead of `x * y`, convert one of the terms to a " *
        "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
        "`T` is the coefficient type For example: `1.0 * x * y`.",
    )
end

# Vector +/-
###############################################################################

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{<:VectorAffineLike{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{<:VectorQuadraticLike{T}},
) where {T}
    return MOI.VectorQuadraticFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:VectorAffineLike{T}},
    ::Type{<:VectorAffineLike{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:VectorQuadraticLike{T}},
    ::Type{<:VectorQuadraticLike{T}},
) where {T}
    return MOI.VectorQuadraticFunction{T}
end

# Vector Variable +/- ...
function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::VectorQuadraticLike,
) where {T}
    return operate(op, T, f, g)
end

# Vector Affine +/-! ...
function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    α::T,
) where {T}
    f.constants[output_index] = op(f.constants[output_index], α)
    return f
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::Vector{T},
) where {T}
    @assert MOI.output_dimension(f) == length(g)
    f.constants .= op.(f.constants, g)
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    g::MOI.VariableIndex,
) where {T}
    push!(
        f.terms,
        MOI.VectorAffineTerm(output_index, MOI.ScalarAffineTerm(op(one(T)), g)),
    )
    return f
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.terms,
        MOI.VectorAffineTerm.(
            1:d,
            MOI.ScalarAffineTerm.(op(one(T)), g.variables),
        ),
    )
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T}
    append!(
        f.terms,
        MOI.VectorAffineTerm.(output_index, operate_terms(op, g.terms)),
    )
    return operate_output_index!(op, T, output_index, f, MOI.constant(g))
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T}
    append!(f.terms, operate_terms(op, g.terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    return operate(op, T, f, g)
end

# Vector Quadratic +/-! ...
function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    α::T,
) where {T}
    f.constants[output_index] = op(f.constants[output_index], α)
    return f
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::Vector{T},
) where {T}
    @assert MOI.output_dimension(f) == length(g)
    f.constants .= op.(f.constants, g)
    return f
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(
            collect(1:d),
            MOI.ScalarAffineTerm.(op(one(T)), g.variables),
        ),
    )
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T}
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(output_index, operate_terms(op, g.terms)),
    )
    return operate_output_index!(op, T, output_index, f, MOI.constant(g))
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T}
    append!(f.affine_terms, operate_terms(op, g.terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(output_index, operate_terms(op, g.affine_terms)),
    )
    append!(
        f.quadratic_terms,
        MOI.VectorQuadraticTerm.(
            output_index,
            operate_terms(op, g.quadratic_terms),
        ),
    )
    return operate_output_index!(op, T, output_index, f, MOI.constant(g))
end

function operate!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    append!(f.affine_terms, operate_terms(op, g.affine_terms))
    append!(f.quadratic_terms, operate_terms(op, g.quadratic_terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end

## operate
# + with at least 3 arguments, can use in-place as the user cannot use
# intermediate results
# overload
# function operate(op::typeof(+), ::Type{T}, f, g, h, args...) where T
#     return operate!(+, T, operate(+, T, f, g), h, args...)
# end

# function operate(op::typeof(+), ::Type{T}, f::VectorOfVariables) where T
#     return f
# end

function operate(op::typeof(-), ::Type{T}, f::MOI.VectorOfVariables) where {T}
    d = MOI.output_dimension(f)
    return MOI.VectorAffineFunction{T}(
        MOI.VectorAffineTerm.(
            collect(1:d),
            MOI.ScalarAffineTerm.(-one(T), f.variables),
        ),
        fill(zero(T), d),
    )
end

# Vector number +/- ...
function operate(
    op::typeof(+),
    ::Type{T},
    α::Vector{T},
    f::VectorLike{T},
) where {T}
    return operate(op, T, f, α)
end

function operate(
    ::typeof(-),
    ::Type{T},
    α::Vector{T},
    f::VectorLike{T},
) where {T}
    return operate!(+, T, operate(-, T, f), α)
end

# Vector Variable +/- ...
function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorOfVariables,
    α::Vector{T},
) where {T}
    d = MOI.output_dimension(f)
    @assert length(α) == d
    return MOI.VectorAffineFunction{T}(
        MOI.VectorAffineTerm.(
            collect(1:d),
            MOI.ScalarAffineTerm.(one(T), f.variables),
        ),
        op.(α),
    )
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(f)
    @assert MOI.output_dimension(g) == d
    return MOI.VectorAffineFunction{T}(
        vcat(
            MOI.VectorAffineTerm.(
                1:d,
                MOI.ScalarAffineTerm.(one(T), f.variables),
            ),
            MOI.VectorAffineTerm.(
                1:d,
                MOI.ScalarAffineTerm.(op(one(T)), g.variables),
            ),
        ),
        fill(zero(T), d),
    )
end

function operate(
    op::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return operate(op, T, g, f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

# Vector Affine +/- ...
function operate(
    op::Union{typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
) where {T}
    return MOI.VectorAffineFunction(
        operate_terms(op, f.terms),
        op.(f.constants),
    )
end

function operate(
    op::Union{typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
) where {T}
    return MOI.VectorQuadraticFunction(
        operate_terms(op, f.quadratic_terms),
        operate_terms(op, f.affine_terms),
        op.(f.constants),
    )
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::VectorAffineLike{T},
) where {T}
    return operate!(op, T, copy(f), g)
end

function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    return MOI.VectorQuadraticFunction(
        operate_terms(op, g.quadratic_terms),
        [f.terms; operate_terms(op, g.affine_terms)],
        op.(f.constants, g.constants),
    )
end

# Vector Quadratic +/- ...
function operate(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::VectorQuadraticLike{T},
) where {T}
    return operate!(op, T, copy(f), g)
end

function Base.:+(args::VectorLike{T}...) where {T}
    return operate(+, T, args...)
end

# Base.:+(α::Vector{T}, f::VectorLike{T}...) is too general as it also covers
# Base.:+(α::Vector) which is type piracy
function Base.:+(α::Vector{T}, f::VectorLike{T}, g::VectorLike{T}...) where {T}
    return operate(+, T, α, f, g...)
end

function Base.:+(f::VectorLike{T}, α::Vector{T}) where {T}
    return operate(+, T, f, α)
end

function Base.:-(args::VectorLike{T}...) where {T}
    return operate(-, T, args...)
end

function Base.:-(f::VectorLike{T}, α::Vector{T}) where {T}
    return operate(-, T, f, α)
end

function Base.:-(α::Vector{T}, f::VectorLike{T}) where {T}
    return operate(-, T, α, f)
end

####################################### * ######################################

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
    ::Type{T},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{T},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function operate!(::typeof(*), ::Type{T}, f::MOI.VariableIndex, α::T) where {T}
    return operate(*, T, α, f)
end

function operate(::typeof(*), ::Type{T}, α::T, f::MOI.VariableIndex) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(α, f)], zero(T))
end

function operate(
    ::typeof(*),
    ::Type{T},
    α::T,
    f::MOI.VectorOfVariables,
) where {T}
    return MOI.VectorAffineFunction{T}(
        [
            MOI.VectorAffineTerm(i, MOI.ScalarAffineTerm(α, f.variables[i])) for
            i in eachindex(f.variables)
        ],
        zeros(T, MOI.output_dimension(f)),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::Union{MOI.VariableIndex,MOI.VectorOfVariables},
    α::T,
) where {T}
    return operate(*, T, α, f)
end

# `<:Number` is a workaround for https://github.com/jump-dev/MathOptInterface.jl/issues/980
function operate!(
    ::typeof(*),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    α::T,
) where {T<:Number}
    map_terms!(term -> operate_term(*, term, α), f)
    f.constant *= α
    return f
end

function operate!(
    ::typeof(*),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    α::T,
) where {T}
    map_terms!(term -> operate_term(*, term, α), f)
    LinearAlgebra.rmul!(f.constants, α)
    return f
end

function operate(::typeof(*), ::Type{T}, α::T, f::TypedLike{T}) where {T}
    return operate!(*, T, copy(f), α)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T}
    return MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(f == g ? 2one(T) : one(T), f, g)],
        MOI.ScalarAffineTerm{T}[],
        zero(T),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.VariableIndex,
) where {T}
    if iszero(f.constant)
        aff_terms = MOI.ScalarAffineTerm{T}[]
    else
        aff_terms = [MOI.ScalarAffineTerm(f.constant, g)]
    end
    quad_terms = map(
        t -> MOI.ScalarQuadraticTerm(
            t.variable == g ? 2t.coefficient : t.coefficient,
            t.variable,
            g,
        ),
        f.terms,
    )
    return MOI.ScalarQuadraticFunction(quad_terms, aff_terms, zero(T))
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.ScalarAffineFunction{T},
) where {T}
    return operate(*, T, g, f)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T}
    nfterms = length(f.terms)
    ngterms = length(g.terms)
    quad_terms = Vector{MOI.ScalarQuadraticTerm{T}}(undef, nfterms * ngterms)
    k = 0
    for t1 in f.terms
        for t2 in g.terms
            k += 1
            quad_terms[k] = operate_term(*, t1, t2)
        end
    end
    @assert k == length(quad_terms)
    if iszero(f.constant)
        if iszero(g.constant)
            aff_terms = MOI.ScalarAffineTerm{T}[]
        else
            aff_terms = operate_term.(*, g.constant, f.terms)
        end
    else
        if iszero(g.constant)
            aff_terms = operate_term.(*, f.constant, g.terms)
        else
            aff_terms =
                Vector{MOI.ScalarAffineTerm{T}}(undef, nfterms + ngterms)
            map!(t -> operate_term(*, g.constant, t), aff_terms, f.terms)
            for i in 1:ngterms
                aff_terms[nfterms+i] = operate_term(*, f.constant, g.terms[i])
            end
        end
    end
    constant = f.constant * g.constant
    return MOI.ScalarQuadraticFunction(quad_terms, aff_terms, constant)
end

Base.:*(f::MOI.AbstractFunction) = f

# To avoid type piracy, we add at least one `ScalarLike` outside of the `...`.
function Base.:*(
    f::ScalarLike{T},
    g::ScalarLike{T},
    args::ScalarLike{T}...,
) where {T}
    return operate(*, T, f, g, args...)
end

function Base.:*(f::Number, g::Union{MOI.VariableIndex,MOI.VectorOfVariables})
    return operate(*, typeof(f), f, g)
end

function Base.:*(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}, g::Number)
    return operate(*, typeof(g), f, g)
end

# Used by sparse matrix multiplication after
# https://github.com/JuliaLang/julia/pull/33205
function Base.:*(f::TypedLike, g::Bool)
    if g
        return MA.copy_if_mutable(f)
    else
        return zero(typeof(f))
    end
end

Base.:*(f::Bool, g::TypedLike) = g * f

function Base.:^(func::MOI.ScalarAffineFunction{T}, p::Integer) where {T}
    if iszero(p)
        return one(MOI.ScalarQuadraticFunction{T})
    elseif isone(p)
        return convert(MOI.ScalarQuadraticFunction{T}, func)
    elseif p == 2
        return func * func
    else
        throw(ArgumentError("Cannot take $(typeof(func)) to the power $p."))
    end
end

function Base.:^(func::MOI.ScalarQuadraticFunction{T}, p::Integer) where {T}
    if iszero(p)
        return one(MOI.ScalarQuadraticFunction{T})
    elseif isone(p)
        return MA.mutable_copy(func)
    else
        throw(ArgumentError("Cannot take $(typeof(func)) to the power $p."))
    end
end

function LinearAlgebra.dot(f::ScalarLike, g::ScalarLike)
    return f * g
end

function LinearAlgebra.dot(α::T, func::TypedLike{T}) where {T}
    return α * func
end

function LinearAlgebra.dot(func::TypedLike{T}, α::T) where {T}
    return func * α
end

LinearAlgebra.adjoint(f::ScalarLike) = f
LinearAlgebra.transpose(f::ScalarLike) = f
LinearAlgebra.symmetric_type(::Type{F}) where {F<:ScalarLike} = F
LinearAlgebra.symmetric(f::ScalarLike, ::Symbol) = f

####################################### / ######################################

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
    ::Type{T},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{T},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function operate!(::typeof(/), ::Type{T}, f::MOI.VariableIndex, α::T) where {T}
    return operate(/, T, f, α)
end

function operate(::typeof(/), ::Type{T}, f::MOI.VariableIndex, α::T) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(inv(α), f)],
        zero(T),
    )
end

function operate!(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    α::T,
) where {T}
    map_terms!(term -> operate_term(/, term, α), f)
    f.constant /= α
    return f
end

function operate!(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    α::T,
) where {T}
    map_terms!(term -> operate_term(/, term, α), f)
    rmul!(f.constants, inv(α))
    return f
end

function operate!(
    ::typeof(/),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    α::T,
) where {T}
    f.affine_terms .= operate_term.(/, f.affine_terms, α)
    f.quadratic_terms .= operate_term.(/, f.quadratic_terms, α)
    f.constant /= α
    return f
end

function operate(::typeof(/), ::Type{T}, f::TypedLike{T}, α::T) where {T}
    return operate!(/, T, copy(f), α)
end

function operate(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.VariableIndex,MOI.VectorOfVariables},
    α::T,
) where {T}
    return operate(*, T, inv(α), f)
end

function Base.:/(f::TypedLike{T}, g::T) where {T}
    return operate(/, T, f, g)
end

function Base.:/(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}, g::Number)
    return operate(/, typeof(g), f, g)
end

#################### Concatenation of MOI functions: `vcat` ####################

"""
    fill_vector(
        vector::Vector,
        ::Type{T},
        fill_func::Function,
        dim_func::Function,
        funcs,
    ) where {T}

Fill the vector `vector` with
`fill_func(vector, vector_offset, output_offset, func)` for each function `func`
in `funcs` where `vector_offset` (resp. `output_offset`) is the sum of
`dim_func(T, func)` (resp. `output_dim(T, func)`) of previous functions of
`func`.

    fill_vector(
        vector::Vector,
        ::Type{T},
        vector_offset::Int,
        output_offset::Int,
        fill_func::Function,
        dim_func::Function,
        funcs...
    ) where {T}

Same than previous method but starting with possible nonzero `vector_offset` and
`output_offset`.
"""
function fill_vector(
    vector::Vector,
    ::Type{T},
    fill_func::F1,
    dim_func::F2,
    funcs,
) where {T,F1<:Function,F2<:Function}
    vector_offset = 0
    output_offset = 0
    for func in funcs
        fill_func(vector, vector_offset, output_offset, func)
        vector_offset += dim_func(T, func)
        output_offset += output_dim(T, func)
    end
    @assert length(vector) == vector_offset
    return
end

function fill_vector(
    vector::Vector,
    ::Type,
    vector_offset::Int,
    output_offset::Int,
    fill_func::F1,
    dim_func::F2,
) where {F1<:Function,F2<:Function}
    @assert length(vector) == vector_offset
    return
end

function fill_vector(
    vector::Vector,
    ::Type{T},
    vector_offset::Int,
    output_offset::Int,
    fill_func::F1,
    dim_func::F2,
    func,
    funcs...,
) where {T,F1<:Function,F2<:Function}
    fill_func(vector, vector_offset, output_offset, func)
    fill_vector(
        vector,
        T,
        vector_offset + dim_func(T, func),
        output_offset + output_dim(T, func),
        fill_func,
        dim_func,
        funcs...,
    )
    return
end

function fill_variables(
    variables::Vector{MOI.VariableIndex},
    offset::Int,
    output_offset::Int,
    func::MOI.VariableIndex,
)
    variables[offset+1] = func
    return
end

function fill_variables(
    variables::Vector{MOI.VariableIndex},
    offset::Int,
    ::Int,
    func::MOI.VectorOfVariables,
)
    variables[offset.+(1:length(func.variables))] .= func.variables
    return
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}}...,
) where {T}
    return MOI.VectorOfVariables
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    funcs::Union{MOI.VariableIndex,MOI.VectorOfVariables}...,
) where {T}
    out_dim = sum(func -> output_dim(T, func), funcs)
    variables = Vector{MOI.VariableIndex}(undef, out_dim)
    fill_vector(variables, T, 0, 0, fill_variables, output_dim, funcs...)
    return MOI.VectorOfVariables(variables)
end

number_of_affine_terms(::Type{T}, ::T) where {T} = 0

number_of_affine_terms(::Type, ::MOI.VariableIndex) = 1

number_of_affine_terms(::Type, f::MOI.VectorOfVariables) = length(f.variables)

function number_of_affine_terms(
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.VectorAffineFunction{T}},
) where {T}
    return length(f.terms)
end

function number_of_affine_terms(
    ::Type{T},
    f::Union{MOI.ScalarQuadraticFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return length(f.affine_terms)
end

function number_of_quadratic_terms(
    ::Type{T},
    ::Union{
        T,
        MOI.VariableIndex,
        MOI.VectorOfVariables,
        MOI.ScalarAffineFunction{T},
        MOI.VectorAffineFunction{T},
    },
) where {T}
    return 0
end

function number_of_quadratic_terms(
    ::Type{T},
    f::Union{MOI.ScalarQuadraticFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return length(f.quadratic_terms)
end

function offset_term(t::MOI.ScalarAffineTerm, offset::Int)
    return MOI.VectorAffineTerm(offset + 1, t)
end

function offset_term(t::MOI.VectorAffineTerm, offset::Int)
    return MOI.VectorAffineTerm(offset + t.output_index, t.scalar_term)
end

function offset_term(t::MOI.ScalarQuadraticTerm, offset::Int)
    return MOI.VectorQuadraticTerm(offset + 1, t)
end

function offset_term(t::MOI.VectorQuadraticTerm, offset::Int)
    return MOI.VectorQuadraticTerm(offset + t.output_index, t.scalar_term)
end

function fill_terms(
    ::Vector{MOI.VectorAffineTerm{T}},
    ::Int,
    ::Int,
    ::T,
) where {T}
    return
end

function fill_terms(
    terms::Vector{MOI.VectorAffineTerm{T}},
    offset::Int,
    output_offset::Int,
    func::MOI.VariableIndex,
) where {T}
    terms[offset+1] =
        offset_term(MOI.ScalarAffineTerm(one(T), func), output_offset)
    return
end

function fill_terms(
    terms::Vector{MOI.VectorAffineTerm{T}},
    offset::Int,
    output_offset::Int,
    func::MOI.VectorOfVariables,
) where {T}
    n = number_of_affine_terms(T, func)
    terms[offset.+(1:n)] .=
        MOI.VectorAffineTerm.(
            output_offset .+ (1:n),
            MOI.ScalarAffineTerm.(one(T), func.variables),
        )
    return
end

function fill_terms(
    terms::Vector{MOI.VectorAffineTerm{T}},
    offset::Int,
    output_offset::Int,
    func::Union{MOI.ScalarAffineFunction{T},MOI.VectorAffineFunction{T}},
) where {T}
    n = number_of_affine_terms(T, func)
    terms[offset.+(1:n)] .= offset_term.(func.terms, output_offset)
    return
end

function fill_terms(
    terms::Vector{MOI.VectorAffineTerm{T}},
    offset::Int,
    output_offset::Int,
    func::Union{MOI.ScalarQuadraticFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    n = number_of_affine_terms(T, func)
    terms[offset.+(1:n)] .= offset_term.(func.affine_terms, output_offset)
    return
end

function fill_terms(
    ::Vector{MOI.VectorQuadraticTerm{T}},
    ::Int,
    ::Int,
    ::Union{
        T,
        MOI.VariableIndex,
        MOI.VectorOfVariables,
        MOI.ScalarAffineFunction{T},
        MOI.VectorAffineFunction{T},
    },
) where {T}
    return
end

function fill_terms(
    terms::Vector{MOI.VectorQuadraticTerm{T}},
    offset::Int,
    output_offset::Int,
    func::Union{MOI.ScalarQuadraticFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    n = number_of_quadratic_terms(T, func)
    terms[offset.+(1:n)] .= offset_term.(func.quadratic_terms, output_offset)
    return
end

output_dim(::Type{T}, ::T) where {T} = 1

output_dim(::Type, func::MOI.AbstractFunction) = MOI.output_dimension(func)

function fill_constant(
    constant::Vector{T},
    offset::Int,
    ::Int,
    func::T,
) where {T}
    constant[offset+1] = func
    return
end

function fill_constant(
    ::Vector{T},
    ::Int,
    ::Int,
    ::Union{MOI.VariableIndex,MOI.VectorOfVariables},
) where {T}
    return
end

function fill_constant(
    constant::Vector{T},
    offset::Int,
    ::Int,
    func::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    constant[offset+1] = func.constant
    return
end

function fill_constant(
    constant::Vector{T},
    offset::Int,
    ::Int,
    func::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    n = MOI.output_dimension(func)
    constant[offset.+(1:n)] .= func.constants
    return
end

"""
    vectorize(x::AbstractVector{MOI.VariableIndex})

Returns the vector of scalar affine functions in the form of a
`MOI.VectorAffineFunction{T}`.
"""
vectorize(x::AbstractVector{MOI.VariableIndex}) = MOI.VectorOfVariables(x)

"""
    vectorize(funcs::AbstractVector{MOI.ScalarAffineFunction{T}}) where T

Returns the vector of scalar affine functions in the form of a
`MOI.VectorAffineFunction{T}`.
"""
function vectorize(funcs::AbstractVector{MOI.ScalarAffineFunction{T}}) where {T}
    nterms =
        mapreduce(func -> number_of_affine_terms(T, func), +, funcs, init = 0)
    out_dim = mapreduce(func -> output_dim(T, func), +, funcs, init = 0)
    terms = Vector{MOI.VectorAffineTerm{T}}(undef, nterms)
    constant = zeros(T, out_dim)
    fill_vector(terms, T, fill_terms, number_of_affine_terms, funcs)
    fill_vector(constant, T, fill_constant, output_dim, funcs)
    return MOI.VectorAffineFunction(terms, constant)
end

"""
    vectorize(funcs::AbstractVector{MOI.ScalarQuadraticFunction{T}}) where T

Returns the vector of scalar quadratic functions in the form of a
`MOI.VectorQuadraticFunction{T}`.
"""
function vectorize(
    funcs::AbstractVector{MOI.ScalarQuadraticFunction{T}},
) where {T}
    num_affine_terms =
        mapreduce(func -> number_of_affine_terms(T, func), +, funcs, init = 0)
    num_quadratic_terms = mapreduce(
        func -> number_of_quadratic_terms(T, func),
        +,
        funcs,
        init = 0,
    )
    out_dim = mapreduce(func -> output_dim(T, func), +, funcs, init = 0)
    affine_terms = Vector{MOI.VectorAffineTerm{T}}(undef, num_affine_terms)
    quadratic_terms =
        Vector{MOI.VectorQuadraticTerm{T}}(undef, num_quadratic_terms)
    constant = zeros(T, out_dim)
    fill_vector(affine_terms, T, fill_terms, number_of_affine_terms, funcs)
    fill_vector(
        quadratic_terms,
        T,
        fill_terms,
        number_of_quadratic_terms,
        funcs,
    )
    fill_vector(constant, T, fill_constant, output_dim, funcs)
    return MOI.VectorQuadraticFunction(quadratic_terms, affine_terms, constant)
end

function promote_operation(::typeof(vcat), ::Type{T}, ::Type{T}...) where {T}
    return Vector{T}
end

# TODO Remove `<:Number` when we drop Julia v1.1.1, otherwise it gives a `StackOverflowError`
function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Union{Type{T},Type{<:AbstractVector{T}}}...,
) where {T<:Number}
    return Vector{T}
end

# TODO Remove `<:Number` when we drop Julia v1.1.1, it's needed for disambiguation
function operate(
    ::typeof(vcat),
    ::Type{T},
    funcs::Union{T,AbstractVector{T}}...,
) where {T<:Number}
    return vcat(funcs...)
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{
        <:Union{
            ScalarAffineLike{T},
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
        },
    }...,
) where {T}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{
        <:Union{
            ScalarQuadraticLike{T},
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
            MOI.VectorQuadraticFunction{T},
        },
    }...,
) where {T}
    return MOI.VectorQuadraticFunction{T}
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    funcs::Union{
        ScalarAffineLike{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
    }...,
) where {T}
    nterms = sum(func -> number_of_affine_terms(T, func), funcs)
    out_dim = sum(func -> output_dim(T, func), funcs)
    terms = Vector{MOI.VectorAffineTerm{T}}(undef, nterms)
    constant = zeros(T, out_dim)
    fill_vector(terms, T, 0, 0, fill_terms, number_of_affine_terms, funcs...)
    fill_vector(constant, T, 0, 0, fill_constant, output_dim, funcs...)
    return MOI.VectorAffineFunction(terms, constant)
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    funcs::Union{
        ScalarQuadraticLike{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    }...,
) where {T}
    num_affine_terms = sum(func -> number_of_affine_terms(T, func), funcs)
    num_quadratic_terms = sum(func -> number_of_quadratic_terms(T, func), funcs)
    out_dim = sum(func -> output_dim(T, func), funcs)
    affine_terms = Vector{MOI.VectorAffineTerm{T}}(undef, num_affine_terms)
    quadratic_terms =
        Vector{MOI.VectorQuadraticTerm{T}}(undef, num_quadratic_terms)
    constant = zeros(T, out_dim)
    fill_vector(
        affine_terms,
        T,
        0,
        0,
        fill_terms,
        number_of_affine_terms,
        funcs...,
    )
    fill_vector(
        quadratic_terms,
        T,
        0,
        0,
        fill_terms,
        number_of_quadratic_terms,
        funcs...,
    )
    fill_vector(constant, T, 0, 0, fill_constant, output_dim, funcs...)
    return MOI.VectorQuadraticFunction(quadratic_terms, affine_terms, constant)
end

"""
    scalarize(func::MOI.VectorOfVariables, ignore_constants::Bool = false)

Returns a vector of scalar functions making up the vector function in the form
of a `Vector{MOI.SingleVariable}`.

See also [`eachscalar`](@ref).
"""
function scalarize(f::MOI.VectorOfVariables, ignore_constants::Bool = false)
    return f.variables
end

"""
    scalarize(func::MOI.VectorAffineFunction{T}, ignore_constants::Bool = false)

Returns a vector of scalar functions making up the vector function in the form
of a `Vector{MOI.ScalarAffineFunction{T}}`.

See also [`eachscalar`](@ref).
"""
function scalarize(
    f::MOI.VectorAffineFunction{T},
    ignore_constants::Bool = false,
) where {T}
    dimension = MOI.output_dimension(f)
    constants = ignore_constants ? zeros(T, dimension) : MOI.constant(f)
    counting = count_terms(dimension, f.terms)
    functions = MOI.ScalarAffineFunction{T}[
        MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}[], constants[i]) for i in 1:dimension
    ]
    for i in 1:dimension
        sizehint!(functions[i].terms, counting[i])
    end
    for term in f.terms
        push!(functions[term.output_index].terms, term.scalar_term)
    end
    return functions
end

"""
    scalarize(func::MOI.VectorQuadraticFunction{T}, ignore_constants::Bool = false)

Returns a vector of scalar functions making up the vector function in the form
of a `Vector{MOI.ScalarQuadraticFunction{T}}`.

See also [`eachscalar`](@ref).
"""
function scalarize(
    f::MOI.VectorQuadraticFunction{T},
    ignore_constants::Bool = false,
) where {T}
    dimension = MOI.output_dimension(f)
    constants = ignore_constants ? zeros(T, dimension) : MOI.constant(f)
    counting_scalars = count_terms(dimension, f.affine_terms)
    counting_quadratics = count_terms(dimension, f.quadratic_terms)
    functions = MOI.ScalarQuadraticFunction{T}[
        MOI.ScalarQuadraticFunction{T}(
            MOI.ScalarQuadraticTerm{T}[],
            MOI.ScalarAffineTerm{T}[],
            constants[i],
        ) for i in 1:dimension
    ]
    for i in 1:dimension
        sizehint!(functions[i].affine_terms, counting_scalars[i])
        sizehint!(functions[i].quadratic_terms, counting_quadratics[i])
    end
    for term in f.affine_terms
        push!(functions[term.output_index].affine_terms, term.scalar_term)
    end
    for term in f.quadratic_terms
        push!(functions[term.output_index].quadratic_terms, term.scalar_term)
    end
    return functions
end

function count_terms(counting::Vector{<:Integer}, terms::Vector{T}) where {T}
    for term in terms
        counting[term.output_index] += 1
    end
    return
end

function count_terms(dimension::I, terms::Vector{T}) where {I,T}
    counting = zeros(I, dimension)
    count_terms(counting, terms)
    return counting
end

tol_default(T::Type{<:Union{Integer,Rational}}) = zero(T)

tol_default(T::Type{<:AbstractFloat}) = sqrt(eps(T))

convert_approx(::Type{T}, func::T; kws...) where {T} = func

function convert_approx(
    ::Type{MOI.VariableIndex},
    func::MOI.ScalarAffineFunction{T};
    tol = tol_default(T),
) where {T}
    f = canonical(func)
    i = findfirst(t -> isapprox(t.coefficient, one(T), atol = tol), f.terms)
    if abs(f.constant) > tol ||
       i === nothing ||
       any(
           j -> j != i && abs(f.terms[j].coefficient) > tol,
           eachindex(f.terms),
       )
        throw(InexactError(:convert_approx, MOI.VariableIndex, func))
    end
    return f.terms[i].variable
end

function convert_approx(
    ::Type{MOI.VectorOfVariables},
    func::MOI.VectorAffineFunction{T};
    tol = tol_default(T),
) where {T}
    return MOI.VectorOfVariables([
        convert_approx(MOI.VariableIndex, f, tol = tol) for f in scalarize(func)
    ])
end

function Base.zero(F::Type{<:TypedScalarLike{T}}) where {T}
    return convert(F, zero(T))
end

function Base.one(F::Type{<:TypedScalarLike{T}}) where {T}
    return convert(F, one(T))
end

# !!! note
#     These two `promote_rules` are written explicitly instead of
#     `TypedScalarLike` in order to avoid a method invalidation.
function Base.promote_rule(
    F::Type{MOI.ScalarAffineFunction{T}},
    ::Type{T},
) where {T}
    return F
end

function Base.promote_rule(
    F::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{T},
) where {T}
    return F
end

function operate_coefficient(f, term::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(f(term.coefficient), term.variable)
end

function operate_coefficient(f, term::MOI.ScalarQuadraticTerm)
    return MOI.ScalarQuadraticTerm(
        f(term.coefficient),
        term.variable_1,
        term.variable_2,
    )
end

function operate_coefficient(f, term::MOI.VectorAffineTerm)
    return MOI.VectorAffineTerm(
        term.output_index,
        operate_coefficient(f, term.scalar_term),
    )
end

function operate_coefficient(f, term::MOI.VectorQuadraticTerm)
    return MOI.VectorQuadraticTerm(
        term.output_index,
        operate_coefficient(f, term.scalar_term),
    )
end

function operate_coefficients(f, func::MOI.ScalarAffineFunction)
    return MOI.ScalarAffineFunction(
        [operate_coefficient(f, term) for term in func.terms],
        f(func.constant),
    )
end

function operate_coefficients(f, func::MOI.ScalarQuadraticFunction)
    return MOI.ScalarQuadraticFunction(
        [operate_coefficient(f, term) for term in func.quadratic_terms],
        [operate_coefficient(f, term) for term in func.affine_terms],
        f(func.constant),
    )
end

function operate_coefficients(f, func::MOI.VectorAffineFunction)
    return MOI.VectorAffineFunction(
        [operate_coefficient(f, term) for term in func.terms],
        map(f, func.constants),
    )
end

function operate_coefficients(f, func::MOI.VectorQuadraticFunction)
    return MOI.VectorQuadraticFunction(
        [operate_coefficient(f, term) for term in func.quadratic_terms],
        [operate_coefficient(f, term) for term in func.affine_terms],
        map(f, func.constants),
    )
end

function Base.:*(α::T, g::TypedLike{T}) where {T}
    return operate_coefficients(β -> α * β, g)
end

function Base.:*(α::Number, g::TypedLike)
    return operate_coefficients(β -> α * β, g)
end

# Breaks ambiguity
function Base.:*(α::T, g::TypedLike{T}) where {T<:Number}
    return operate_coefficients(β -> α * β, g)
end

function Base.:*(g::TypedLike{T}, β::T) where {T}
    return operate_coefficients(α -> α * β, g)
end

function Base.:*(g::TypedLike, β::Number)
    return operate_coefficients(α -> α * β, g)
end

# Breaks ambiguity
function Base.:*(g::TypedLike{T}, β::T) where {T<:Number}
    return operate_coefficients(α -> α * β, g)
end

function is_coefficient_type(
    ::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
    ::Type,
)
    return true
end

is_coefficient_type(::Type{<:TypedLike{T}}, ::Type{T}) where {T} = true

is_coefficient_type(::Type{<:TypedLike}, ::Type) = false

function similar_type(::Type{<:MOI.ScalarAffineFunction}, ::Type{T}) where {T}
    return MOI.ScalarAffineFunction{T}
end

function similar_type(
    ::Type{<:MOI.ScalarQuadraticFunction},
    ::Type{T},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function similar_type(::Type{<:MOI.VectorAffineFunction}, ::Type{T}) where {T}
    return MOI.VectorAffineFunction{T}
end

function similar_type(
    ::Type{<:MOI.VectorQuadraticFunction},
    ::Type{T},
) where {T}
    return MOI.VectorQuadraticFunction{T}
end

## Complex operations
# We assume MOI variables are real numbers.

function MA.promote_operation(
    op::Union{typeof(real),typeof(imag),typeof(conj)},
    F::Type{<:TypedLike{T}},
) where {T}
    return similar_type(F, MA.promote_operation(op, T))
end

Base.real(f::TypedLike) = operate_coefficients(real, f)

function MA.promote_operation(
    ::typeof(real),
    T::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
)
    return T
end

Base.real(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}) = f

function promote_operation(
    ::typeof(imag),
    ::Type{T},
    ::Type{MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(imag),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T}
    return MOI.VectorAffineFunction{T}
end

function operate(::typeof(imag), ::Type{T}, f::MOI.VectorOfVariables) where {T}
    return zero_with_output_dimension(
        MOI.VectorAffineFunction{T},
        MOI.output_dimension(f),
    )
end

Base.imag(f::TypedLike) = operate_coefficients(imag, f)

operate(::typeof(imag), ::Type, f::TypedLike) = imag(f)

function operate(::typeof(imag), ::Type{T}, ::MOI.VariableIndex) where {T}
    return zero(MOI.ScalarAffineFunction{T})
end

Base.conj(f::TypedLike) = operate_coefficients(conj, f)

function MA.promote_operation(
    ::typeof(conj),
    T::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
)
    return T
end

Base.conj(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}) = f
