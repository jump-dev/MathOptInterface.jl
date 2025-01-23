# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Functions convertible to a ScalarAffineFunction
const ScalarAffineLike{T} =
    Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}}
# Functions convertible to a ScalarQuadraticFunction
const ScalarQuadraticLike{T} =
    Union{ScalarAffineLike{T},MOI.ScalarQuadraticFunction{T}}

# `ScalarLike` for which `T` is defined to avoid defining, for example,
# `+(::VariableIndex, ::Any)` which should rather be
# `+(::VariableIndex, ::Number)`.
const TypedScalarLike{T} =
    Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}
# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading for example, `+(::Float64, ::Float64)`
const ScalarLike{T} = Union{MOI.VariableIndex,TypedScalarLike{T}}

# Functions convertible to a VectorAffineFunction
const VectorAffineLike{T} =
    Union{Vector{T},MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}
# Functions convertible to a VectorQuadraticFunction
const VectorQuadraticLike{T} =
    Union{VectorAffineLike{T},MOI.VectorQuadraticFunction{T}}

# `VectorLike` for which `T` is defined to avoid defining, for example,
# `+(::VectorOfVariables, ::Any)` which should rather be
# `+(::VectorOfVariables, ::Number)`.
const TypedVectorLike{T} =
    Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}}
# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading for example, `+(::Float64, ::Float64)`
const VectorLike{T} = Union{MOI.VectorOfVariables,TypedVectorLike{T}}

const TypedLike{T} = Union{TypedScalarLike{T},TypedVectorLike{T}}

const ObjectWithoutIndex = Union{
    AbstractString,
    Enum,
    Function,
    Nothing,
    Number,
    Symbol,
    Type,
    MOI.AbstractSet,
    MOI.AnyAttribute,
    MOI.ModelLike,
    MOI.NLPBlockData,
}

const ObjectOrTupleWithoutIndex =
    Union{ObjectWithoutIndex,Tuple{Vararg{ObjectWithoutIndex}}}

const ObjectOrTupleOrArrayWithoutIndex = Union{
    ObjectOrTupleWithoutIndex,
    AbstractArray{<:ObjectOrTupleWithoutIndex},
    AbstractArray{<:AbstractArray{<:ObjectOrTupleWithoutIndex}},
}

"""
    variable_function_type(::Type{<:MOI.AbstractSet})

Return the variable type associated with a set, that is, [`MOI.VariableIndex`](@ref)
for scalar sets and [`MOI.VectorOfVariables`](@ref) for vector sets.
"""
variable_function_type(::Type{<:MOI.AbstractScalarSet}) = MOI.VariableIndex

variable_function_type(::Type{<:MOI.AbstractVectorSet}) = MOI.VectorOfVariables

"""
    value_type(::Type{T}, ::Type{F}) where {T,F<:MOI.AbstractFunction}

Returns the output type that results if a function of type `F` is evaluated
using variables with numeric type `T`.

In other words, this is the return type for
`MOI.Utilities.eval_variables(value_fn::Function, model, f::F)` for a function
`value_fn(::MOI.VariableIndex)::T`.
"""
function value_type end

value_type(::Type{T}, ::Type{MOI.VariableIndex}) where {T} = T

function value_type(
    ::Type{T},
    ::Type{<:Union{MOI.ScalarAffineFunction{C},MOI.ScalarQuadraticFunction{C}}},
) where {C,T}
    return MA.promote_operation(*, C, T)
end

value_type(::Type{T}, ::Type{MOI.ScalarNonlinearFunction}) where {T} = T

function value_type(
    ::Type{T},
    ::Type{F},
) where {T,F<:MOI.AbstractVectorFunction}
    return Vector{value_type(T, scalar_type(F))}
end

"""
    eval_variables(value_fn::Function, f::MOI.AbstractFunction)

Returns the value of function `f` if each variable index `vi` is evaluated as
`value_fn(vi)`.

Note that `value_fn` must return a Number. See [`substitute_variables`](@ref)
for a similar function where `value_fn` returns an
[`MOI.AbstractScalarFunction`](@ref).

!!! warning
    The two-argument version of `eval_variables` is deprecated and may be
    removed in MOI v2.0.0. Use the three-argument method
    `eval_variables(::Function, ::MOI.ModelLike, ::MOI.AbstractFunction)`
    instead.
"""
function eval_variables end

function eval_variables(value_fn::Function, t::MOI.ScalarAffineTerm)
    return t.coefficient * value_fn(t.variable)
end

function eval_variables(value_fn::Function, t::MOI.ScalarQuadraticTerm)
    out = t.coefficient * value_fn(t.variable_1) * value_fn(t.variable_2)
    return t.variable_1 == t.variable_2 ? out / 2 : out
end

eval_variables(value_fn::Function, f::MOI.VariableIndex) = value_fn(f)

function eval_variables(value_fn::Function, f::MOI.ScalarAffineFunction)
    out = f.constant
    for t in f.terms
        out += eval_variables(value_fn, t)
    end
    return out
end

function eval_variables(value_fn::Function, f::MOI.ScalarQuadraticFunction)
    out = f.constant
    for a in f.affine_terms
        out += eval_variables(value_fn, a)
    end
    for q in f.quadratic_terms
        out += eval_variables(value_fn, q)
    end
    return out
end

function eval_variables(value_fn::Function, f::MOI.VectorOfVariables)
    return map(value_fn, f.variables)
end

function eval_variables(value_fn::Function, f::MOI.VectorAffineFunction)
    out = copy(f.constants)
    for t in f.terms
        out[t.output_index] += eval_variables(value_fn, t.scalar_term)
    end
    return out
end

function eval_variables(value_fn::Function, f::MOI.VectorQuadraticFunction)
    out = copy(f.constants)
    for t in f.affine_terms
        out[t.output_index] += eval_variables(value_fn, t.scalar_term)
    end
    for t in f.quadratic_terms
        out[t.output_index] += eval_variables(value_fn, t.scalar_term)
    end
    return out
end

"""
    eval_variables(
        value_fn::Function,
        model::MOI.ModelLike,
        f::MOI.AbstractFunction,
    )

Returns the value of function `f` if each variable index `vi` is evaluated as
`value_fn(vi)`.

Note that `value_fn` must return a Number. See [`substitute_variables`](@ref)
for a similar function where `value_fn` returns an
[`MOI.AbstractScalarFunction`](@ref).
"""
function eval_variables(
    value_fn::F,
    model::MOI.ModelLike,
    f::Union{MOI.AbstractFunction,Real,AbstractVector{<:Real}},
) where {F}
    return eval_variables(value_fn, f)
end

# The `eval_variables(::F, ::MOI.ModelLike, ::MOI.ScalarNonlinearFunction)`
# method is defined in the MOI.Nonlinear submodule.

function eval_variables(
    value_fn::F,
    model::MOI.ModelLike,
    f::MOI.VectorNonlinearFunction,
) where {F}
    return map(f.rows) do row
        return eval_variables(value_fn, model, row)
    end
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
    return map_indices(Base.Fix1(getindex, variable_map), x)
end

map_indices(::F, x::ObjectOrTupleOrArrayWithoutIndex) where {F<:Function} = x

function map_indices(index_map::F, vi::MOI.VariableIndex) where {F<:Function}
    return index_map(vi)
end

function map_indices(index_map::F, ci::MOI.ConstraintIndex) where {F<:Function}
    return index_map(ci)
end

function map_indices(index_map::F, x::AbstractArray) where {F<:Function}
    # @odow tried other alternatives here, like
    #     [map_indices(index_map, xi) for xi in x]
    # and
    #     map(Base.Fix1(map_indices, index_map), x)
    # but this was the fastest. The insight is that we know that map_indices
    # does not modify the element type of `x`.
    y = similar(x)
    for i in eachindex(x)
        y[i] = map_indices(index_map, x[i])
    end
    return y
end

function map_indices(index_map::F, t::MOI.ScalarAffineTerm) where {F<:Function}
    return MOI.ScalarAffineTerm(t.coefficient, index_map(t.variable))
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

function map_indices(index_map::F, t::MOI.VectorAffineTerm) where {F<:Function}
    return MOI.VectorAffineTerm(
        t.output_index,
        map_indices(index_map, t.scalar_term),
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

function map_indices(
    index_map::F,
    f::MOI.ScalarAffineFunction,
) where {F<:Function}
    return MOI.ScalarAffineFunction(
        map_indices(index_map, f.terms),
        MOI.constant(f),
    )
end

function map_indices(
    index_map::F,
    f::MOI.ScalarQuadraticFunction,
) where {F<:Function}
    return MOI.ScalarQuadraticFunction(
        map_indices(index_map, f.quadratic_terms),
        map_indices(index_map, f.affine_terms),
        MOI.constant(f),
    )
end

function map_indices(index_map::F, f::MOI.VectorOfVariables) where {F<:Function}
    return MOI.VectorOfVariables(map_indices(index_map, f.variables))
end

function map_indices(
    index_map::F,
    f::MOI.VectorAffineFunction,
) where {F<:Function}
    return MOI.VectorAffineFunction(
        map_indices(index_map, f.terms),
        MOI.constant(f),
    )
end

function map_indices(
    index_map::F,
    f::MOI.VectorQuadraticFunction,
) where {F<:Function}
    return MOI.VectorQuadraticFunction(
        map_indices(index_map, f.quadratic_terms),
        map_indices(index_map, f.affine_terms),
        MOI.constant(f),
    )
end

function map_indices(
    index_map::F,
    f::MOI.ScalarNonlinearFunction,
) where {F<:Function}
    root = MOI.ScalarNonlinearFunction(f.head, similar(f.args))
    stack = Tuple{MOI.ScalarNonlinearFunction,Int,MOI.ScalarNonlinearFunction}[]
    for (i, fi) in enumerate(f.args)
        if fi isa MOI.ScalarNonlinearFunction
            push!(stack, (root, i, fi))
        else
            root.args[i] = MOI.Utilities.map_indices(index_map, fi)
        end
    end
    while !isempty(stack)
        parent, i, arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            child = MOI.ScalarNonlinearFunction(arg.head, similar(arg.args))
            for (j, argj) in enumerate(arg.args)
                if argj isa MOI.ScalarNonlinearFunction
                    push!(stack, (child, j, argj))
                else
                    child.args[j] = MOI.Utilities.map_indices(index_map, argj)
                end
            end
            parent.args[i] = child
        else
            parent.args[i] = MOI.Utilities.map_indices(index_map, arg)
        end
    end
    return root
end

function map_indices(
    index_map::F,
    f::MOI.VectorNonlinearFunction,
) where {F<:Function}
    return MOI.VectorNonlinearFunction(map_indices(index_map, f.rows))
end

map_indices(::F, change::MOI.ScalarConstantChange) where {F<:Function} = change

map_indices(::F, change::MOI.VectorConstantChange) where {F<:Function} = change

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
    change::MOI.ScalarQuadraticCoefficientChange,
) where {F<:Function}
    return MOI.ScalarQuadraticCoefficientChange(
        index_map(change.variable_1),
        index_map(change.variable_2),
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

!!! note
    When implementing a new method, don't use `substitute_variables(::Function`,
    because Julia will not specialize on it. Use instead
    `substitute_variables(::F, ...) where {F<:Function}`.
"""
function substitute_variables end

function substitute_variables(
    ::F,
    x::ObjectOrTupleOrArrayWithoutIndex,
) where {F<:Function}
    return x
end

function substitute_variables(
    variable_map::F,
    x::MOI.VariableIndex,
) where {F<:Function}
    f = variable_map(x)
    if f != x
        error("Cannot substitute `$x` as it is bridged into `$f`.")
    end
    return x
end

# This method is used when submitting `HeuristicSolution`.
function substitute_variables(
    variable_map::F,
    x::Vector{MOI.VariableIndex},
) where {F<:Function}
    return substitute_variables.(variable_map, x)
end

function substitute_variables(
    variable_map::F,
    term::MOI.ScalarAffineTerm{T},
) where {T,F<:Function}
    # We could have `T = Complex{Float64}` and `variable_map(term.variable)`
    # be a `MOI.ScalarAffineFunction{Float64}` with the Hermitian to PSD bridge.
    # We convert to `MOI.ScalarAffineFunction{T}` to avoid any issue.
    f = convert(MOI.ScalarAffineFunction{T}, variable_map(term.variable))
    return operate(*, T, term.coefficient, f)::MOI.ScalarAffineFunction{T}
end

function substitute_variables(
    variable_map::F,
    term::MOI.ScalarQuadraticTerm{T},
) where {T,F<:Function}
    # We could have `T = Complex{Float64}` and `variable_map(term.variable)`
    # be a `MOI.ScalarAffineFunction{Float64}` with the Hermitian to PSD bridge.
    # We convert to `MOI.ScalarAffineFunction{T}` to avoid any issue.
    f1 = convert(MOI.ScalarAffineFunction{T}, variable_map(term.variable_1))
    f2 = convert(MOI.ScalarAffineFunction{T}, variable_map(term.variable_2))
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
    f::MOI.ScalarAffineFunction{T},
) where {T,F<:Function}
    g = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], MOI.constant(f))
    for term in f.terms
        # This works because substitute_variables actually returns a function,
        # not a term.
        g = operate!(+, T, g, substitute_variables(variable_map, term))
    end
    return g
end

function substitute_variables(
    variable_map::F,
    f::MOI.ScalarQuadraticFunction{T},
) where {T,F<:Function}
    g = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{T}[],
        MOI.ScalarAffineTerm{T}[],
        MOI.constant(f),
    )
    for a_term in f.affine_terms
        g = operate!(+, T, g, substitute_variables(variable_map, a_term))
    end
    for q_term in f.quadratic_terms
        g = operate!(+, T, g, substitute_variables(variable_map, q_term))
    end
    return g
end

function substitute_variables(
    variable_map::F,
    f::MOI.ScalarNonlinearFunction,
) where {F<:Function}
    new_args = Any[]
    for arg in f.args
        if arg isa MOI.VariableIndex
            push!(new_args, variable_map(arg))
        else
            push!(new_args, substitute_variables(variable_map, arg))
        end
    end
    return MOI.ScalarNonlinearFunction(f.head, new_args)
end

function substitute_variables(
    variable_map::F,
    f::MOI.VectorAffineFunction{T},
) where {T,F<:Function}
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm{T}[],
        copy(MOI.constant(f)),
    )
    for term in f.terms
        term_f = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, term_f)
    end
    return g
end

function substitute_variables(
    variable_map::F,
    f::MOI.VectorQuadraticFunction{T},
) where {T,F<:Function}
    g = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm{T}[],
        MOI.VectorAffineTerm{T}[],
        copy(MOI.constant(f)),
    )
    for term in f.affine_terms
        sub = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, sub)
    end
    for term in f.quadratic_terms
        sub = substitute_variables(variable_map, term.scalar_term)
        operate_output_index!(+, T, term.output_index, g, sub)
    end
    return g
end

function substitute_variables(
    variable_map::F,
    f::MOI.VectorNonlinearFunction,
) where {F<:Function}
    rows = [substitute_variables(variable_map, row) for row in f.rows]
    return MOI.VectorNonlinearFunction(rows)
end

"""
    scalar_type(F::Type{<:MOI.AbstractVectorFunction})

Type of functions obtained by indexing objects obtained by calling `eachscalar`
on functions of type `F`.
"""
function scalar_type end

scalar_type(::Type{<:AbstractVector{T}}) where {T} = T

scalar_type(::Type{MOI.VectorOfVariables}) = MOI.VariableIndex

function scalar_type(::Type{MOI.VectorAffineFunction{T}}) where {T}
    return MOI.ScalarAffineFunction{T}
end

function scalar_type(::Type{MOI.VectorQuadraticFunction{T}}) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

scalar_type(::Type{MOI.VectorNonlinearFunction}) = MOI.ScalarNonlinearFunction

"""
    vector_type(::Type{<:MOI.AbstractScalarFunction})

Return the [`MOI.AbstractVectorFunction`](@ref) associated with the scalar type
`F`.
"""
function vector_type end

vector_type(::Type{T}) where {T} = Vector{T}

vector_type(::Type{MOI.VariableIndex}) = MOI.VectorOfVariables

function vector_type(::Type{MOI.ScalarAffineFunction{T}}) where {T}
    return MOI.VectorAffineFunction{T}
end

function vector_type(::Type{MOI.ScalarQuadraticFunction{T}}) where {T}
    return MOI.VectorQuadraticFunction{T}
end

function vector_type(::Type{MOI.ScalarNonlinearFunction})
    return MOI.VectorNonlinearFunction
end

"""
    ScalarFunctionIterator{F<:MOI.AbstractVectorFunction}

A type that allows iterating over the scalar-functions that comprise an
`AbstractVectorFunction`.
"""
struct ScalarFunctionIterator{F<:MOI.AbstractVectorFunction,C,S} <:
       AbstractVector{S}
    f::F
    # Cache that can be used to store a precomputed datastructure that allows
    # an efficient implementation of `getindex`.
    cache::C
    function ScalarFunctionIterator(f::MOI.AbstractVectorFunction, cache)
        return new{typeof(f),typeof(cache),scalar_type(typeof(f))}(f, cache)
    end
end

function ScalarFunctionIterator(func::MOI.AbstractVectorFunction)
    return ScalarFunctionIterator(func, scalar_iterator_cache(func))
end

Base.size(s::ScalarFunctionIterator) = (MOI.output_dimension(s.f),)

scalar_iterator_cache(::MOI.AbstractVectorFunction) = nothing

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

function Base.eltype(
    ::ScalarFunctionIterator{F},
) where {F<:MOI.AbstractVectorFunction}
    return scalar_type(F)
end

Base.lastindex(it::ScalarFunctionIterator) = length(it)

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

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorNonlinearFunction},
    output_index::Integer,
)
    return it.f.rows[output_index]
end

function Base.getindex(
    it::ScalarFunctionIterator{MOI.VectorNonlinearFunction},
    output_index::AbstractVector{<:Integer},
)
    return MOI.VectorNonlinearFunction(it.f.rows[output_index])
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

# Generic fallback for items inside NonlinearFunctions like numbers.
is_canonical(::Any) = true

is_canonical(::MOI.AbstractFunction) = false

is_canonical(::Union{MOI.VariableIndex,MOI.VectorOfVariables}) = true

function is_canonical(f::MOI.ScalarNonlinearFunction)
    # Don't use recursion here. This gets called for all scalar nonlinear
    # constraints.
    stack = Any[arg for arg in f.args]
    while !isempty(stack)
        arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            for a in arg.args
                push!(stack, a)
            end
        else
            if !is_canonical(arg)
                return false
            end
        end
    end
    return true
end

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

is_canonical(f::MOI.VectorNonlinearFunction) = all(is_canonical, f.rows)

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
    canonical(f::MOI.AbstractFunction)

Returns the function in a canonical form, that is,

 * A term appear only once.
 * The coefficients are nonzero.
 * The terms appear in increasing order of variable where there the order of the
   variables is the order of their value.
 * For a `AbstractVectorFunction`, the terms are sorted in ascending order of
   output index.

The output of `canonical` can be assumed to be a copy of `f`, even for
`VectorOfVariables`.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x, y, z = MOI.VariableIndex.(1:3);

julia> f = MOI.ScalarAffineFunction(
           MOI.ScalarAffineTerm.(Float64[2, 1, 3, -2, -3], [y, x, z, x, z]),
           5.0,
       )
5.0 + 2.0 MOI.VariableIndex(2) + 1.0 MOI.VariableIndex(1) + 3.0 MOI.VariableIndex(3) - 2.0 MOI.VariableIndex(1) - 3.0 MOI.VariableIndex(3)

julia> MOI.Utilities.canonical(f)
5.0 - 1.0 MOI.VariableIndex(1) + 2.0 MOI.VariableIndex(2)
```
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

function canonical(f::MOI.ScalarNonlinearFunction)::MOI.ScalarNonlinearFunction
    return MOI.Nonlinear.SymbolicAD.simplify(f)
end

function canonicalize!(f::MOI.ScalarNonlinearFunction)
    for (i, arg) in enumerate(f.args)
        if !is_canonical(arg)
            f.args[i] = canonicalize!(arg)
        end
    end
    return f
end

function canonical(f::MOI.VectorNonlinearFunction)
    rows = MOI.Nonlinear.SymbolicAD.simplify.(f.rows)
    return MOI.VectorNonlinearFunction(rows)
end

function canonicalize!(f::MOI.VectorNonlinearFunction)
    for (i, fi) in enumerate(f.rows)
        f.rows[i] = canonicalize!(fi)
    end
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

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, -1], [x, x]), 0)
(0) + (1) MOI.VariableIndex(1) - (1) MOI.VariableIndex(1)

julia> MOI.Utilities.isapprox_zero(f, 1e-8)
false

julia> MOI.Utilities.isapprox_zero(MOI.Utilities.canonical(f), 1e-8)
true
```
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

function filter_variables(keep::Function, f::MOI.ScalarNonlinearFunction)
    args = Any[]
    first_arg_deleted = false
    for (i, arg) in enumerate(f.args)
        if arg isa MOI.VariableIndex
            if keep(arg)
                push!(args, arg)
            else
                if i == 1
                    first_arg_deleted = true
                end
                if !(f.head in (:+, :-, :*))
                    error("Unable to delete variable in `$(f.head) operation.")
                end
            end
        elseif arg isa Number
            push!(args, arg)
        else
            push!(args, filter_variables(keep, arg))
        end
    end
    if f.head == :-
        if first_arg_deleted
            # -(x, y...) has become -(y...), but it should be -(0, y...)
            pushfirst!(args, 0)
        elseif length(f.args) > 1 && length(args) == 1
            # -(x, y...) has become -(x), but it should be +(x)
            return f.args[1]
        end
    end
    return MOI.ScalarNonlinearFunction(f.head, args)
end

function filter_variables(keep::Function, f::MOI.VectorNonlinearFunction)
    return MOI.VectorNonlinearFunction(
        MOI.ScalarNonlinearFunction[
            filter_variables(keep, row) for row in f.rows
        ],
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

Return a copy of the function `f`, modified according to `change`.
"""
function modify_function(
    f::MOI.AbstractFunction,
    change::MOI.AbstractFunctionModification,
)
    new_f = copy(f)
    modify_function!(new_f, change)
    return new_f
end

"""
    modify_function!(f::AbstractFunction, change::AbstractFunctionModification)

Modify the function `f` in-place, according to `change`.
"""
function modify_function!(
    f::MOI.ScalarAffineFunction,
    change::MOI.ScalarConstantChange,
)
    f.constant = change.new_constant
    return f
end

function modify_function!(
    f::MOI.VectorAffineFunction,
    change::MOI.VectorConstantChange,
)
    for (i, constant) in enumerate(change.new_constant)
        f.constants[i] = constant
    end
    return f
end

function modify_function!(
    f::MOI.ScalarQuadraticFunction,
    change::MOI.ScalarConstantChange,
)
    f.constant = change.new_constant
    return f
end

function modify_function!(
    f::MOI.VectorQuadraticFunction,
    change::MOI.VectorConstantChange,
)
    for (i, constant) in enumerate(change.new_constant)
        f.constants[i] = constant
    end
    return f
end

function _modify_coefficient(
    terms::Vector{MOI.ScalarAffineTerm{T}},
    variable::MOI.VariableIndex,
    new_coefficient::T,
) where {T}
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
    return
end

function modify_function!(
    f::MOI.ScalarAffineFunction{T},
    change::MOI.ScalarCoefficientChange{T},
) where {T}
    _modify_coefficient(f.terms, change.variable, change.new_coefficient)
    return f
end

function modify_function!(
    f::MOI.ScalarQuadraticFunction{T},
    change::MOI.ScalarCoefficientChange{T},
) where {T}
    _modify_coefficient(f.affine_terms, change.variable, change.new_coefficient)
    return f
end

function modify_function!(
    f::MOI.ScalarQuadraticFunction{T},
    change::MOI.ScalarQuadraticCoefficientChange{T},
) where {T}
    indices = findall(f.quadratic_terms) do term
        x, y = term.variable_1, term.variable_2
        return (x == change.variable_1 && y == change.variable_2) ||
               (y == change.variable_1 && x == change.variable_2)
    end
    for j in reverse(indices)
        deleteat!(f.quadratic_terms, j)
    end
    term = MOI.ScalarQuadraticTerm(
        change.new_coefficient,
        change.variable_1,
        change.variable_2,
    )
    push!(f.quadratic_terms, term)
    return f
end

function _modify_coefficients(
    terms::Vector{MOI.VectorAffineTerm{T}},
    variable::MOI.VariableIndex,
    new_coefficients::Vector{Tuple{Int64,T}},
) where {T}
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
    return
end

function modify_function!(
    f::MOI.VectorAffineFunction{T},
    change::MOI.MultirowChange{T},
) where {T}
    _modify_coefficients(f.terms, change.variable, change.new_coefficients)
    return f
end

function modify_function!(
    f::MOI.VectorQuadraticFunction{T},
    change::MOI.MultirowChange{T},
) where {T}
    _modify_coefficients(
        f.affine_terms,
        change.variable,
        change.new_coefficients,
    )
    return f
end

# Arithmetic

function map_terms!(
    op,
    func::Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction},
)
    map!(op, func.terms, func.terms)
    return
end

function map_terms!(
    op,
    func::Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction},
)
    map!(op, func.affine_terms, func.affine_terms)
    map!(op, func.quadratic_terms, func.quadratic_terms)
    return
end

### Base methods

_eltype(args::Tuple) = _eltype(first(args), Base.tail(args))
_eltype(::Tuple{}) = nothing
_eltype(::MOI.Utilities.TypedScalarLike{T}, tail) where {T} = T
_eltype(::MOI.VariableIndex, tail) = _eltype(tail)
_eltype(::MOI.Utilities.TypedVectorLike{T}, tail) where {T} = T
_eltype(::MOI.VectorOfVariables, tail) = _eltype(tail)

### Base.:+

function Base.:+(
    arg::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
    },
    args::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
    }...,
)
    T = _eltype(arg, args)
    if T === nothing
        error(
            "Unable to add VariableIndex together because no coefficient type " *
            "is specified. Instead of `x + y`, convert one of the terms to a " *
            "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
            "`T` is the coefficient type For example: `1.0 * x + y`.",
        )
    end
    return operate(+, T, arg, args...)
end

function Base.:+(
    f::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
    g::T,
) where {T<:Number}
    return operate(+, T, f, g)
end

function Base.:+(
    f::T,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T<:Number}
    return operate(+, T, f, g)
end

function Base.:+(
    arg::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    },
    args::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    }...,
)
    T = _eltype(arg, args)
    if T === nothing
        error(
            "Cannot add VectorOfVariables together without a coefficient " *
            "type. Convert one argument to a VectorAffineFunction first.",
        )
    end
    return operate(+, T, arg, args...)
end

function Base.:+(
    f::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::AbstractVector{T},
) where {T<:Number}
    return operate(+, T, f, g)
end

function Base.:+(
    f::AbstractVector{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return operate(+, T, f, g)
end

### Base.:-

function Base.:-(
    arg::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
    },
    args::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
    }...,
)
    T = _eltype(arg, args)
    if T === nothing
        error(
            "Unable to subtract VariableIndex together because no coefficient type " *
            "is specified. Instead of `x - y`, convert one of the terms to a " *
            "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
            "`T` is the coefficient type For example: `1.0 * x - y`.",
        )
    end
    return operate(-, T, arg, args...)
end

function Base.:-(
    f::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::T,
) where {T<:Number}
    return operate(-, T, f, g)
end

function Base.:-(
    f::T,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return operate(-, T, f, g)
end

function Base.:-(
    arg::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    },
    args::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    }...,
)
    T = _eltype(arg, args)
    if T === nothing
        error(
            "Cannot subtract VectorOfVariables together without a coefficient " *
            "type. Convert one argument to a VectorAffineFunction first.",
        )
    end
    return operate(-, T, arg, args...)
end

function Base.:-(
    f::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::AbstractVector{T},
) where {T<:Number}
    return operate(-, T, f, g)
end

function Base.:-(
    f::AbstractVector{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return operate(-, T, f, g)
end

### Base.:*

Base.:*(f::MOI.AbstractFunction) = f

function Base.:*(f::ScalarLike, g::ScalarLike, args::ScalarLike...)
    T = _eltype(f, (g, args...))
    if T === nothing
        error(
            "Unable to multiply VariableIndex together because no coefficient " *
            "type is specified. Instead of `x * y`, convert one of the terms to a " *
            "`ScalarAffineFunction` first by left-multiplying by `one(T)` where " *
            "`T` is the coefficient type For example: `1.0 * x * y`.",
        )
    end
    return operate(*, T, f, g, args...)
end

function Base.:*(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}, g::Number)
    return operate(*, typeof(g), f, g)
end

function Base.:*(
    f::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::T,
) where {T<:Number}
    return operate_coefficients(Base.Fix2(*, g), f)
end

function Base.:*(
    f::T,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return g * f
end

# Used by sparse matrix multiplication after
# https://github.com/JuliaLang/julia/pull/33205
function Base.:*(
    f::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::Bool,
) where {T<:Number}
    if g
        return MA.copy_if_mutable(f)
    else
        return zero(typeof(f))
    end
end

# These method are needed for multiplication between different number types. The
# most common case is when the number is a `Complex{T}`.

function Base.:*(
    f::Union{
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    },
    g::Number,
)
    return operate_coefficients(Base.Fix2(*, g), f)
end

function Base.:*(
    f::Number,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
    },
)
    return g * f
end

### Base.:/

function Base.:/(
    f::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::T,
) where {T<:Number}
    return operate(/, T, f, g)
end

function Base.:/(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}, g::Number)
    return operate(/, typeof(g), f, g)
end

### Base.:^

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

### LinearAlgebra

LinearAlgebra.dot(f::ScalarLike, g::ScalarLike) = f * g
LinearAlgebra.dot(α::T, func::TypedLike{T}) where {T<:Number} = α * func
LinearAlgebra.dot(func::TypedLike{T}, α::T) where {T<:Number} = func * α
LinearAlgebra.adjoint(f::ScalarLike) = f
LinearAlgebra.transpose(f::ScalarLike) = f
LinearAlgebra.symmetric_type(::Type{F}) where {F<:ScalarLike} = F
LinearAlgebra.symmetric(f::ScalarLike, ::Symbol) = f

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

number_of_affine_terms(::Type{T}, ::Union{T,AbstractVector{T}}) where {T} = 0

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
        AbstractVector{T},
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
    ::Union{T,AbstractVector{T}},
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
        AbstractVector{T},
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

output_dim(::Type{T}, x::AbstractVector{T}) where {T} = length(x)

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
    constant::Vector{T},
    offset::Int,
    ::Int,
    func::AbstractVector{T},
) where {T}
    for (i, fi) in enumerate(func)
        constant[offset+i] = fi
    end
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
    vectorize(x::AbstractVector{<:Number})

Returns `x`.
"""
vectorize(x::AbstractVector{<:Number}) = x

"""
    vectorize(x::AbstractVector{MOI.VariableIndex})

Returns the vector of scalar affine functions in the form of a
`MOI.VectorAffineFunction{T}`.
"""
function vectorize(x::AbstractVector{MOI.VariableIndex})
    # Explicitly construct the output vector here because don't know that `x`
    # has a `convert` method to `Vector`.
    y = Vector{MOI.VariableIndex}(undef, length(x))
    for (i, xi) in enumerate(x)
        y[i] = xi
    end
    return MOI.VectorOfVariables(y)
end

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

function vectorize(x::AbstractVector{MOI.ScalarNonlinearFunction})
    # Explicitly construct the output vector here because don't know that `x`
    # has a `convert` method to `Vector`.
    y = Vector{MOI.ScalarNonlinearFunction}(undef, length(x))
    for (i, xi) in enumerate(x)
        y[i] = xi
    end
    return MOI.VectorNonlinearFunction(y)
end

scalarize(f::AbstractVector, ::Bool = false) = f

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

function scalarize(
    f::MOI.VectorNonlinearFunction,
    ignore_constants::Bool = false,
)
    return f.rows
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

convert_approx(::Type{F}, func::T; kws...) where {F,T} = convert(F, func)

function convert_approx(
    ::Type{MOI.VariableIndex},
    func::MOI.ScalarAffineFunction{T};
    tol = tol_default(T),
) where {T}
    f = canonical(func)
    i = findfirst(t -> isapprox(t.coefficient, one(T), atol = tol), f.terms)
    if abs(f.constant) > tol ||
       i === nothing ||
       any(j -> j != i && abs(f.terms[j].coefficient) > tol, eachindex(f.terms))
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

function Base.promote_rule(
    F::Type{MOI.ScalarAffineFunction{T}},
    ::Type{MOI.VariableIndex},
) where {T}
    return F
end

function Base.promote_rule(
    F::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{MOI.VariableIndex},
) where {T}
    return F
end

function is_coefficient_type(
    ::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
    ::Type,
)
    return true
end

is_coefficient_type(::Type{<:TypedLike{T}}, ::Type{T}) where {T} = true

is_coefficient_type(::Type{<:TypedLike}, ::Type) = false

is_coefficient_type(::Type{<:MOI.ScalarNonlinearFunction}, ::Type) = true

is_coefficient_type(::Type{MOI.VectorNonlinearFunction}, ::Type) = true

similar_type(::Type{F}, ::Type{T}) where {F,T} = F

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

Base.imag(f::TypedLike) = operate_coefficients(imag, f)

Base.conj(f::TypedLike) = operate_coefficients(conj, f)

function MA.promote_operation(
    ::typeof(conj),
    T::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
)
    return T
end

Base.conj(f::Union{MOI.VariableIndex,MOI.VectorOfVariables}) = f

"""
    eval_term(value_fn::Function, t)

!!! warning
    This method is deprecated any may be removed in MathOptInterface v2.0. Use
    `eval_variables` instead.
"""
eval_term(value_fn::Function, t) = eval_variables(value_fn, t)

"""
    constant_vector(f)

!!! warning
    This method is deprecated and may be removed in MathOptInterface v2.0.
"""
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

"""
    is_complex(::Type{<:MOI.AbstractFunction})

Return `true` if the function type may return `<:Complex` values. It returns
`false` for real-valued functions.

This function defaults to returning a false negatives (`false`). If your new
function explicitly returns complex values, opt-in by defining a new method.

This function is mostly intended for use in the `MOI.Bridges` submodule to
identify when bridges are not applicable (because the function is
complex-valued).

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> MOI.Utilities.is_complex(MOI.VariableIndex)
false

julia> MOI.Utilities.is_complex(MOI.ScalarAffineFunction{Complex{Int}})
true

julia> MOI.Utilities.is_complex(MOI.ScalarNonlinearFunction)
false
```
"""
is_complex(::Type{<:MOI.AbstractFunction}) = false
is_complex(::Type{<:MOI.ScalarAffineFunction{<:Complex}}) = true
is_complex(::Type{<:MOI.VectorAffineFunction{<:Complex}}) = true
is_complex(::Type{<:MOI.ScalarQuadraticFunction{<:Complex}}) = true
is_complex(::Type{<:MOI.VectorQuadraticFunction{<:Complex}}) = true
