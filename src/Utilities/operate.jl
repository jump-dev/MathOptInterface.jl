# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    operate(
        op::Function,
        ::Type{T},
        args::Union{T,MOI.AbstractFunction}...,
    )::MOI.AbstractFunction where {T}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`.

No argument can be modified.

## Methods

 1. `+`
    a. `operate(::typeof(+), ::Type{T}, ::F1)`
    b. `operate(::typeof(+), ::Type{T}, ::F1, ::F2)`
 2. `-`
    a. `operate(::typeof(-), ::Type{T}, ::F)`
    b. `operate(::typeof(-), ::Type{T}, ::F1, ::F2)`
 3. `*`
    a. `operate(::typeof(*), ::Type{T}, ::T, ::F)`
    b. `operate(::typeof(*), ::Type{T}, ::F, ::T)`
    c. `operate(::typeof(*), ::Type{T}, ::F1, ::F2)`
       where `F1` and `F2` are `VariableIndex` or `ScalarAffineFunction`
    d. `operate(::typeof(*), ::Type{T}, ::Diagonal{T}, ::F)`
 4. `/`
    a. `operate(::typeof(/), ::Type{T}, ::F, ::T)`
 5. `vcat`
    a. `operate(::typeof(vcat), ::Type{T}, ::F...)`
 6. `imag`
    a. `operate(::typeof(imag), ::Type{T}, ::F)`
       where `F` is `VariableIndex` or `VectorOfVariables`

One assumption is that the element type `T` is invariant under each operation.
That is, `op(::T, ::T)::T` where `op` is a `+`, `-`, `*`, and `/`.

In each case, `F` (or `F1` and `F2`) is one of the nine supported types, with
a restriction that the mathematical operation makes sense, for example, we don't
define `promote_operation(-, T, F1, F2)` where `F1` is a scalar-valued function
and  `F2` is a vector-valued function. The nine supported types are:

 1. ::T
 2. ::VariableIndex
 3. ::ScalarAffineFunction{T}
 4. ::ScalarQuadraticFunction{T}
 5. ::ScalarNonlinearFunction
 6. ::AbstractVector{T}
 7. ::VectorOfVariables
 8. ::VectorAffineFunction{T}
 9. ::VectorQuadraticFunction{T}
"""
function operate end

### Special case: a generic fallback for operations on numbers only.

function operate(
    op::F,
    ::Type{T},
    args::Union{T,AbstractVector{T}}...,
) where {T<:Number,F<:Function}
    return op(args...)
end

### 1a: unary addition

operate(::typeof(+), ::Type{T}, f::MOI.AbstractFunction) where {T} = f

### 1b: binary addition

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    α::T,
) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], α)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(one(T), f), MOI.ScalarAffineTerm(one(T), g)],
        zero(T),
    )
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    g::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    return MOI.ScalarQuadraticFunction(
        g.quadratic_terms,
        vcat(f.terms, g.affine_terms),
        f.constant + g.constant,
    )
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    α::Vector{T},
    f::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T}
    return operate(+, T, f, α)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    α::Vector{T},
) where {T}
    d = MOI.output_dimension(f)
    @assert length(α) == d
    return MOI.VectorAffineFunction{T}(
        MOI.VectorAffineTerm.(
            1:d,
            MOI.ScalarAffineTerm.(one(T), f.variables),
        ),
        α,
    )
end

function operate(
    ::typeof(+),
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
                MOI.ScalarAffineTerm.(one(T), g.variables),
            ),
        ),
        fill(zero(T), d),
    )
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::VectorAffineLike{T},
) where {T}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    return MOI.VectorQuadraticFunction(
        g.quadratic_terms,
        vcat(f.terms, g.affine_terms),
        f.constants .+ g.constants,
    )
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::VectorQuadraticLike{T},
) where {T}
    return operate!(+, T, copy(f), g)
end

### 2a: unary subtraction

function operate(::typeof(-), ::Type{T}, f::MOI.VariableIndex) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(-one(T), f)],
        zero(T),
    )
end

function operate(
    op::Union{typeof(-)},
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    return MOI.ScalarAffineFunction(operate_terms(op, f.terms), op(f.constant))
end

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

### 2b: binary subtraction

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    α::T,
) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], -α)
end

function operate(
    op::typeof(-),
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
    ::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    g::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T}
    return operate!(op, T, copy(f), g)
end

function operate(
    op::typeof(-),
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

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::ScalarQuadraticLike{T},
) where {T}
    return operate!(op, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    α::Vector{T},
    f::VectorLike{T},
) where {T}
    return operate!(+, T, operate(-, T, f), α)
end

function operate(
    op::typeof(-),
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
    op::typeof(-),
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
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
    },
) where {T}
    return operate!(op, T, copy(f), g)
end

function operate(
    op::typeof(-),
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

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    }
) where {T}
    return operate!(op, T, copy(f), g)
end

"""
    operate!(
        op::Function,
        ::Type{T},
        args::Union{T,MOI.AbstractFunction}...,
    )::MOI.AbstractFunction where {T}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`.

The first argument may be modified, in which case the return value is identical
to the first argument. For operations which cannot be implemented in-place, this
function returns a new object.
"""
operate!(op, ::Type{T}, args...) where {T} = operate(io, T, args...)

operate!(::typeof(+), ::Type{T}, f::MOI.AbstractFunction) where {T} = f

function operate!(
    op::typeof(-),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T}
    return MA.operate!(-, f)
end


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
