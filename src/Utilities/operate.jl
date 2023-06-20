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

### 1a: operate(::typeof(+), ::Type{T}, ::F1)

operate(::typeof(+), ::Type{T}, f::MOI.AbstractFunction) where {T} = f
operate(::typeof(+), ::Type{T}, f::T) where {T<:Number} = f

### 1b: operate(::typeof(+), ::Type{T}, ::F1, ::F2)

function operate(
    ::typeof(+),
    ::Type{T},
    f::T,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T}
    return operate(+, T, g, f)
end

function operate(::typeof(+), ::Type{T}, f::MOI.VariableIndex, g::T) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], g)
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
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        T,
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
    f::AbstractVector{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Vector{T},
) where {T}
    d = MOI.output_dimension(f)
    @assert length(g) == d
    scalar_terms = MOI.ScalarAffineTerm.(one(T), f.variables)
    vector_terms = MOI.VectorAffineTerm.(1:d, scalar_terms)
    return MOI.VectorAffineFunction{T}(vector_terms, g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(f)
    @assert MOI.output_dimension(g) == d
    fs = MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(one(T), f.variables))
    gs = MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(one(T), g.variables))
    return MOI.VectorAffineFunction{T}(vcat(fs, gs), zeros(T, d))
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
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
    },
) where {T}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T}
    return operate!(+, T, copy(f), g)
end

### 2a: operate(::typeof(-), ::Type{T}, ::F)

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

### 2b: operate(::typeof(-), ::Type{T}, ::F1, ::F2)

function operate(
    ::typeof(-),
    ::Type{T},
    f::T,
    g::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(::typeof(-), ::Type{T}, f::MOI.VariableIndex, g::T) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], -g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(one(T), f), MOI.ScalarAffineTerm(-one(T), g)],
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
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T}
    return MOI.ScalarQuadraticFunction(
        operate_terms(-, g.quadratic_terms),
        vcat(f.terms, operate_terms(-, g.affine_terms)),
        f.constant .- g.constant,
    )
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::ScalarQuadraticLike{T},
) where {T}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::AbstractVector{T},
    g::VectorLike{T},
) where {T}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::AbstractVector{T},
) where {T}
    d = MOI.output_dimension(f)
    @assert length(g) == d
    scalar_terms = MOI.ScalarAffineTerm.(one(T), f.variables)
    vector_terms = MOI.VectorAffineTerm.(1:d, scalar_terms)
    return MOI.VectorAffineFunction{T}(vector_terms, -g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(f)
    @assert MOI.output_dimension(g) == d
    fs = MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(one(T), f.variables))
    gs = MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(-one(T), g.variables))
    return MOI.VectorAffineFunction{T}(vcat(fs, gs), zeros(T, d))
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
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
    },
) where {T}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T}
    return MOI.VectorQuadraticFunction(
        operate_terms(-, g.quadratic_terms),
        [f.terms; operate_terms(-, g.affine_terms)],
        f.constants .- g.constants,
    )
end

function operate(
    op::typeof(-),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T}
    return operate!(op, T, copy(f), g)
end

### 3a: operate(::typeof(*), ::Type{T}, ::T, ::F)

function operate(::typeof(*), ::Type{T}, f::T, g::MOI.VariableIndex) where {T}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(f, g)], zero(T))
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::MOI.VectorOfVariables,
) where {T}
    d = MOI.output_dimension(g)
    terms = MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(f, g.variables))
    return MOI.VectorAffineFunction{T}(terms, zeros(T, d))
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T}
    return operate!(*, T, copy(g), f)
end

### 3b: operate(::typeof(*), ::Type{T}, ::F,  ::T)

function operate(
    ::typeof(*),
    ::Type{T},
    f::Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::T,
) where {T}
    return operate(*, T, g, f)
end

### 3c: operate(::typeof(*), ::Type{T}, ::F1, ::F2)

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

### 3d: operate(::typeof(*), ::Type{T}, ::Diagonal{T}, ::F)

function operate(
    ::typeof(*),
    ::Type{T},
    D::Diagonal{T},
    func::MOI.VectorQuadraticFunction{T},
) where {T}
    return MOI.VectorQuadraticFunction{T}(
        operate_terms(*, D, func.quadratic_terms),
        operate_terms(*, D, func.affine_terms),
        operate(*, T, D, func.constants),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    D::Diagonal{T},
    func::MOI.VectorAffineFunction{T},
) where {T}
    return MOI.VectorAffineFunction{T}(
        operate_terms(*, D, func.terms),
        operate(*, T, D, func.constants),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    D::Diagonal{T},
    func::MOI.VectorOfVariables,
) where {T}
    return MOI.VectorAffineFunction{T}(
        MOI.VectorAffineTerm{T}[
            MOI.VectorAffineTerm(
                i,
                MOI.ScalarAffineTerm(D.diag[i], func.variables[i]),
            ) for i in eachindex(func.variables)
        ],
        zeros(T, length(func.variables)),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    D::Diagonal{T},
    v::AbstractVector{T},
) where {T}
    return T[D.diag[i] * v[i] for i in eachindex(v)]
end

### 4a: `operate(::typeof(/), ::Type{T}, ::F, ::T)

function operate(::typeof(/), ::Type{T}, f::MOI.VariableIndex, α::T) where {T}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(inv(α), f)],
        zero(T),
    )
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

### 6a: operate(::typeof(imag), ::Type{T}, ::F)

function operate(::typeof(imag), ::Type{T}, ::MOI.VariableIndex) where {T}
    return zero(MOI.ScalarAffineFunction{T})
end

function operate(::typeof(imag), ::Type{T}, f::MOI.VectorOfVariables) where {T}
    return zero_with_output_dimension(
        MOI.VectorAffineFunction{T},
        MOI.output_dimension(f),
    )
end

operate(::typeof(imag), ::Type, f::TypedLike) = imag(f)

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
operate!(op, ::Type{T}, args...) where {T} = operate(op, T, args...)

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
