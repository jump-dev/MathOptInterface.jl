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
    )::MOI.AbstractFunction where {T<:Number}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`.

No argument can be modified.

## Methods

 1. `+`
    a. `operate(::typeof(+), ::Type{T}, ::F1)`
    b. `operate(::typeof(+), ::Type{T}, ::F1, ::F2)`
    c. `operate(::typeof(+), ::Type{T}, ::F1...)`
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

In each case, `F` (or `F1` and `F2`) is one of the ten supported types, with
a restriction that the mathematical operation makes sense, for example, we don't
define `promote_operation(-, T, F1, F2)` where `F1` is a scalar-valued function
and  `F2` is a vector-valued function. The ten supported types are:

 1. ::T
 2. ::VariableIndex
 3. ::ScalarAffineFunction{T}
 4. ::ScalarQuadraticFunction{T}
 5. ::ScalarNonlinearFunction
 6. ::AbstractVector{T}
 7. ::VectorOfVariables
 8. ::VectorAffineFunction{T}
 9. ::VectorQuadraticFunction{T}
10. ::VectorNonlinearFunction
"""
function operate end

### Special case: a generic fallback for operations on numbers only.

function operate(
    op::F,
    ::Type{T},
    f::Union{T,AbstractVector{T}},
) where {T<:Number,F<:Function}
    return op(f)
end

function operate(
    op::F,
    ::Type{T},
    f::Union{T,AbstractVector{T}},
    g::Union{T,AbstractVector{T}},
) where {T<:Number,F<:Function}
    return op(f, g)
end

### 1a: operate(::typeof(+), ::Type{T}, ::F1)

operate(::typeof(+), ::Type{T}, f::MOI.AbstractFunction) where {T<:Number} = f

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
) where {T<:Number}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    g::T,
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T<:Number}
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
) where {T<:Number}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T<:Number}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T<:Number}
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
) where {T<:Number}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:+, Any[f, g])
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
    g::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:+, Any[f, g])
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:+, Any[f, g])
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
) where {T<:Number}
    return operate(+, T, g, f)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::Vector{T},
) where {T<:Number}
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
) where {T<:Number}
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
) where {T<:Number}
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
) where {T<:Number}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T<:Number}
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
) where {T<:Number}
    return operate!(+, T, copy(f), g)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorNonlinearFunction,
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
) where {T<:Number}
    args = Any[
        operate(+, T, fi, gi) for (fi, gi) in zip(scalarize(f), scalarize(g))
    ]
    return MOI.VectorNonlinearFunction(args)
end

function operate(
    ::typeof(+),
    ::Type{T},
    f::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::MOI.VectorNonlinearFunction,
) where {T<:Number}
    args = Any[
        operate(+, T, fi, gi) for (fi, gi) in zip(scalarize(f), scalarize(g))
    ]
    return MOI.VectorNonlinearFunction(args)
end

### 1c: operate(+, T, args...)

function operate(::typeof(+), ::Type{T}, f, g, h, args...) where {T<:Number}
    return operate!(+, T, operate(+, T, f, g), h, args...)
end

### 2a: operate(::typeof(-), ::Type{T}, ::F)

function operate(::typeof(-), ::Type{T}, f::MOI.VariableIndex) where {T<:Number}
    return MOI.ScalarAffineFunction{T}(
        [MOI.ScalarAffineTerm(-one(T), f)],
        zero(T),
    )
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
) where {T<:Number}
    d = MOI.output_dimension(f)
    return MOI.VectorAffineFunction{T}(
        MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(-one(T), f.variables)),
        zeros(T, d),
    )
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return operate_coefficients(-, f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    if f.head == :- && length(f.args) == 1
        # A simplification for -(-(f)) into f, but only if f is an SNF.
        if f.args[1] isa MOI.ScalarNonlinearFunction
            return f.args[1]
        end
    end
    return MOI.ScalarNonlinearFunction(:-, Any[f])
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorNonlinearFunction,
) where {T<:Number}
    return MOI.VectorNonlinearFunction(Any[operate(-, T, fi) for fi in f.rows])
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
) where {T<:Number}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    g::T,
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f)], -g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T<:Number}
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
) where {T<:Number}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T<:Number}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T<:Number}
    return MOI.ScalarQuadraticFunction{T}(
        operate_terms(-, g.quadratic_terms),
        vcat(f.terms, operate_terms(-, g.affine_terms)),
        f.constant .- g.constant,
    )
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T<:Number}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:-, Any[f, g])
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
    g::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:-, Any[f, g])
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:-, Any[f, g])
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::AbstractVector{T},
    g::Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
) where {T<:Number}
    return operate!(+, T, operate(-, T, g), f)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorOfVariables,
    g::AbstractVector{T},
) where {T<:Number}
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
) where {T<:Number}
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
) where {T<:Number}
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
) where {T<:Number}
    return operate!(-, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T<:Number}
    return MOI.VectorQuadraticFunction{T}(
        operate_terms(-, g.quadratic_terms),
        MOI.VectorAffineTerm{T}[f.terms; operate_terms(-, g.affine_terms)],
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
) where {T<:Number}
    return operate!(op, T, copy(f), g)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorNonlinearFunction,
    g::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
) where {T<:Number}
    args = Any[
        operate(-, T, fi, gi) for (fi, gi) in zip(scalarize(f), scalarize(g))
    ]
    return MOI.VectorNonlinearFunction(args)
end

function operate(
    ::typeof(-),
    ::Type{T},
    f::Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::MOI.VectorNonlinearFunction,
) where {T<:Number}
    args = Any[
        operate(-, T, fi, gi) for (fi, gi) in zip(scalarize(f), scalarize(g))
    ]
    return MOI.VectorNonlinearFunction(args)
end

### 3a: operate(::typeof(*), ::Type{T}, ::T, ::F)

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::MOI.VariableIndex,
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(f, g)], zero(T))
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::MOI.VectorOfVariables,
) where {T<:Number}
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
) where {T<:Number}
    return operate!(*, T, copy(g), f)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::MOI.ScalarNonlinearFunction,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:*, Any[f, g])
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::T,
    g::MOI.VectorNonlinearFunction,
) where {T<:Number}
    return MOI.VectorNonlinearFunction(Any[operate(*, T, f, h) for h in g.rows])
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
) where {T<:Number}
    return operate(*, T, g, f)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::T,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:*, Any[f, g])
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.VectorNonlinearFunction,
    g::T,
) where {T<:Number}
    return MOI.VectorNonlinearFunction(Any[operate(*, T, h, g) for h in f.rows])
end

### 3c: operate(::typeof(*), ::Type{T}, ::F1, ::F2)

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.ScalarAffineFunction{T},
) where {T<:Number}
    return operate(*, T, g, f)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T<:Number}
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
    return MOI.ScalarQuadraticFunction{T}(quad_terms, aff_terms, constant)
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.VariableIndex,
    g::MOI.VariableIndex,
) where {T<:Number}
    return MOI.ScalarQuadraticFunction{T}(
        [MOI.ScalarQuadraticTerm{T}(f == g ? T(2) : one(T), f, g)],
        MOI.ScalarAffineTerm{T}[],
        zero(T),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::MOI.VariableIndex,
) where {T<:Number}
    aff_terms = MOI.ScalarAffineTerm{T}[]
    if !iszero(f.constant)
        push!(aff_terms, MOI.ScalarAffineTerm{T}(f.constant, g))
    end
    quad_terms = map(f.terms) do t
        scale = t.variable == g ? T(2) : one(T)
        return MOI.ScalarQuadraticTerm{T}(scale * t.coefficient, t.variable, g)
    end
    return MOI.ScalarQuadraticFunction{T}(quad_terms, aff_terms, zero(T))
end

### 3d: operate(::typeof(*), ::Type{T}, ::Diagonal{T}, ::F)

function operate(
    ::typeof(*),
    ::Type{T},
    D::LinearAlgebra.Diagonal{T},
    func::MOI.VectorQuadraticFunction{T},
) where {T<:Number}
    return MOI.VectorQuadraticFunction{T}(
        operate_terms(*, D, func.quadratic_terms),
        operate_terms(*, D, func.affine_terms),
        operate(*, T, D, func.constants),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    D::LinearAlgebra.Diagonal{T},
    func::MOI.VectorAffineFunction{T},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}(
        operate_terms(*, D, func.terms),
        operate(*, T, D, func.constants),
    )
end

function operate(
    ::typeof(*),
    ::Type{T},
    D::LinearAlgebra.Diagonal{T},
    func::MOI.VectorOfVariables,
) where {T<:Number}
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
    D::LinearAlgebra.Diagonal{T},
    v::AbstractVector{T},
) where {T<:Number}
    return T[D.diag[i] * v[i] for i in eachindex(v)]
end

### 4a: `operate(::typeof(/), ::Type{T}, ::F, ::T)

function operate(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.VariableIndex,MOI.VectorOfVariables},
    g::T,
) where {T<:Number}
    return operate(*, T, inv(g), f)
end

function operate(
    ::typeof(/),
    ::Type{T},
    f::Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
    g::T,
) where {T<:Number}
    return operate!(/, T, copy(f), g)
end

function operate(
    ::typeof(/),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::T,
) where {T<:Number}
    return MOI.ScalarNonlinearFunction(:/, Any[f, g])
end

function operate(
    ::typeof(/),
    ::Type{T},
    f::MOI.VectorNonlinearFunction,
    g::T,
) where {T<:Number}
    return MOI.VectorNonlinearFunction(Any[operate(/, T, h, g) for h in f.rows])
end

### 5a: operate(::typeof(vcat), ::Type{T}, ::F...)

operate(::typeof(vcat), ::Type{T}) where {T<:Number} = T[]

function operate(
    ::typeof(vcat),
    ::Type{T},
    f::Union{T,AbstractVector{T}}...,
) where {T<:Number}
    return vcat(f...)
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    f::Union{MOI.VariableIndex,MOI.VectorOfVariables}...,
) where {T<:Number}
    x = Vector{MOI.VariableIndex}(undef, sum(f -> output_dim(T, f), f))
    fill_vector(x, T, 0, 0, fill_variables, output_dim, f...)
    return MOI.VectorOfVariables(x)
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    f::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
    }...,
) where {T<:Number}
    num_aterms = sum(f -> number_of_affine_terms(T, f), f)
    aterms = Vector{MOI.VectorAffineTerm{T}}(undef, num_aterms)
    fill_vector(aterms, T, 0, 0, fill_terms, number_of_affine_terms, f...)
    constants = zeros(T, sum(f -> output_dim(T, f), f))
    fill_vector(constants, T, 0, 0, fill_constant, output_dim, f...)
    return MOI.VectorAffineFunction{T}(aterms, constants)
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    f::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    }...,
) where {T<:Number}
    num_aterms = sum(func -> number_of_affine_terms(T, func), f)
    aterms = Vector{MOI.VectorAffineTerm{T}}(undef, num_aterms)
    fill_vector(aterms, T, 0, 0, fill_terms, number_of_affine_terms, f...)
    num_qterms = sum(func -> number_of_quadratic_terms(T, func), f)
    qterms = Vector{MOI.VectorQuadraticTerm{T}}(undef, num_qterms)
    fill_vector(qterms, T, 0, 0, fill_terms, number_of_quadratic_terms, f...)
    constants = zeros(T, sum(f -> output_dim(T, f), f))
    fill_vector(constants, T, 0, 0, fill_constant, output_dim, f...)
    return MOI.VectorQuadraticFunction{T}(qterms, aterms, constants)
end

function operate(
    ::typeof(vcat),
    ::Type{T},
    args::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    }...,
) where {T<:Number}
    out = Any[]
    for a in args
        if a isa T
            push!(out, a)
        elseif a isa AbstractVector{T}
            append!(out, a)
        elseif a isa MOI.AbstractScalarFunction
            push!(out, a)
        else
            append!(out, scalarize(a))
        end
    end
    _to_new_snf(f::MOI.ScalarNonlinearFunction) = copy(f)
    _to_new_snf(f) = convert(MOI.ScalarNonlinearFunction, f)
    # We need to `copy` the ScalarNonlinearFunction rows here, everything else
    # will be `convert(ScalarNonlinearFunction, row)` into a new object, but the
    # ScalarNonlinearFunction rows won't be, so mutating the return value of
    # this `operate` method will mutate the inputs. This _is_ what Base.vcat
    # does, but it doesn't fit with the general assumption that
    # `Utilities.operate(` returns a new object.
    return MOI.VectorNonlinearFunction(_to_new_snf.(out))
end

### 6a: operate(::typeof(imag), ::Type{T}, ::F)

function operate(
    ::typeof(imag),
    ::Type{T},
    ::MOI.VariableIndex,
) where {T<:Number}
    return zero(MOI.ScalarAffineFunction{T})
end

function operate(
    ::typeof(imag),
    ::Type{T},
    f::MOI.VectorOfVariables,
) where {T<:Number}
    return zero_with_output_dimension(
        MOI.VectorAffineFunction{T},
        MOI.output_dimension(f),
    )
end

function operate(
    ::typeof(imag),
    ::Type{T},
    f::Union{
        MOI.ScalarAffineFunction{S},
        MOI.ScalarQuadraticFunction{S},
        MOI.VectorAffineFunction{S},
        MOI.VectorQuadraticFunction{S},
    },
) where {T,S<:Union{T,Complex{T}}}
    return imag(f)
end

"""
    operate!(
        op::Function,
        ::Type{T},
        args::Union{T,MOI.AbstractFunction}...,
    )::MOI.AbstractFunction where {T<:Number}

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)` on functions of coefficient type `T`.

The first argument may be modified, in which case the return value is identical
to the first argument. For operations which cannot be implemented in-place, this
function returns a new object.
"""
operate!(op, ::Type{T}, args...) where {T<:Number} = operate(op, T, args...)

### 1a: operate!(::typeof(+), ::Type{T}, ::F1)

### 1b: operate!(::typeof(+), ::Type{T}, ::F1, ::F2)

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T<:Number}
    return MA.operate!(+, f, g)
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T<:Number}
    return MA.operate!(+, f, g)
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
) where {T<:Number}
    if f.head == :+
        push!(f.args, g)
        return f
    end
    return operate(+, T, f, g)
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    g::AbstractVector{T},
) where {T<:Number}
    @assert MOI.output_dimension(f) == length(g)
    f.constants .+= g
    return f
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorOfVariables,
) where {T<:Number}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.terms,
        MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(one(T), g.variables)),
    )
    return f
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T<:Number}
    append!(f.terms, g.terms)
    f.constants .+= g.constants
    return f
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorOfVariables,
) where {T<:Number}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(one(T), g.variables)),
    )
    return f
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T<:Number}
    append!(f.affine_terms, g.terms)
    f.constants .+= g.constants
    return f
end

function operate!(
    ::typeof(+),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T<:Number}
    append!(f.affine_terms, g.affine_terms)
    append!(f.quadratic_terms, g.quadratic_terms)
    f.constants .+= g.constants
    return f
end

### 1c: operate!(+, T, args...)

function operate!(::typeof(+), ::Type{T}, f, g, h, args...) where {T<:Number}
    # In order to improve performance and avoid a `StackOverflow` if there are
    # too many arguments, `foldl` does not do recursion and fall back to a
    # `for` loop if there are too many arguments.
    return foldl((x, y) -> operate!(+, T, x, y), (f, g, h, args...))
end

### 2a: operate!(::typeof(-), ::Type{T}, ::F)

function operate!(
    op::typeof(-),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
) where {T<:Number}
    return MA.operate!(-, f)
end

### 2b: operate!(::typeof(-), ::Type{T}, ::F1, ::F2)

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarAffineFunction{T},
    g::Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
) where {T<:Number}
    return MA.operate!(-, f, g)
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.ScalarQuadraticFunction{T},
    g::Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
) where {T<:Number}
    return MA.operate!(-, f, g)
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    g::AbstractVector{T},
) where {T<:Number}
    @assert MOI.output_dimension(f) == length(g)
    f.constants .-= g
    return f
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorOfVariables,
) where {T<:Number}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.terms,
        MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(-one(T), g.variables)),
    )
    return f
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T<:Number}
    append!(f.terms, operate_terms(-, g.terms))
    f.constants .-= g.constants
    return f
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorOfVariables,
) where {T<:Number}
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(1:d, MOI.ScalarAffineTerm.(-one(T), g.variables)),
    )
    return f
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorAffineFunction{T},
) where {T<:Number}
    append!(f.affine_terms, operate_terms(-, g.terms))
    f.constants .-= g.constants
    return f
end

function operate!(
    ::typeof(-),
    ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VectorQuadraticFunction{T},
) where {T<:Number}
    append!(f.affine_terms, operate_terms(-, g.affine_terms))
    append!(f.quadratic_terms, operate_terms(-, g.quadratic_terms))
    f.constants .-= g.constants
    return f
end

### 3a: operate!(::typeof(*), ::Type{T}, ::T, ::F)

### 3b: operate!(::typeof(*), ::Type{T}, ::F, ::T)

function operate!(
    ::typeof(*),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    g::T,
) where {T<:Number}
    map_terms!(term -> operate_term(*, term, g), f)
    f.constant *= g
    return f
end

function operate!(
    ::typeof(*),
    ::Type{T},
    f::MOI.ScalarNonlinearFunction,
    g::T,
) where {T<:Number}
    if f.head == :*
        push!(f.args, g)
        return f
    end
    return operate(*, T, f, g)
end

function operate!(
    ::typeof(*),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    g::T,
) where {T<:Number}
    map_terms!(term -> operate_term(*, term, g), f)
    f.constants .*= g
    return f
end

### 3c: operate!(::typeof(*), ::Type{T}, ::F1, ::F2)

### 3d: operate!(::typeof(*), ::Type{T}, ::Diagonal{T}, ::F)

### 4a: operate!(::typeof(/), ::Type{T}, ::F, ::T)

function operate!(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    g::T,
) where {T<:Number}
    map_terms!(term -> operate_term(/, term, g), f)
    f.constant /= g
    return f
end

function operate!(
    ::typeof(/),
    ::Type{T},
    f::Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
    g::T,
) where {T<:Number}
    map_terms!(term -> operate_term(/, term, g), f)
    f.constants ./= g
    return f
end

### 5a: operate!(::typeof(vcat), ::Type{T}, ::F...)

### 6a: operate!(::typeof(imag), ::Type{T}, ::F)

"""
    operate_term(op::Function, args...)

Compute `op(args...)`, where at least one element of `args` is one of
[`MOI.ScalarAffineTerm`](@ref), [`MOI.ScalarQuadraticTerm`](@ref),
[`MOI.VectorAffineTerm`](@ref), or [`MOI.VectorQuadraticTerm`](@ref).

## Methods

 1. `+`
    a. `operate_term(::typeof(+), ::Term)`
 2. `-`
    a. `operate_term(::typeof(-), ::Term)`
 3. `*`
    a. `operate_term(::typeof(*), ::T, ::Term)`
    b. `operate_term(::typeof(*), ::Term, ::T)`
    c. `operate_term(::typeof(*), ::AffineTerm, ::AffineTerm)`
    d. `operate_term(::typeof(*), ::T, ::ScalarTerm, ::T)`
    e. `operate_term(::typeof(*), ::Diagonal, ::VectorTerm)`
 4. `/`
    a. `operate_term(::typeof(/), ::Term, ::T)`
 5. User-defined function
    a. `operate_term(::Function, ::Term)`
"""
function operate_term end

### 1a: operate_term(::typeof(+), ::Term)

# Works automatically via 5a fallback.

### 2a: operate_term(::typeof(-), ::Term)

# Works automatically via 5a fallback.

### 3a: operate_term(::typeof(*), ::T, ::Term)

function operate_term(
    ::typeof(*),
    f::T,
    g::Union{
        MOI.ScalarAffineTerm{T},
        MOI.ScalarQuadraticTerm{T},
        MOI.VectorAffineTerm{T},
        MOI.VectorQuadraticTerm{T},
    },
) where {T<:Number}
    return operate_term(Base.Fix1(*, f), g)
end

### 3b: operate_term(::typeof(*), ::Term, ::T)

function operate_term(
    ::typeof(*),
    f::Union{
        MOI.ScalarAffineTerm{T},
        MOI.ScalarQuadraticTerm{T},
        MOI.VectorAffineTerm{T},
        MOI.VectorQuadraticTerm{T},
    },
    g::T,
) where {T<:Number}
    return operate_term(*, g, f)
end

### 3c: operate_term(::typeof(*), ::Term, ::Term)

function operate_term(
    ::typeof(*),
    t1::MOI.ScalarAffineTerm,
    t2::MOI.ScalarAffineTerm,
)
    coef = t1.coefficient * t2.coefficient
    if t1.variable == t2.variable
        return MOI.ScalarQuadraticTerm(2 * coef, t1.variable, t2.variable)
    end
    return MOI.ScalarQuadraticTerm(coef, t1.variable, t2.variable)
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

### 3d: operate_term(::typeof(*), ::T, ::ScalarTerm, ::T)

function operate_term(
    ::typeof(*),
    f::T,
    g::Union{MOI.ScalarAffineTerm{T},MOI.ScalarQuadraticTerm{T}},
    h::T,
) where {T<:Number}
    return operate_term(Base.Fix1(*, f * h), g)
end

### 3e: operate_term(::typeof(*), ::Diagonal, ::Vector)

function operate_term(
    ::typeof(*),
    f::LinearAlgebra.Diagonal,
    g::MOI.VectorAffineTerm,
)
    return MOI.VectorAffineTerm(
        g.output_index,
        operate_term(*, f.diag[g.output_index], g.scalar_term),
    )
end

function operate_term(
    ::typeof(*),
    f::LinearAlgebra.Diagonal,
    g::MOI.VectorQuadraticTerm,
)
    return MOI.VectorQuadraticTerm(
        g.output_index,
        operate_term(*, f.diag[g.output_index], g.scalar_term),
    )
end

### 4a: operate_term(::typeof(/), ::Term, ::T)

function operate_term(
    ::typeof(/),
    f::Union{
        MOI.ScalarAffineTerm{T},
        MOI.ScalarQuadraticTerm{T},
        MOI.VectorAffineTerm{T},
        MOI.VectorQuadraticTerm{T},
    },
    g::T,
) where {T<:Number}
    return operate_term(Base.Fix2(/, g), f)
end

### 5a: operate_term(::Function, ::Term)

function operate_term(op::F, f::MOI.ScalarAffineTerm) where {F<:Function}
    return MOI.ScalarAffineTerm(op(f.coefficient), f.variable)
end

function operate_term(op::F, f::MOI.ScalarQuadraticTerm) where {F<:Function}
    coef = op(f.coefficient)
    return MOI.ScalarQuadraticTerm(coef, f.variable_1, f.variable_2)
end

function operate_term(op::F, f::MOI.VectorAffineTerm) where {F<:Function}
    return MOI.VectorAffineTerm(f.output_index, operate_term(op, f.scalar_term))
end

function operate_term(op::F, f::MOI.VectorQuadraticTerm) where {F<:Function}
    return MOI.VectorQuadraticTerm(
        f.output_index,
        operate_term(op, f.scalar_term),
    )
end

"""
    operate_terms(op::Function, args...)

Compute `op(args...)`, where at least one element of `args` is a vector of
[`MOI.ScalarAffineTerm`](@ref), [`MOI.ScalarQuadraticTerm`](@ref),
[`MOI.VectorAffineTerm`](@ref), or [`MOI.VectorQuadraticTerm`](@ref).

## Methods

 1. `+`
    a. `operate_term(::typeof(+), ::Vector{<:Term})`
 2. `-`
    a. `operate_term(::typeof(-), ::Vector{<:Term})`
 3. `*`
    a. `operate_term(::typeof(*), ::Diagonal, ::Vector{<:VectorTerm})`
 4. User-defined function
    a. `operate_term(::Function, ::Vector{<:Term})`
"""
function operate_terms end

### 1a: operate_term(::typeof(+), ::Vector{<:Term})

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

### 2a: operate_term(::typeof(-), ::Vector{<:Term})

# Works automatically by 4a fallback.

### 3a: operate_term(::typeof(*), ::Diagonal, ::Vector{<:VectorTerm})

function operate_terms(::typeof(*), D::LinearAlgebra.Diagonal, terms)
    return map(term -> operate_term(*, D, term), terms)
end

### 4a: operate_term(::Function, ::Vector{<:Term})

function operate_terms(
    op::F,
    f::Vector{
        <:Union{
            MOI.ScalarAffineTerm,
            MOI.ScalarQuadraticTerm,
            MOI.VectorAffineTerm,
            MOI.VectorQuadraticTerm,
        },
    },
) where {F}
    return map(Base.Fix1(operate_term, op), f)
end

"""
    operate_terms!(op::Function, args...)

Compute `op(args...)`, where at least one element of `args` is a vector of
[`MOI.ScalarAffineTerm`](@ref), [`MOI.ScalarQuadraticTerm`](@ref),
[`MOI.VectorAffineTerm`](@ref), or [`MOI.VectorQuadraticTerm`](@ref).

!!! warning
    This method is deprecated and may be removed in MathOptInterface v2.0.

## Methods

 1. `-`
    a. `operate_term(::typeof(-), ::Vector{<:ScalarTerm})`
"""
function operate_terms!(
    ::typeof(-),
    terms::Vector{<:Union{MOI.ScalarAffineTerm,MOI.ScalarQuadraticTerm}},
)
    map!(Base.Fix1(operate_term, -), terms, terms)
    return terms
end

"""
    operate_output_index!(
        op::Union{typeof(+),typeof(-)},
        ::Type{T},
        output_index::Integer,
        f::Union{AbstractVector{T},MOI.AbstractVectorFunction}
        g::Union{T,MOI.AbstractScalarFunction}...
    ) where {T<:Number}

Return an `MOI.AbstractVectorFunction` in which the scalar function in row
`output_index` is the result of `op(f[output_index], g)`.

The functions at output index different to `output_index` are the same as the
functions at the same output index in `func`. The first argument may be
modified.

## Methods

 1. `+`
    a. `operate_output_index!(+, ::Type{T}, ::Int, ::VectorF, ::ScalarF)`
 2. `-`
    a. `operate_output_index!(-, ::Type{T}, ::Int, ::VectorF, ::ScalarF)`
"""
function operate_output_index! end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::AbstractVector{T},
    g::T,
) where {T<:Number}
    f[output_index] = op(f[output_index], g)
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    g::T,
) where {T<:Number}
    f.constants[output_index] = op(f.constants[output_index], g)
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    g::MOI.VariableIndex,
) where {T<:Number}
    push!(
        f.terms,
        MOI.VectorAffineTerm(output_index, MOI.ScalarAffineTerm(op(one(T)), g)),
    )
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorAffineFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T<:Number}
    append!(
        f.terms,
        MOI.VectorAffineTerm.(output_index, operate_terms(op, g.terms)),
    )
    operate_output_index!(op, T, output_index, f, MOI.constant(g))
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::T,
) where {T<:Number}
    f.constants[output_index] = op(f.constants[output_index], g)
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.VariableIndex,
) where {T<:Number}
    push!(
        f.affine_terms,
        MOI.VectorAffineTerm(output_index, MOI.ScalarAffineTerm(op(one(T)), g)),
    )
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.ScalarAffineFunction{T},
) where {T<:Number}
    append!(
        f.affine_terms,
        MOI.VectorAffineTerm.(output_index, operate_terms(op, g.terms)),
    )
    operate_output_index!(op, T, output_index, f, MOI.constant(g))
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorQuadraticFunction{T},
    g::MOI.ScalarQuadraticFunction{T},
) where {T<:Number}
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
    operate_output_index!(op, T, output_index, f, MOI.constant(g))
    return f
end

function operate_output_index!(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    output_index::Integer,
    f::MOI.VectorNonlinearFunction,
    g::Union{T,MOI.AbstractScalarFunction},
) where {T<:Number}
    f.rows[output_index] = operate!(op, T, f.rows[output_index], g)
    return f
end

"""
    operate_coefficient(
        op::Function,
        f::Union{
            MOI.ScalarAffineTerm,
            MOI.ScalarQuadraticTerm,
            MOI.VectorAffineTerm,
            MOI.VectorQuadraticTerm,
        },
    )

Return a new term, which is the result of calling `op(coefficient)` for on the
coefficient of the term `f`.

!!! warning
    This method is deprecated and may be removed in MathOptInterface v2.0. Use
    `operate_term(op, f)` instead.
"""
operate_coefficient(op::F, f) where {F} = operate_term(op, f)

"""
    operate_coefficients(
        op::F,
        f::Union{
            MOI.ScalarAffineFunction,
            MOI.ScalarQuadraticFunction,
            MOI.VectorAffineFunction,
            MOI.VectorQuadraticFunction,
        },
    ) where {F}
"""
function operate_coefficients end

operate_coefficients(op::F, f) where {F} = operate_terms(op, f)

function operate_coefficients(op::F, f::MOI.ScalarAffineFunction) where {F}
    return MOI.ScalarAffineFunction(operate_terms(op, f.terms), op(f.constant))
end

function operate_coefficients(op::F, f::MOI.ScalarQuadraticFunction) where {F}
    return MOI.ScalarQuadraticFunction(
        operate_terms(op, f.quadratic_terms),
        operate_terms(op, f.affine_terms),
        op(f.constant),
    )
end

function operate_coefficients(op::F, f::MOI.VectorAffineFunction) where {F}
    return MOI.VectorAffineFunction(
        operate_terms(op, f.terms),
        op.(f.constants),
    )
end

function operate_coefficients(op::F, f::MOI.VectorQuadraticFunction) where {F}
    return MOI.VectorQuadraticFunction(
        operate_terms(op, f.quadratic_terms),
        operate_terms(op, f.affine_terms),
        map(op, f.constants),
    )
end
