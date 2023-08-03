# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    promote_operation(
        op::Function,
        ::Type{T},
        ArgsTypes::Type{<:Union{T,AbstractVector{T},MOI.AbstractFunction}}...,
    ) where {T<:Number}

Compute the return type of the call `operate(op, T, args...)`, where the types
of the arguments `args` are `ArgsTypes`.

One assumption is that the element type `T` is invariant under each operation.
That is, `op(::T, ::T)::T` where `op` is a `+`, `-`, `*`, and `/`.

There are six methods for which we implement `Utilities.promote_operation`:

 1. `+`
    a. `promote_operation(::typeof(+), ::Type{T}, ::Type{F1}, ::Type{F2})`
 2. `-`
    a. `promote_operation(::typeof(-), ::Type{T}, ::Type{F})`
    b. `promote_operation(::typeof(-), ::Type{T}, ::Type{F1}, ::Type{F2})`
 3. `*`
    a. `promote_operation(::typeof(*), ::Type{T}, ::Type{T}, ::Type{F})`
    b. `promote_operation(::typeof(*), ::Type{T}, ::Type{F}, ::Type{T})`
    c. `promote_operation(::typeof(*), ::Type{T}, ::Type{F1}, ::Type{F2})`
       where `F1` and `F2` are `VariableIndex` or `ScalarAffineFunction`
    d. `promote_operation(::typeof(*), ::Type{T}, ::Type{<:Diagonal{T}}, ::Type{F}`
 4. `/`
    a. `promote_operation(::typeof(/), ::Type{T}, ::Type{F}, ::Type{T})`
 5. `vcat`
    a. `promote_operation(::typeof(vcat), ::Type{T}, ::Type{F}...)`
 6. `imag`
    a. `promote_operation(::typeof(imag), ::Type{T}, ::Type{F})`
       where `F` is `VariableIndex` or `VectorOfVariables`

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
function promote_operation end

### Method 1a and 2b

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{T},
    ::Type{T},
) where {T<:Number}
    return T
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:F},
    ::Type{<:F},
) where {T,F<:Union{T,MOI.VariableIndex,MOI.ScalarAffineFunction{T}}}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:F},
    ::Type{<:F},
) where {
    T,
    F<:Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    },
}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{<:F},
    ::Type{<:F},
) where {
    T,
    F<:Union{
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
    },
}
    return MOI.ScalarNonlinearFunction
end

function promote_operation(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{F1},
    ::Type{F2},
) where {
    T,
    F1<:Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.GenericVectorFunction,
    },
    F2<:Union{
        AbstractVector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.GenericVectorFunction,
    },
}
    S = promote_operation(op, T, scalar_type(F1), scalar_type(F2))
    return vector_type(S)
end

### Method 2a

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{F},
) where {
    T,
    F<:Union{
        T,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        AbstractVector{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return F
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{MOI.VariableIndex},
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{F},
) where {T,F<:MOI.GenericVectorFunction}
    return vector_type(promote_operation(-, T, scalar_type(F)))
end

### Method 3a

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{F},
) where {
    T<:Number,
    F<:Union{
        # T,  # Stackoverflow if included
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        AbstractVector{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return F
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{T},
) where {T<:Number}
    return T
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{MOI.VariableIndex},
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{F},
) where {T<:Number,F<:MOI.GenericVectorFunction}
    return vector_type(promote_operation(*, T, T, scalar_type(F)))
end

### Method 3b

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{F},
    ::Type{T},
) where {
    T<:Number,
    F<:Union{
        # T,  # Stackoverflow if included
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        AbstractVector{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return F
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{MOI.VariableIndex},
    ::Type{T},
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{T},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{F},
    ::Type{T},
) where {T,F<:MOI.GenericVectorFunction}
    return vector_type(promote_operation(*, T, scalar_type(F), T))
end

### Method 3c

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
    ::Type{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
) where {T<:Number}
    return MOI.ScalarQuadraticFunction{T}
end

### Method 3d

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{<:Diagonal{T}},
    ::Type{F},
) where {T,F}
    U = promote_operation(*, T, T, scalar_type(F))
    return promote_operation(vcat, T, U)
end

### Method 4a

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{F},
    ::Type{T},
) where {
    T,
    F<:Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        AbstractVector{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return F
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{T},
    ::Type{T},
) where {T<:Number}
    return T
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{MOI.VariableIndex},
    ::Type{T},
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{T},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{F},
    ::Type{T},
) where {T,F<:MOI.GenericVectorFunction}
    return vector_type(promote_operation(/, T, scalar_type(F), T))
end

### Method 5a

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{<:Union{T,AbstractVector{T}}},
    ::Type{<:Union{T,AbstractVector{T}}}...,
) where {T<:Number}
    return Vector{T}
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
    ::Type{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}}...,
) where {T<:Number}
    return MOI.VectorOfVariables
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{
        <:Union{
            T,
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{T},
            AbstractVector{T},
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
        },
    }...,
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{
        <:Union{
            T,
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{T},
            MOI.ScalarQuadraticFunction{T},
            AbstractVector{T},
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
            MOI.VectorQuadraticFunction{T},
        },
    }...,
) where {T<:Number}
    return MOI.VectorQuadraticFunction{T}
end

function promote_operation(
    ::typeof(vcat),
    ::Type{T},
    ::Type{
        <:Union{
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
        },
    }...,
) where {T<:Number}
    return MOI.VectorNonlinearFunction
end

### Method 6a

function promote_operation(
    ::typeof(imag),
    ::Type{T},
    ::Type{MOI.VariableIndex},
) where {T<:Number}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(imag),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T<:Number}
    return MOI.VectorAffineFunction{T}
end
