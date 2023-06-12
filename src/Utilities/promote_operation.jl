# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# There are five methods for which we implement `Utilities.promote_operation`
# and `Utilities.operate`:
#
#  1. `+`
#     a. `promote_operation(::typeof(+), ::Type{T}, ::Type{F1}, ::Type{F2})`
#  2. `-`
#     a. `promote_operation(::typeof(-), ::Type{T}, ::Type{F})`
#     b. `promote_operation(::typeof(-), ::Type{T}, ::Type{F1}, ::Type{F2})`
#     c. `promote_operation(::typeof(-), ::Type{T}, ::Type{F1}, ::Type{Vector{T}})`
#  3. `*`
#     a. `promote_operation(::typeof(*), ::Type{T}, ::Type{T}, ::Type{F})`
#  4. `/`
#     a. `promote_operation(::typeof(/), ::Type{T}, ::Type{F}, ::Type{T})`
#  5. `vcat`
#     a. `promote_operation(::typeof(vcat), ::Type{T}, ::Type{F}...)`

# Scalar types
#
#  1. ::T
#  2. ::VariableIndex
#  3. ::ScalarAffineFunction{T}
#  4. ::ScalarQuadraticFunction{T}
#
# Vector functions
#
#  1. ::Vector{T}
#  2. ::VectorOfVariables
#  3. ::VectorAffineFunction{T}
#  4. ::VectorQuadraticFunction{T}

vector_type(::Type{T}) where {T} = Vector{T}

vector_type(::Type{MOI.VariableIndex}) = MOI.VectorOfVariables

function vector_type(::Type{MOI.ScalarAffineFunction{T}}) where {T}
    return VectorAffineFunction{T}
end

function vector_type(::Type{MOI.ScalarQuadraticFunction{T}}) where {T}
    return VectorQuadraticFunction{T}
end

function vector_type(::Type{MOI.ScalarNonlinearFunction})
    return VectorNonlinearFunction
end

### Method 1a and 2b

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{T},
    ::Type{F},
) where {T,F}
    return F
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{T},
    ::Type{MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{MOI.VariableIndex},
    ::Type{F},
) where {T,F}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{MOI.ScalarAffineFunction{T}},
    ::Type{F},
) where {T,F}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{MOI.ScalarAffineFunction{T}},
    ::Type{MOI.ScalarQuadraticFunction{T}},
) where {T,F}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    ::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{F},
) where {T,F}
    return MOI.ScalarQuadraticFunction{T}
end

function promote_operation(
    op::Union{typeof(+),typeof(-)},
    ::Type{T},
    ::Type{F1},
    ::Type{F2},
) where {T,F1<:MOI.AbstractVectorFunction,F2<:MOI.AbstractVectorFunction}
    S = promote_operation(io, T, scalar_type(F1), scalar_type(F2))
    return vector_type(S, T)
end

### Method 2a

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{F},
) where {
    T,
    F<:Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
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
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T}
    return MOI.VectorAffineFunction{T}
end

### Method 2b

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{F},
    ::Type{<:AbstractVector{T}},
) where {
    T,
    F<:Union{MOI.VectorAffineFunction{T},MOI.VectorQuadraticFunction{T}},
}
    return F
end

function promote_operation(
    ::typeof(-),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{<:AbstractVector{T}},
) where {T}
    return MOI.VectorAffineFunction{T}
end

### Method 3a

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{F},
) where {
    T,
    F<:Union{
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
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
    ::Type{MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(*),
    ::Type{T},
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
) where {T}
    return MOI.VectorAffineFunction{T}
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
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    },
}
    return F
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{MOI.VariableIndex},
    ::Type{T},
) where {T}
    return MOI.ScalarAffineFunction{T}
end

function promote_operation(
    ::typeof(/),
    ::Type{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{T},
) where {T}
    return MOI.VectorAffineFunction{T}
end

### Method 5a

# TODO
