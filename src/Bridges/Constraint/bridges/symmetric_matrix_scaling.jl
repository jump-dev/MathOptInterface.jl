# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SymmetricMatrixScalingBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SymmetricMatrixScalingBridge` implements the reformulation from constraints
in the `MOI.PositiveSemidefiniteConeTriangle` to constraints
in the `MOI.ScaledPositiveSemidefiniteConeTriangle`.

## Source node

`SymmetricMatrixScalingBridge` supports:

  * `G` in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)

## Target node

`SymmetricMatrixScalingBridge` creates:

  * `F` in [`MOI.ScaledPositiveSemidefiniteConeTriangle`](@ref)
"""
struct SymmetricMatrixScalingBridge{T,F,G} <:
       SetMapBridge{T,MOI.ScaledPositiveSemidefiniteConeTriangle,MOI.PositiveSemidefiniteConeTriangle,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.ScaledPositiveSemidefiniteConeTriangle}
end

const SymmetricMatrixScaling{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SymmetricMatrixScalingBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SymmetricMatrixScalingBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.PositiveSemidefiniteConeTriangle},
) where {T}
    F = MOI.Utilities.promote_operation(*, T, Diagonal{T,Vector{T}}, G)
    return SymmetricMatrixScalingBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:SymmetricMatrixScalingBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.ScaledPositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SymmetricMatrixScalingBridge},
    set::MOI.ScaledPositiveSemidefiniteConeTriangle,
)
    return MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.map_function(::Type{<:SymmetricMatrixScalingBridge{T}}, func) where {T}
    return MOI.Utilities.operate(*, T, Diagonal(MOI.Utilities.symmetric_matrix_scaling_vector(T)), func)
end

function MOI.Bridges.inverse_map_function(::Type{<:SymmetricMatrixScalingBridge}, func)
    return MOI.Utilities.operate(*, T, Diagonal(MOI.Utilities.symmetric_matrix_inverse_scaling_vector(T)), func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:SymmetricMatrixScalingBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:SymmetricMatrixScalingBridge},
    func,
)
    return MOI.Bridges.inverse_map_function(BT, func)
end

"""
    SymmetricMatrixInverseScalingBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SymmetricMatrixInverseScalingBridge` implements the reformulation from constraints
in the `MOI.ScaledPositiveSemidefiniteConeTriangle` to constraints
in the `MOI.PositiveSemidefiniteConeTriangle`.

## Source node

`SymmetricMatrixInverseScalingBridge` supports:

  * `G` in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)

## Target node

`SymmetricMatrixInverseScalingBridge` creates:

  * `F` in [`MOI.ScaledPositiveSemidefiniteConeTriangle`](@ref)
"""
struct SymmetricMatrixInverseScalingBridge{T,F,G} <:
       SetMapBridge{T,MOI.ScaledPositiveSemidefiniteConeTriangle,MOI.PositiveSemidefiniteConeTriangle,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.ScaledPositiveSemidefiniteConeTriangle}
end

const SymmetricMatrixInverseScaling{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SymmetricMatrixInverseScalingBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:SymmetricMatrixInverseScalingBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.PositiveSemidefiniteConeTriangle},
) where {T}
    F = MOI.Utilities.promote_operation(*, T, Diagonal{T,Vector{T}}, G)
    return SymmetricMatrixInverseScalingBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:SymmetricMatrixInverseScalingBridge},
    set::MOI.ScaledPositiveSemidefiniteConeTriangle,
)
    return MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SymmetricMatrixInverseScalingBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.ScaledPositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.map_function(::Type{<:SymmetricMatrixInverseScalingBridge{T}}, func) where {T}
    return MOI.Utilities.operate(*, T, Diagonal(MOI.Utilities.symmetric_matrix_inverse_scaling_vector{T}()), func)
end

function MOI.Bridges.inverse_map_function(::Type{<:SymmetricMatrixInverseScalingBridge}, func)
    return MOI.Utilities.operate(*, T, Diagonal(MOI.Utilities.symmetric_matrix_scaling_vector{T}()), func)
end

# The map is symmetric
function MOI.Bridges.adjoint_map_function(BT::Type{<:SymmetricMatrixInverseScalingBridge}, func)
    return MOI.Bridges.map_function(BT, func)
end

# The map is a symmetric
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:SymmetricMatrixInverseScalingBridge},
    func,
)
    return MOI.Bridges.inverse_map_function(BT, func)
end
