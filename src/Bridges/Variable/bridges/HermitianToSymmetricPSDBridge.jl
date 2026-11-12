# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    HermitianToSymmetricPSDBridge{T} <: Bridges.Variable.AbstractBridge

`HermitianToSymmetricPSDBridge` represents an `n x n` Hermitian positive
semidefinite matrix using a `2n x 2n` real symmetric positive semidefinite matrix.

## Source node

`HermitianToSymmetricPSDBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in
    [`MOI.HermitianPositiveSemidefiniteConeTriangle`](@ref)

## Target node

`HermitianToSymmetricPSDBridge` creates:

  * [`MOI.VectorOfVariables`](@ref) in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)

## Reformulation

Partition the real symmetric positive semidefinite matrix as `[A B; B' C]`.
The Hermitian matrix is represented by `A + C + im * (B - B')`. Every Hermitian
positive semidefinite matrix `X + im * Y` has a positive semidefinite preimage
`[X Y; -Y X] / 2`, so no additional equality constraints are needed.

This map is the adjoint of the map used by
[`MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge`](@ref).
"""
mutable struct HermitianToSymmetricPSDBridge{T} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.HermitianPositiveSemidefiniteConeTriangle,
}
    variables::Vector{MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    }
    imag_diag_start_set::Bool

    function HermitianToSymmetricPSDBridge{T}(variables, constraint) where {T}
        return new{T}(variables, constraint, false)
    end
end

const HermitianToSymmetricPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{HermitianToSymmetricPSDBridge{T},OT}

function MOI.Bridges.map_set(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    set::MOI.PositiveSemidefiniteConeTriangle,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.inverse_map_set(BT, set)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.map_set(BT, set)
end

function MOI.Bridges.map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
    index::MOI.Bridges.IndexInVector...,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.adjoint_map_function(BT, func, index...)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.inverse_adjoint_map_function(BT, func)
end

function MOI.Bridges.adjoint_map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.map_function(BT, func)
end

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    BT = MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge{T}
    return MOI.Bridges.inverse_map_function(BT, func)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::HermitianToSymmetricPSDBridge,
    index::MOI.Bridges.IndexInVector,
)
    # Query only this component, so unrelated missing starts do not affect it.
    f = MOI.Bridges.bridged_function(bridge, index)
    values = MOI.get(model, attr, [term.variable for term in f.terms])
    if any(isnothing, values)
        return nothing
    end
    return sum(
        term.coefficient * value for (term, value) in zip(f.terms, values)
    )
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::HermitianToSymmetricPSDBridge,
    value,
    index::MOI.Bridges.IndexInVector,
)
    n = div(
        MOI.Utilities.side_dimension_for_vectorized_dimension(
            length(bridge.variables),
        ),
        2,
    )
    d = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(n))
    value = value === nothing ? nothing : value / 2
    if index.value > d  # Imaginary part
        i, j = MOI.Utilities.inverse_trimap(index.value - d)
        j += 1  # Increment `j` by `1` to account for the zero diagonal
        k12 = MOI.Utilities.trimap(i, n + j)
        MOI.set(model, attr, bridge.variables[k12], value)
        k21 = MOI.Utilities.trimap(j, n + i)
        minus_value = value === nothing ? nothing : -value
        MOI.set(model, attr, bridge.variables[k21], minus_value)
    else  # Real part
        i, j = MOI.Utilities.inverse_trimap(index.value)
        k11 = MOI.Utilities.trimap(i, j)
        MOI.set(model, attr, bridge.variables[k11], value)
        k22 = MOI.Utilities.trimap(n + i, n + j)
        MOI.set(model, attr, bridge.variables[k22], value)
    end
    # The variables on the imaginary diagonal have a value 0, and they cannot be
    # referenced by the user. So if we are setting the VariablePrimalStart for
    # some components, assume that we also want to set it for the imaginary
    # diagonal (so that every variable has a primal start). But we should do
    # this once and only once, so cache whether we have in the
    # `imag_diag_start_set` field.
    if value === nothing && bridge.imag_diag_start_set
        bridge.imag_diag_start_set = false
        for i in 1:n
            k = MOI.Utilities.trimap(i, n + i)
            MOI.set(model, attr, bridge.variables[k], nothing)
        end
    elseif value !== nothing && !bridge.imag_diag_start_set
        bridge.imag_diag_start_set = true
        for i in 1:n
            k = MOI.Utilities.trimap(i, n + i)
            MOI.set(model, attr, bridge.variables[k], zero(value))
        end
    end
    return
end
