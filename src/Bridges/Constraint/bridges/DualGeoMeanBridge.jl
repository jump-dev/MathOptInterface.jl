# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    DualGeoMeanBridge{T,G,H} <: Bridges.Constraint.AbstractBridge

`DualGeoMeanBridge` implements the following reformulation:

  * ``(u, v) \\in DualGeometricMeanCone`` into
    ``(-u/length(v), v) \\in GeometricMeanCone`` and ``u \\le 0``

## Source node

`DualGeoMeanBridge` supports:

  * `H` in [`MOI.DualGeometricMeanCone`](@ref)

## Target nodes

`DualGeoMeanBridge` creates:

  * `G` in [`MOI.GeometricMeanCone`](@ref)
  * `G` in [`MOI.Nonnegatives`](@ref)
"""
struct DualGeoMeanBridge{T,G,H} <: AbstractBridge
    nn_index::MOI.ConstraintIndex{G,MOI.Nonnegatives}
    geomean_index::MOI.ConstraintIndex{G,MOI.GeometricMeanCone}
end

const DualGeoMean{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{DualGeoMeanBridge{T},OT}

function bridge_constraint(
    ::Type{DualGeoMeanBridge{T,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.DualGeometricMeanCone,
) where {T,G,H}
    f_scalars = MOI.Utilities.eachscalar(f)
    nn_index = MOI.add_constraint(
        model,
        MOI.Utilities.vectorize([MOI.Utilities.operate(-, T, f_scalars[1])]),
        MOI.Nonnegatives(1),
    )
    geomean_func = MOI.Utilities.operate(
        vcat,
        T,
        MOI.Utilities.operate(/, T, f_scalars[1], -T(MOI.dimension(s) - 1)),
        f_scalars[2:end],
    )
    geomean_index = MOI.add_constraint(
        model,
        geomean_func,
        MOI.GeometricMeanCone(MOI.dimension(s)),
    )
    return DualGeoMeanBridge{T,G,H}(nn_index, geomean_index)
end

function MOI.supports_constraint(
    ::Type{<:DualGeoMeanBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.DualGeometricMeanCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:DualGeoMeanBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:DualGeoMeanBridge{T,G}},
) where {T,G}
    return Tuple{Type,Type,Type}[(G, MOI.Nonnegatives, MOI.GeometricMeanCone)]
end

function concrete_bridge_type(
    ::Type{<:DualGeoMeanBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.DualGeometricMeanCone},
) where {T}
    S = MOI.Utilities.scalar_type(H)
    G = MOI.Utilities.promote_operation(
        vcat,
        T,
        T,
        S,
        MOI.Utilities.promote_operation(+, T, S, MOI.VariableIndex),
    )
    return DualGeoMeanBridge{T,G,H}
end

MOI.get(::DualGeoMeanBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(
    ::DualGeoMeanBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.Nonnegatives},
)::Int64 where {T,G}
    return 1
end

function MOI.get(
    ::DualGeoMeanBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.GeometricMeanCone},
)::Int64 where {T,G}
    return 1
end

function MOI.get(
    bridge::DualGeoMeanBridge{T,G},
    ::MOI.ListOfConstraintIndices{G,MOI.Nonnegatives},
) where {T,G}
    return [bridge.nn_index]
end

function MOI.get(
    bridge::DualGeoMeanBridge{T,G},
    ::MOI.ListOfConstraintIndices{G,MOI.GeometricMeanCone},
) where {T,G}
    return [bridge.geomean_index]
end

function MOI.delete(model::MOI.ModelLike, bridge::DualGeoMeanBridge)
    MOI.delete(model, bridge.geomean_index)
    MOI.delete(model, bridge.nn_index)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::DualGeoMeanBridge{T,G,H},
) where {T,G,H}
    geomean_func = MOI.Utilities.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.geomean_index),
    )
    d = length(geomean_func) - 1
    u = MOI.Utilities.operate(*, T, geomean_func[1], T(-d))
    return MOI.Utilities.convert_approx(
        H,
        MOI.Utilities.operate(vcat, T, u, geomean_func[2:end]),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::DualGeoMeanBridge,
)
    return MOI.DualGeometricMeanCone(
        MOI.dimension(
            MOI.get(model, MOI.ConstraintSet(), bridge.geomean_index),
        ),
    )
end
