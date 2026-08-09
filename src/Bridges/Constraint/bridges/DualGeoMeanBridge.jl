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
    u_neg = MOI.Utilities.vectorize([MOI.Utilities.operate(-, T, f_scalars[1])])
    nn_index = MOI.add_constraint(model, u_neg, MOI.Nonnegatives(1))
    u_n = MOI.Utilities.operate(/, T, f_scalars[1], -T(MOI.dimension(s) - 1))
    ci = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, u_n, f_scalars[2:end]),
        MOI.GeometricMeanCone(MOI.dimension(s)),
    )
    return DualGeoMeanBridge{T,G,H}(nn_index, ci)
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
    return Tuple{Type,Type}[(G, MOI.Nonnegatives), (G, MOI.GeometricMeanCone)]
end

function concrete_bridge_type(
    ::Type{<:DualGeoMeanBridge{T}},
    ::Type{H},
    ::Type{MOI.DualGeometricMeanCone},
) where {T,H<:MOI.AbstractVectorFunction}
    S = MOI.Utilities.scalar_type(H)
    TS = MOI.Utilities.promote_operation(+, T, S, MOI.VariableIndex)
    G = MOI.Utilities.promote_operation(vcat, T, T, S, TS)
    return DualGeoMeanBridge{T,G,H}
end

function MOI.get(
    ::DualGeoMeanBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.Nonnegatives},
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
    ::DualGeoMeanBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.GeometricMeanCone},
)::Int64 where {T,G}
    return 1
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
    g = MOI.get(model, MOI.ConstraintFunction(), bridge.geomean_index)
    scalars = MOI.Utilities.eachscalar(g)
    n = -T(length(scalars) - 1)
    u = MOI.Utilities.operate(*, T, scalars[1], n)
    f = MOI.Utilities.operate(vcat, T, u, scalars[2:end])
    return MOI.Utilities.convert_approx(H, f)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::DualGeoMeanBridge,
)
    set = MOI.get(model, MOI.ConstraintSet(), bridge.geomean_index)
    return MOI.DualGeometricMeanCone(MOI.dimension(set))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::DualGeoMeanBridge,
)
    primal = MOI.get(model, attr, bridge.geomean_index)
    primal[1] *= -(length(primal) - 1)
    return primal
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::DualGeoMeanBridge,
)
    dual = MOI.get(model, attr, bridge.geomean_index)
    dual[1] ./= -(length(dual) - 1)
    return dual
end
