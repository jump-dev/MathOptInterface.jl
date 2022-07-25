# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    GeoMeanToPowerBridge{T,F} <: Bridges.Constraint.AbstractBridge

`GeoMeanToPowerBridge` implements the following reformulation:

  * ``(y, x...) \\in GeometricMeanCone(1+d)`` into
    ``(x_1, t, y) \\in PowerCone(1/d)`` and ``(t, x_2, ..., x_d) in GeometricMeanCone(d)``.

## Source node

`GeoMeanToPowerBridge` supports:

  * `F` in [`MOI.GeometricMeanCone`](@ref)

## Target nodes

`GeoMeanToPowerBridge` creates:

  * `F` in [`MOI.PowerCone{T}`](@ref)
  * [`MOI.VectorOfVariables`](@ref) in [`MOI.Nonnegatives`](@ref)
"""
struct GeoMeanToPowerBridge{T,F} <: AbstractBridge
    power::Vector{MOI.ConstraintIndex{F,MOI.PowerCone{T}}}
    t::Vector{MOI.VariableIndex}
    nn::Union{
        Nothing,
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives},
    }
    dimension::Int
end

const GeoMeanToPower{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{GeoMeanToPowerBridge{T},OT}

function bridge_constraint(
    ::Type{GeoMeanToPowerBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.GeometricMeanCone,
) where {T,F}
    d = MOI.dimension(s)
    fi_s = MOI.Utilities.eachscalar(f)
    if d == 2
        # We could do something cleverer here, but this is a weird constraint,
        # and we don't want to overcomplicate the bridge.
        # t <= 1√(x_1) <=> t <= √(x_1 * x_1)
        ci = MOI.add_constraint(model, fi_s[[2, 2, 1]], MOI.PowerCone(T(1 / 2)))
        return GeoMeanToPowerBridge{T,F}([ci], MOI.VariableIndex[], nothing, d)
    elseif d == 3
        # t <= √(x_1 * x_2)
        ci = MOI.add_constraint(model, fi_s[[2, 3, 1]], MOI.PowerCone(T(1 / 2)))
        return GeoMeanToPowerBridge{T,F}([ci], MOI.VariableIndex[], nothing, d)
    else
        ts, nn = MOI.add_constrained_variables(model, MOI.Nonnegatives(d - 3))
        cis = MOI.ConstraintIndex{F,MOI.PowerCone{T}}[]
        z, x, n = fi_s[1], fi_s[2], d - 1
        for i in 1:(d-3)
            y = ts[i]
            fi = MOI.Utilities.operate(vcat, T, x, y, z)
            push!(cis, MOI.add_constraint(model, fi, MOI.PowerCone(T(1 / n))))
            n -= 1
            x, z = fi_s[2+i], y
        end
        fi = MOI.Utilities.operate(vcat, T, fi_s[end-1], fi_s[end], ts[end])
        push!(cis, MOI.add_constraint(model, fi, MOI.PowerCone(T(1 / 2))))
        return GeoMeanToPowerBridge{T,F}(cis, ts, nn, d)
    end
end

function MOI.supports_constraint(
    ::Type{<:GeoMeanToPowerBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:GeoMeanToPowerBridge},
)
    return Tuple{Type}[(MOI.Nonnegatives,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:GeoMeanToPowerBridge{T,F}},
) where {T,F}
    return Tuple{Type,Type}[(F, MOI.PowerCone{T})]
end

function concrete_bridge_type(
    ::Type{<:GeoMeanToPowerBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    return GeoMeanToPowerBridge{T,F}
end

function MOI.get(bridge::GeoMeanToPowerBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.t)
end

function MOI.get(bridge::GeoMeanToPowerBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.t)
end

function MOI.get(
    bridge::GeoMeanToPowerBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives},
)::Int64
    if bridge.nn === nothing
        return 0
    end
    return 1
end

function MOI.get(
    bridge::GeoMeanToPowerBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives},
)
    if bridge.nn === nothing
        return MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}[]
    end
    return [bridge.nn]
end

function MOI.get(
    bridge::GeoMeanToPowerBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.PowerCone{T}},
)::Int64 where {T,F}
    return length(bridge.power)
end

function MOI.get(
    bridge::GeoMeanToPowerBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.PowerCone{T}},
) where {T,F}
    return copy(bridge.power)
end

function MOI.delete(model::MOI.ModelLike, bridge::GeoMeanToPowerBridge)
    MOI.delete(model, bridge.power)
    if bridge.nn !== nothing
        MOI.delete(model, bridge.nn)
        MOI.delete(model, bridge.t)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::GeoMeanToPowerBridge{T,F},
) where {T,F}
    f = MOI.get(model, MOI.ConstraintFunction(), bridge.power[1])
    fi_s = MOI.Utilities.eachscalar(f)
    if bridge.dimension == 2
        return fi_s[[3, 1]]
    elseif bridge.dimension == 3
        return fi_s[[3, 1, 2]]
    end
    g = fi_s[[3, 1]]
    for i in 2:(length(bridge.power)-1)
        fi = MOI.get(model, MOI.ConstraintFunction(), bridge.power[i])
        fi_s = first(MOI.Utilities.eachscalar(fi))
        g = MOI.Utilities.operate(vcat, T, g, fi_s)
    end
    fi = MOI.get(model, MOI.ConstraintFunction(), bridge.power[end])
    fi_s = MOI.Utilities.eachscalar(fi)
    return MOI.Utilities.operate(vcat, T, g, fi_s[1:2])
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::GeoMeanToPowerBridge,
)
    return MOI.GeometricMeanCone(bridge.dimension)
end
