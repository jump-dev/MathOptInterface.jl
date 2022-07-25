# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SplitHyperRectangleBridge{T,G,F} <: Bridges.Constraint.AbstractBridge

`SplitHyperRectangleBridge` implements the following reformulation:

  * ``f(x) \\in \\textsf{HyperRectangle}(l, u)`` to
    ``[f(x) - l; u - f(x)] \\in \\mathbb{R}_+``.

## Source node

`SplitHyperRectangleBridge` supports:

  * `F` in [`MOI.HyperRectangle`](@ref)

## Target nodes

`SplitHyperRectangleBridge` creates:

  * `G` in [`MOI.Nonnegatives`](@ref)
"""
mutable struct SplitHyperRectangleBridge{T,G,F} <: AbstractBridge
    ci::MOI.ConstraintIndex{G,MOI.Nonnegatives}
    func::F
    set::MOI.HyperRectangle{T}
end

const SplitHyperRectangle{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitHyperRectangleBridge{T},OT}

function bridge_constraint(
    ::Type{SplitHyperRectangleBridge{T,G,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.HyperRectangle,
) where {T,G,F}
    lower = MOI.Utilities.operate(-, T, f, s.lower)
    upper = MOI.Utilities.operate(-, T, s.upper, f)
    if any(!isfinite, s.lower)
        indices = [i for (i, l) in enumerate(s.lower) if isfinite(l)]
        lower = MOI.Utilities.eachscalar(lower)[indices]
    end
    if any(!isfinite, s.upper)
        indices = [i for (i, u) in enumerate(s.upper) if isfinite(u)]
        upper = MOI.Utilities.eachscalar(upper)[indices]
    end
    g = MOI.Utilities.operate(vcat, T, lower, upper)
    ci = MOI.add_constraint(model, g, MOI.Nonnegatives(MOI.output_dimension(g)))
    return SplitHyperRectangleBridge{T,typeof(g),F}(ci, f, s)
end

function MOI.supports_constraint(
    ::Type{<:SplitHyperRectangleBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.HyperRectangle{T}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SplitHyperRectangleBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SplitHyperRectangleBridge{T,G}},
) where {T,G}
    return Tuple{Type,Type}[(G, MOI.Nonnegatives),]
end

function concrete_bridge_type(
    ::Type{<:SplitHyperRectangleBridge{T}},
    ::Type{F},
    ::Type{MOI.HyperRectangle{T}},
) where {T,F<:MOI.AbstractVectorFunction}
    G = MOI.Utilities.promote_operation(-, T, F, Vector{T})
    return SplitHyperRectangleBridge{T,G,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::SplitHyperRectangleBridge,
)
    # We cache the function to simplify cases in which the box is (-Inf, Inf).
    # This contributes some memory overhead, so if someone complains in future
    # we could reconsider this decision.
    return bridge.func
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitHyperRectangleBridge,
)
    return bridge.set
end

function MOI.delete(model::MOI.ModelLike, bridge::SplitHyperRectangleBridge)
    MOI.delete(model, bridge.ci)
    return
end

function MOI.get(
    ::SplitHyperRectangleBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.Nonnegatives},
)::Int64 where {T,G}
    return 1
end

function MOI.get(
    bridge::SplitHyperRectangleBridge{T,G},
    ::MOI.ListOfConstraintIndices{G,MOI.Nonnegatives},
) where {T,G}
    return [bridge.ci]
end

function MOI.supports(
    ::MOI.ModelLike,
    ::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitHyperRectangleBridge},
)
    return true
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitHyperRectangleBridge{T},
    value::AbstractVector{T}
) where {T}
    new_values = vcat(
        T[v for (v, l) in zip(value, bridge.set.lower) if isfinite(l)],
        T[v for (v, u) in zip(value, bridge.set.upper) if isfinite(u)],
    )
    MOI.set(model, attr, bridge.ci, new_values)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitHyperRectangleBridge{T},
) where {T}
    values = MOI.get(model, attr, bridge.ci)
    if values === nothing
        return nothing
    end
    ret = zeros(T, MOI.dimension(bridge.set))
    row = 0
    for (i, l) in enumerate(bridge.set.lower)
        if isfinite(l)
            row += 1
            ret[i] = values[row]
        end
    end
    for (i, u) in enumerate(bridge.set.upper)
        if isfinite(u)
            row += 1
            ret[i] = values[row]
        end
    end
    return ret
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SplitHyperRectangleBridge{T},
    values::AbstractVector{T}
) where {T}
    set = bridge.set
    new_values = vcat(
        T[max(T(0), v) for (v, l) in zip(values, set.lower) if isfinite(l)],
        T[min(T(0), v) for (v, u) in zip(values, set.upper) if isfinite(u)],
    )
    MOI.set(model, attr, bridge.ci, new_values)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SplitHyperRectangleBridge{T},
) where {T}
    values = MOI.get(model, attr, bridge.ci)
    if values === nothing
        return nothing
    end
    ret = zeros(T, MOI.dimension(bridge.set))
    row = 0
    for (i, l) in enumerate(bridge.set.lower)
        if isfinite(l)
            row += 1
            ret[i] += values[row]
        end
    end
    for (i, u) in enumerate(bridge.set.upper)
        if isfinite(u)
            row += 1
            ret[i] += values[row]
        end
    end
    return ret
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitHyperRectangleBridge{T},
    ::Nothing
) where {T}
    MOI.set(model, attr, bridge.ci, nothing)
    return
end
