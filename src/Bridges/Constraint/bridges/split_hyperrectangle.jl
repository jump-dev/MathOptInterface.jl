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
    scalars = MOI.Utilities.eachscalar(f)
    lower = [
        MOI.Utilities.operate(-, T, fi, s.lower[i]) for
        (i, fi) in enumerate(scalars) if isfinite(s.lower[i])
    ]
    upper = [
        MOI.Utilities.operate(-, T, s.upper[i], fi) for
        (i, fi) in enumerate(scalars) if isfinite(s.upper[i])
    ]
    g = MOI.Utilities.operate(vcat, T, lower..., upper...)
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
