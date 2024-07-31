# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct LinearCombinationBridge{T,S,A,V,F,G} <:
       SetMapBridge{T,S,MOI.LinearCombinationInSet{S,A,V},F,G}
    constraint::MOI.ConstraintIndex{F,S}
    set::MOI.LinearCombinationInSet{S,A,V}
end

function MOI.supports_constraint(
    ::Type{<:LinearCombinationBridge},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.LinearCombinationInSet},
)
    return true
end

function concrete_bridge_type(
    ::Type{<:LinearCombinationBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.LinearCombinationInSet{S,A,V}},
) where {T,S,A,V}
    U = MOI.Utilities.promote_operation(*, T, MOI.Utilities.scalar_type(G), T)
    F = MOI.Utilities.promote_operation(vcat, T, U)
    return LinearCombinationBridge{T,S,A,V,F,G}
end

function _map_function(set::MOI.LinearCombinationInSet, func)
    scalars = MOI.Utilities.eachscalar(func)
    return MOI.Utilities.vectorize([
        sum(scalars[j] * set.vectors[j][i] for j in eachindex(scalars)) for
        i in 1:MOI.dimension(set.set)
    ])
end

function bridge_constraint(
    ::Type{LinearCombinationBridge{T,S,A,V,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::MOI.LinearCombinationInSet{S,A,V},
) where {T,S,A,F,G,V}
    mapped_func = _map_function(set, func)
    constraint = MOI.add_constraint(model, mapped_func, set.set)
    return LinearCombinationBridge{T,S,A,V,F,G}(constraint, set)
end

function MOI.Bridges.map_set(
    ::Type{<:LinearCombinationBridge},
    set::MOI.LinearCombinationInSet,
)
    return set.set
end

function MOI.Bridges.inverse_map_set(
    bridge::LinearCombinationBridge,
    ::MOI.AbstractSet,
)
    return bridge.set
end

function MOI.Bridges.adjoint_map_function(bridge::LinearCombinationBridge, func)
    scalars = MOI.Utilities.eachscalar(func)
    return MOI.Utilities.vectorize([
        MOI.Utilities.set_dot(vector, scalars, bridge.set.set) for
        vector in bridge.set.vectors
    ])
end
