# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    NormInfinityBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`NormInfinityBridge` implements the following reformulation:

  * ``|x|_\\infty \\le t`` into ``[t - x_i, t + x_i] \\in \\mathbb{R}_+``.

## Source node

`NormInfinityBridge` supports:

  * `G` in [`MOI.NormInfinityCone{T}`](@ref)

## Target nodes

`NormInfinityBridge` creates:

  * `F` in [`MOI.Nonnegatives`](@ref)
"""
struct NormInfinityBridge{T,F,G} <:
       SetMapBridge{T,MOI.Nonnegatives,MOI.NormInfinityCone,F,G}
    constraint::MOI.ConstraintIndex{F,MOI.Nonnegatives}
end

const NormInfinity{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NormInfinityBridge{T},OT}

function concrete_bridge_type(
    ::Type{<:NormInfinityBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormInfinityCone},
) where {T}
    F = MOI.Utilities.promote_operation(+, T, G, G)
    return NormInfinityBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:NormInfinityBridge},
    set::MOI.NormInfinityCone,
)
    return MOI.Nonnegatives(2 * (MOI.dimension(set) - 1))
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:NormInfinityBridge},
    set::MOI.Nonnegatives,
)
    return MOI.NormInfinityCone(div(MOI.dimension(set), 2) + 1)
end

function MOI.Bridges.map_function(
    ::Type{<:NormInfinityBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    t, x = scalars[1], scalars[2:end]
    # Create f_new = [-x; x].
    f_new = MOI.Utilities.operate(vcat, T, MOI.Utilities.operate(-, T, x), x)
    # Add +t to each row of x
    for i in 1:(2*(length(scalars)-1))
        MOI.Utilities.operate_output_index!(+, T, i, f_new, t)
    end
    return f_new
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:NormInfinityBridge{T}},
    func,
) where {T}
    # func is [t - x; t + x]
    scalars = MOI.Utilities.eachscalar(func)
    # Get t by `(t - x) + (t + x)`, then dividing by the number of rows.
    t = MOI.Utilities.operate!(/, T, sum(scalars), T(length(scalars)))
    d = div(length(scalars), 2)
    # Get x by (t + x) - (t - x) = 2x
    x = MOI.Utilities.operate!(-, T, scalars[(d+1):end], scalars[1:d])
    x = MOI.Utilities.operate!(/, T, x, T(2))
    return MOI.Utilities.operate(vcat, T, t, x)
end

# Given a_i is dual on t - x_i >= 0 and b_i is dual on t + x_i >= 0,
# the dual on (t, x) in NormInfinityCone is (u, v) in NormOneCone, where
# v_i = -a_i + b_i and u = sum(a) + sum(b).
function MOI.Bridges.adjoint_map_function(::Type{<:NormInfinityBridge}, func)
    scalars = MOI.Utilities.eachscalar(func)
    t = sum(scalars)
    d = div(length(scalars), 2)
    x = scalars[(d+1):end] - scalars[1:d]
    return vcat(t, x)
end

function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:NormInfinityBridge{T}},
    func::AbstractVector{T},
) where {T}
    # This is used by `MOI.ConstraintDualStart`.
    # The result should belong to `MOI.Nonnegatives` and the sum of the elements
    # should be `t`.
    # Only one of dual of the upper and lower bound should be active so one of
    # the two duals is zero. We know which one since they should be nonnegative.
    # Then if `t = sum abs(x_i)`, we will indeed have only one of them being
    # zero.
    t, y = func[1], func[2:end]
    lb = [x > 0 ? x : zero(x) for x in y]
    ub = [x < 0 ? -x : zero(x) for x in y]
    x = [ub; lb]
    return x .+ (t - sum(x)) / length(x)
end
