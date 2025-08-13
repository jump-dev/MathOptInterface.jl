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
    ci::Union{Nothing,MOI.ConstraintIndex{G,MOI.Nonnegatives}}
    set::MOI.HyperRectangle{T}
    free_rows::F
    free_primal_start::Union{Nothing,Vector{T}}
    free_dual_start::Union{Nothing,Vector{T}}

    function SplitHyperRectangleBridge{T,G,F}(
        ci::Union{Nothing,MOI.ConstraintIndex{G,MOI.Nonnegatives}},
        set::MOI.HyperRectangle{T},
        free_rows::F,
    ) where {T,G,F}
        return new{T,G,F}(ci, set, free_rows, nothing, nothing)
    end
end

const SplitHyperRectangle{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SplitHyperRectangleBridge{T},OT}

function bridge_constraint(
    ::Type{SplitHyperRectangleBridge{T,G,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.HyperRectangle,
) where {T,G,F}
    N = MOI.dimension(s)
    g_vec = Vector{MOI.Utilities.scalar_type(G)}(undef, 2 * MOI.dimension(s))
    rows_to_keep = fill(true, length(g_vec))
    free_rows = Int[]
    scalars = MOI.Utilities.eachscalar(f)
    for (i, fi) in enumerate(scalars)
        if !isfinite(s.lower[i])
            rows_to_keep[i] = false
            # It doesn't really matter what goes here. We're going to drop it
            # when we vectorize the function
            g_vec[i] = fi
        elseif iszero(s.lower[i])
            g_vec[i] = fi
        else
            g_vec[i] = MOI.Utilities.operate(-, T, fi, s.lower[i])
        end
        if !isfinite(s.upper[i])
            rows_to_keep[N+i] = false
            g_vec[N+i] = fi
        elseif iszero(s.upper[i])
            g_vec[N+i] = MOI.Utilities.operate(-, T, fi)
        else
            g_vec[N+i] = MOI.Utilities.operate(-, T, s.upper[i], fi)
        end
        if !isfinite(s.lower[i]) && !isfinite(s.upper[i])
            push!(free_rows, i)
        end
    end
    if length(free_rows) == N
        return SplitHyperRectangleBridge{T,G,F}(nothing, s, f)
    end
    g = MOI.Utilities.vectorize(g_vec[rows_to_keep])
    ci = MOI.add_constraint(model, g, MOI.Nonnegatives(MOI.output_dimension(g)))
    return SplitHyperRectangleBridge{T,G,F}(ci, s, scalars[free_rows])
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
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::SplitHyperRectangleBridge{T,G,F},
) where {T,G,F}
    if bridge.ci === nothing
        return bridge.free_rows
    end
    f = MOI.get(model, MOI.ConstraintFunction(), bridge.ci)
    f_s = MOI.Utilities.eachscalar(f)
    func = Vector{eltype(f_s)}(undef, MOI.dimension(bridge.set))
    free_s = MOI.Utilities.eachscalar(bridge.free_rows)
    n_free_rows, n_f_rows, upper_bound_rows = 0, 0, Int[]
    for (row, (l, u)) in enumerate(zip(bridge.set.lower, bridge.set.upper))
        if !isfinite(l) && !isfinite(u)
            n_free_rows += 1
            func[row] = free_s[n_free_rows]
        elseif iszero(l)
            n_f_rows += 1
            func[row] = f_s[n_f_rows]
        elseif isfinite(l)
            n_f_rows += 1
            func[row] = MOI.Utilities.operate(+, T, f_s[n_f_rows], l)
        else
            @assert isfinite(u)
            # This row exists only as u - f, but we don't know where it starts
            # yet because we need to count all the `f - l` rows first.
            push!(upper_bound_rows, row)
        end
    end
    for (row, (l, u)) in enumerate(zip(bridge.set.lower, bridge.set.upper))
        if !isfinite(u)
            continue
        end
        n_f_rows += 1
        if !(row in upper_bound_rows)
            continue
        end
        func[row] = if iszero(bridge.set.upper[row])
            MOI.Utilities.operate(-, T, f_s[n_f_rows])
        else
            MOI.Utilities.operate(-, T, bridge.set.upper[row], f_s[n_f_rows])
        end
    end
    return MOI.Utilities.convert_approx(F, MOI.Utilities.vectorize(func))
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SplitHyperRectangleBridge,
)
    return bridge.set
end

function MOI.delete(model::MOI.ModelLike, bridge::SplitHyperRectangleBridge)
    if bridge.ci !== nothing
        MOI.delete(model, bridge.ci)
    end
    return
end

function MOI.get(
    bridge::SplitHyperRectangleBridge{T,G},
    ::MOI.NumberOfConstraints{G,MOI.Nonnegatives},
)::Int64 where {T,G}
    return ifelse(bridge.ci === nothing, 0, 1)
end

function MOI.get(
    bridge::SplitHyperRectangleBridge{T,G,F},
    ::MOI.ListOfConstraintIndices{G,MOI.Nonnegatives},
) where {T,G,F}
    ret = MOI.ConstraintIndex{G,MOI.Nonnegatives}[]
    if bridge.ci !== nothing
        push!(ret, bridge.ci)
    end
    return ret
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SplitHyperRectangleBridge{T,G}},
) where {T,G}
    return MOI.supports(model, attr, MOI.ConstraintIndex{G,MOI.Nonnegatives})
end

_get_free_start(bridge, ::MOI.ConstraintDualStart) = bridge.free_dual_start

function _set_free_start(bridge, ::MOI.ConstraintDualStart, value)
    bridge.free_dual_start = value
    return
end

_get_free_start(bridge, ::MOI.ConstraintPrimalStart) = bridge.free_primal_start

function _set_free_start(bridge, ::MOI.ConstraintPrimalStart, value)
    bridge.free_primal_start = value
    return
end

# This is a punned overload. We use Union{MOI.ConstraintDual,MOI.ConstraintDualStart}
# in MOI.get, so this hits the ConstraintDual branch. Since no constraints are
# ever added, we just assuem that the dual is `0.0` (this is feasible because)
# the set is really `f(x) in Reals()`, so the dual set is `Zeros()`
function _get_free_start(
    bridge::SplitHyperRectangleBridge{T},
    ::MOI.ConstraintDual,
) where {T}
    return zeros(T, MOI.dimension(bridge.set))
end

# The same cannot be said for ConstraintPrimal because we have no mechanism for
# evaluating the primal of the free rows. Throw an error instead.
function _get_free_start(
    ::SplitHyperRectangleBridge,
    attr::MOI.ConstraintPrimal,
)
    return throw(MOI.GetAttributeNotAllowed(attr))
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SplitHyperRectangleBridge{T},
    value::AbstractVector{T},
) where {T}
    if bridge.ci === nothing
        return _set_free_start(bridge, attr, value)
    end
    new_values = vcat(
        T[v - l for (v, l) in zip(value, bridge.set.lower) if isfinite(l)],
        T[u - v for (v, u) in zip(value, bridge.set.upper) if isfinite(u)],
    )
    MOI.set(model, attr, bridge.ci, new_values)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SplitHyperRectangleBridge{T},
) where {T}
    if bridge.ci === nothing
        return _get_free_start(bridge, attr)
    end
    values = MOI.get(model, attr, bridge.ci)
    if values === nothing
        return nothing
    end
    ret = zeros(T, MOI.dimension(bridge.set))
    row = 0
    for (i, l) in enumerate(bridge.set.lower)
        if isfinite(l)
            row += 1
            ret[i] = l + values[row]
        end
    end
    for (i, u) in enumerate(bridge.set.upper)
        if isfinite(u)
            row += 1
            ret[i] = u - values[row]
        end
    end
    return ret
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::SplitHyperRectangleBridge{T},
    values::AbstractVector{T},
) where {T}
    if bridge.ci === nothing
        return _set_free_start(bridge, attr, values)
    end
    set = bridge.set
    new_values = vcat(
        T[max(T(0), v) for (v, l) in zip(values, set.lower) if isfinite(l)],
        T[max(T(0), -v) for (v, u) in zip(values, set.upper) if isfinite(u)],
    )
    MOI.set(model, attr, bridge.ci, new_values)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SplitHyperRectangleBridge{T},
) where {T}
    if bridge.ci === nothing
        return _get_free_start(bridge, attr)
    end
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
            ret[i] -= values[row]
        end
    end
    return ret
end

function MOI.set(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    bridge::SplitHyperRectangleBridge{T},
    ::Nothing,
) where {T}
    if bridge.ci === nothing
        return _set_free_start(bridge, attr, nothing)
    end
    MOI.set(model, attr, bridge.ci, nothing)
    return
end
