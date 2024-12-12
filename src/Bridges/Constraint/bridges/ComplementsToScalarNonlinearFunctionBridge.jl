# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ComplementsToScalarNonlinearFunctionBridge{T,F,G} <:
        Bridges.Constraint.AbstractBridge

`ComplementsToScalarNonlinearFunctionBridge` implements the following
reformulation:

  * ``(F, x) \\in \\textsf{Complements}()`` to
    ``y - F = 0`` and
    ```julia
    if isfinite(l)
        (x - l) * y <= 0.0
    else
        y <= 0
    end
    if isfinite(u)
        (x - u) * y <= 0.0
    else
        y >= 0
    end
    ```

## Source node

`ComplementsToScalarNonlinearFunctionBridge` supports:

  * `F` in [`MOI.Complements`](@ref)

## Target nodes

`ComplementsToScalarNonlinearFunctionBridge` creates:

  * `G` in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarQuadraticFunction`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.Interval{T}`](@ref)
"""
mutable struct ComplementsToScalarNonlinearFunctionBridge{
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
    G,
} <: AbstractBridge
    f::F
    y::Vector{MOI.VariableIndex}
    ci_eq::Vector{MOI.ConstraintIndex{G,MOI.EqualTo{T}}}
    ci_lt::Vector{
        MOI.ConstraintIndex{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}},
    }
    bounds::Vector{NTuple{2,T}}

    function ComplementsToScalarNonlinearFunctionBridge{T,F,G}(
        f::F,
        ::MOI.Complements,
    ) where {T,F,G}
        return new{T,F,G}(
            f,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{G,MOI.EqualTo{T}}[],
            MOI.ConstraintIndex{
                MOI.ScalarQuadraticFunction{T},
                MOI.LessThan{T},
            }[],
            NTuple{2,T}[],
        )
    end
end

const ComplementsToScalarNonlinearFunction{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ComplementsToScalarNonlinearFunctionBridge{T},OT}

function bridge_constraint(
    ::Type{ComplementsToScalarNonlinearFunctionBridge{T,F,G}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Complements,
) where {
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
    G,
}
    # !!! info
    #     Postpone creation until final_touch.
    return ComplementsToScalarNonlinearFunctionBridge{T,F,G}(f, s)
end

function MOI.supports_constraint(
    ::Type{<:ComplementsToScalarNonlinearFunctionBridge{T}},
    ::Type{F},
    ::Type{MOI.Complements},
) where {
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{ComplementsToScalarNonlinearFunctionBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type}[(MOI.Interval{T},)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{ComplementsToScalarNonlinearFunctionBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[
        (G, MOI.EqualTo{T}),
        (MOI.ScalarQuadraticFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:ComplementsToScalarNonlinearFunctionBridge{T}},
    ::Type{F},
    ::Type{MOI.Complements},
) where {
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
}
    G = MOI.Utilities.promote_operation(
        -,
        T,
        MOI.Utilities.scalar_type(F),
        MOI.VariableIndex,
    )
    return ComplementsToScalarNonlinearFunctionBridge{T,F,G}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ComplementsToScalarNonlinearFunctionBridge,
)
    return copy(bridge.f)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ComplementsToScalarNonlinearFunctionBridge,
)
    n = MOI.output_dimension(bridge.f)
    return MOI.Complements(n)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ComplementsToScalarNonlinearFunctionBridge,
)
    MOI.delete.(model, bridge.y)
    MOI.delete.(model, bridge.ci_eq)
    MOI.delete.(model, bridge.ci_lt)
    empty!(bridge.bounds)
    return
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.y)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    return copy(bridge.y)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.EqualTo{T}},
)::Int64 where {T,F,G}
    return length(bridge.ci_eq)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.EqualTo{T}},
) where {T,F,G}
    return copy(bridge.ci_eq)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.ci_lt)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    },
) where {T}
    return copy(bridge.ci_lt)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Interval{T}},
)::Int64 where {T}
    return length(bridge.y)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Interval{T}},
) where {T}
    return map(bridge.y) do y
        return MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{T}}(y.value)
    end
end

function MOI.Bridges.needs_final_touch(
    ::ComplementsToScalarNonlinearFunctionBridge,
)
    return true
end

function MOI.Bridges.final_touch(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    model::MOI.ModelLike,
) where {T}
    f = collect(MOI.Utilities.eachscalar(bridge.f))
    N = div(length(f), 2)
    final_touch_called_previously = isempty(bridge.bounds)
    for i in 1:N
        x = convert(MOI.VariableIndex, f[i+N])
        ret = MOI.Utilities.get_bounds(model, T, x)
        ret = something(ret, (typemin(T), typemax(T)))
        if length(bridge.bounds) < i
            # This is the first time calling final_touch
            push!(bridge.bounds, ret)
        elseif bridge.bounds[i] == ret
            # We've called final_touch before, and the bounds match. No need to
            # reformulate a second time.
            continue
        elseif bridge.bounds[i] != ret
            # There is a stored bound, and the current bounds do not match. This
            # means the model has been modified since the previous call to
            # final_touch. We need to delete the bridge and start again.
            MOI.delete(model, bridge)
            MOI.Bridges.final_touch(bridge, model)
            return
        end
    end
    if final_touch_called_previously
        return  # Nothing to be done
    end
    for i in 1:N
        (l, u), F = bridge.bounds[i], f[i]
        y_u = isfinite(l) ? typemax(T) : zero(T)
        y_l = isfinite(u) ? typemin(T) : zero(T)
        y, _ = MOI.add_constrained_variable(model, MOI.Interval{T}(y_l, y_u))
        push!(bridge.y, y)
        # F(x) - y = 0
        g = MOI.Utilities.operate(-, T, F, y)
        push!(bridge.ci_eq, MOI.add_constraint(model, g, MOI.EqualTo(zero(T))))
        x = convert(MOI.VariableIndex, f[N+i])
        # (x - b) * y <= 0
        for b in (l, u)
            if isfinite(b)
                x_less_b = MOI.Utilities.operate(-, T, x, b)
                h = MOI.Utilities.operate(*, T, x_less_b, y)
                ci = MOI.add_constraint(model, h, MOI.LessThan(zero(T)))
                push!(bridge.ci_lt, ci)
            end
        end
    end
    return
end
