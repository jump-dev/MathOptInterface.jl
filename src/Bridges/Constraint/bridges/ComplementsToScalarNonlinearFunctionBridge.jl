# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ComplementsToScalarNonlinearFunctionBridge{T,F} <:
        Bridges.Constraint.AbstractBridge

`ComplementsToScalarNonlinearFunctionBridge` implements the following
reformulation:

  * ``(F, x) \\in \\textsf{Complements}()`` to
    ```julia
    if isfinite(l)
        (x - l) * F <= 0.0
    else
        1.0 * F <= 0.0
    end
    if isfinite(u)
        (x - u) * F <= 0.0
    else
        -1.0 * F <= 0.0
    end
    ```

## Source node

`ComplementsToScalarNonlinearFunctionBridge` supports:

  * `F` in [`MOI.Complements`](@ref)

## Target nodes

`ComplementsToScalarNonlinearFunctionBridge` creates:

  * [`MOI.ScalarNonlinearFunction`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct ComplementsToScalarNonlinearFunctionBridge{
    T,
    F<:Union{
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    },
} <: AbstractBridge
    f::F
    ci::Vector{MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,MOI.LessThan{T}}}
    bounds::Vector{NTuple{2,T}}
    function ComplementsToScalarNonlinearFunctionBridge{T}(
        f,
        ::MOI.Complements,
    ) where {T}
        ci = MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,MOI.LessThan{T}}[]
        return new{T,typeof(f)}(f, ci, NTuple{2,T}[])
    end
end

const ComplementsToScalarNonlinearFunction{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ComplementsToScalarNonlinearFunctionBridge{T},OT}

function bridge_constraint(
    ::Type{ComplementsToScalarNonlinearFunctionBridge{T,F}},
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
}
    # !!! info
    #     Postpone creation until final_touch.
    return ComplementsToScalarNonlinearFunctionBridge{T}(f, s)
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
    ::Type{<:ComplementsToScalarNonlinearFunctionBridge},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:ComplementsToScalarNonlinearFunctionBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.ScalarNonlinearFunction, MOI.LessThan{T})]
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
    return ComplementsToScalarNonlinearFunctionBridge{T,F}
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
    MOI.delete.(model, bridge.ci)
    empty!(bridge.bounds)
    return
end

function MOI.get(
    ::ComplementsToScalarNonlinearFunctionBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return 0
end

function MOI.get(
    ::ComplementsToScalarNonlinearFunctionBridge,
    ::MOI.ListOfVariableIndices,
)::Vector{MOI.VariableIndex}
    return MOI.VariableIndex[]
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarNonlinearFunction,MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.ci)
end

function MOI.get(
    bridge::ComplementsToScalarNonlinearFunctionBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarNonlinearFunction,MOI.LessThan{T}},
) where {T}
    return copy(bridge.ci)
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
        x = convert(MOI.VariableIndex, f[N+i])
        g_l = if isfinite(l) && iszero(l)
            MOI.ScalarNonlinearFunction(:*, Any[x, F])
        elseif isfinite(l)
            x_l = MOI.ScalarNonlinearFunction(:-, Any[x, l])
            MOI.ScalarNonlinearFunction(:*, Any[x_l, F])
        elseif F isa MOI.ScalarNonlinearFunction
            F
        else
            MOI.ScalarNonlinearFunction(:+, Any[F])
        end
        push!(bridge.ci, MOI.add_constraint(model, g_l, MOI.LessThan(zero(T))))
        g_u = if isfinite(u) && iszero(u)
            MOI.ScalarNonlinearFunction(:*, Any[x, F])
        elseif isfinite(u)
            x_u = MOI.ScalarNonlinearFunction(:-, Any[x, u])
            MOI.ScalarNonlinearFunction(:*, Any[x_u, F])
        else
            MOI.ScalarNonlinearFunction(:*, Any[-one(T), F])
        end
        push!(bridge.ci, MOI.add_constraint(model, g_u, MOI.LessThan(zero(T))))
    end
    return
end
