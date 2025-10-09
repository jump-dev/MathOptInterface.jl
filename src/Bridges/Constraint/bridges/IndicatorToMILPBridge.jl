# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    IndicatorToMILPBridge{T,F,A,S} <: Bridges.Constraint.AbstractBridge

`IndicatorToMILPBridge` implements the following reformulation:

  * ``x \\in \\textsf{Indicator}(s)`` into a mixed-integer linear program.

## Source node

`IndicatorToMILPBridge` supports:

  * `F` in [`MOI.Indicator{A,S}`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`IndicatorToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in `S`
"""
mutable struct IndicatorToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
    A,
    S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}},
} <: AbstractBridge
    f::F
    s::MOI.Indicator{A,S}
    slack::Union{Nothing,MOI.VariableIndex}
    constraint::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}
    slack_bounds::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    bounds::NTuple{2,T}
    function IndicatorToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
        s::MOI.Indicator{A,S},
    ) where {T,A,S}
        return new{T,typeof(f),A,S}(
            f,
            s,
            nothing,
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}(0),
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
            (typemin(T), typemax(T)),
        )
    end
end

const IndicatorToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{IndicatorToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{IndicatorToMILPBridge{T,F,A,S}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Indicator{A,S},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},A,S}
    # !!! info
    #     Postpone rest of creation until final_touch.
    return IndicatorToMILPBridge{T}(f, s)
end

function MOI.supports_constraint(
    ::Type{<:IndicatorToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{<:MOI.Indicator{A,S}},
) where {T,A,S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}}}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:IndicatorToMILPBridge},
)
    return Tuple{Type}[(MOI.Reals,), (MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:IndicatorToMILPBridge{T,F,A,S}},
) where {T,F,A,S}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, S),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:IndicatorToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.Indicator{A,S}},
) where {
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
    A,
    S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T}},
}
    return IndicatorToMILPBridge{T,F,A,S}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::IndicatorToMILPBridge,
)
    return copy(bridge.f)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::IndicatorToMILPBridge,
)
    return bridge.s
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::IndicatorToMILPBridge{T},
) where {T}
    if bridge.slack === nothing
        return  # We're deleting the bridge before final_touch
    end
    MOI.delete.(model, bridge.slack_bounds)
    MOI.delete(model, bridge.constraint)
    MOI.delete(model, bridge.slack::MOI.VariableIndex)
    bridge.slack = nothing
    empty(bridge.slack_bounds)
    bridge.bounds = (typemin(T), typemax(T))
    return
end

MOI.get(bridge::IndicatorToMILPBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(bridge::IndicatorToMILPBridge, ::MOI.ListOfVariableIndices)
    return [bridge.slack::MOI.VariableIndex]
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},S},
)::Int64 where {T,F,A,S<:Union{MOI.GreaterThan,MOI.EqualTo}}
    return 1
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},S},
) where {T,F,A,S}
    return [bridge.constraint]
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,<:Union{MOI.GreaterThan,MOI.EqualTo}},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T,F,A}
    return length(bridge.slack_bounds)
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,<:Union{MOI.GreaterThan,MOI.EqualTo}},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T,F,A}
    return copy(bridge.slack_bounds)
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,MOI.LessThan{T}},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T,F,A}
    return 1 + length(bridge.slack_bounds)
end

function MOI.get(
    bridge::IndicatorToMILPBridge{T,F,A,MOI.LessThan{T}},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T,F,A}
    return vcat(bridge.constraint, bridge.slack_bounds)
end

MOI.Bridges.needs_final_touch(::IndicatorToMILPBridge) = true

function _is_binary(model::MOI.ModelLike, f::MOI.AbstractScalarFunction)
    x = convert(MOI.VariableIndex, f)
    return _is_binary(model, x)
end

function _is_binary(model::MOI.ModelLike, x::MOI.VariableIndex)
    return MOI.is_valid(
        model,
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value),
    )
end

function MOI.Bridges.final_touch(
    bridge::IndicatorToMILPBridge{T,F,A,S},
    model::MOI.ModelLike,
) where {T,F,A,S}
    bounds = Dict{MOI.VariableIndex,NTuple{2,T}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    fi = scalars[2]
    ret = MOI.Utilities.get_bounds(model, bounds, fi)
    if ret === nothing
        throw(MOI.Bridges.BridgeRequiresFiniteDomainError(bridge, fi))
    elseif !_is_binary(model, scalars[1])
        msg = "Unable to reformulate indicator constraint to a MILP. The indicator variable must be binary."
        throw(MOI.AddConstraintNotAllowed{F,MOI.Indicator{A,S}}(msg))
    end
    if bridge.slack === nothing
        # This is the first time calling final_touch
        bridge.bounds = ret
    elseif bridge.bounds == ret
        # We've called final_touch before, and the bounds match. No need to
        # reformulate a second time.
        return
    elseif bridge.bounds != ret
        # There is a stored bound, and the current bounds do not match. This
        # means the model has been modified since the previous call to
        # final_touch. We need to delete the bridge and start again.
        MOI.delete(model, bridge)
        MOI.Bridges.final_touch(bridge, model)
        return
    end
    bridge.slack = MOI.add_variable(model)
    bridge.constraint = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate(+, T, fi, bridge.slack),
        bridge.s.set;
        allow_modify_function = true,
    )
    _bound_slack(model, bridge, scalars[1], bridge.s)
    return
end

function _to_rhs(A::MOI.ActivationCondition, ::Type{T}, z) where {T}
    if A == MOI.ACTIVATE_ON_ZERO
        return z
    end
    return MOI.Utilities.operate(-, T, one(T), z)
end

function _bound_slack(
    model::MOI.ModelLike,
    bridge::IndicatorToMILPBridge{T},
    z,
    set::MOI.Indicator{A,MOI.GreaterThan{T}},
) where {T,A}
    # {f(x) + y >= b} => {y <= (b - ⌊f(x)⌋)₊ * rhs}
    M = max(zero(T), set.set.lower - bridge.bounds[1])
    c = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate(-, T, bridge.slack, M * _to_rhs(A, T, z)),
        MOI.LessThan(zero(T));
        allow_modify_function = true,
    )
    push!(bridge.slack_bounds, c)
    return
end

function _bound_slack(
    model::MOI.ModelLike,
    bridge::IndicatorToMILPBridge{T},
    z,
    set::MOI.Indicator{A,MOI.LessThan{T}},
) where {T,A}
    # {f(x) + y <= b} => {y >= (b - ⌈f(x)⌉)₋ * rhs}
    M = min(zero(T), set.set.upper - bridge.bounds[2])
    c = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate!(-, T, M * _to_rhs(A, T, z), bridge.slack),
        MOI.LessThan(zero(T));
        allow_modify_function = true,
    )
    push!(bridge.slack_bounds, c)
    return
end

function _bound_slack(
    model::MOI.ModelLike,
    bridge::IndicatorToMILPBridge{T},
    z,
    set::MOI.Indicator{A,MOI.EqualTo{T}},
) where {T,A}
    b = set.set.value
    _bound_slack(model, bridge, z, MOI.Indicator{A}(MOI.LessThan(b)))
    _bound_slack(model, bridge, z, MOI.Indicator{A}(MOI.GreaterThan(b)))
    return
end
