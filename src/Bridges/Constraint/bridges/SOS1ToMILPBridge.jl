# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SOS1ToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`SOS1ToMILPBridge` implements the following reformulation:

  * ``x \\in \\textsf{SOS1}(d)`` into the following mixed-integer linear program.

Assuming ``l_i \\le x_i \\le u_i`` with finite bounds ``l_i``, ``u_i``:
```math
\\begin{aligned}
z_i \\in \\{0, 1\\} &  \\;\\; \\forall i \\in 1\\ldots d            \\\\
l_i z_i \\le x_i \\le u_i z_i &  \\;\\; \\forall i \\in 1\\ldots d  \\\\
\\sum z_i = 1
\\end{aligned}
```

## Source node

`SOS1ToMILPBridge` supports:

  * `F` in [`MOI.SOS1`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`SOS1ToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct SOS1ToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    s::MOI.SOS1{T}
    variables::Vector{MOI.VariableIndex}
    # ∑_i z_i == 1
    equal_to::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}
    # x_i - u_i z_i <= 0 ∀i
    # l_i z_i - x_i <= 0 ∀i
    less_than::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    bounds::Vector{NTuple{2,T}}
    function SOS1ToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
        s::MOI.SOS1{T},
    ) where {T}
        return new{T,typeof(f)}(
            f,
            s,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}(0),
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
            NTuple{2,T}[],
        )
    end
end

const SOS1ToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOS1ToMILPBridge{T},OT}

# We need to weight this bridge such that `VectorAffineFunction-in-SOS1` is
# bridged (if possible) by `VectorSlack + VectorOfVariables-in-SOS1`.
#
# See MathOptInterface#2722 for details.
MOI.Bridges.bridging_cost(::Type{<:SOS1ToMILPBridge}) = 10.0

function bridge_constraint(
    ::Type{SOS1ToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.SOS1,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    # !!! info
    #     Postpone rest of creation until final_touch.
    return SOS1ToMILPBridge{T}(f, s)
end

function MOI.supports_constraint(
    ::Type{<:SOS1ToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.SOS1{T}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SOS1ToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SOS1ToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:SOS1ToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.SOS1{T}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return SOS1ToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::SOS1ToMILPBridge,
)
    return copy(bridge.f)
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::SOS1ToMILPBridge)
    return bridge.s
end

function MOI.delete(model::MOI.ModelLike, bridge::SOS1ToMILPBridge)
    if isempty(bridge.variables)
        return  # We're deleting the bridge before final_touch
    end
    MOI.delete(model, bridge.equal_to)
    for ci in bridge.less_than
        MOI.delete(model, ci)
    end
    empty!(bridge.less_than)
    for x in bridge.variables
        MOI.delete(model, x)
    end
    empty!(bridge.variables)
    empty!(bridge.bounds)
    return
end

function MOI.get(bridge::SOS1ToMILPBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::SOS1ToMILPBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::SOS1ToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::SOS1ToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value) for
        x in bridge.variables
    ]
end

function MOI.get(
    bridge::SOS1ToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::SOS1ToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return [bridge.equal_to]
end

function MOI.get(
    bridge::SOS1ToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.less_than)
end

function MOI.get(
    bridge::SOS1ToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.less_than)
end

MOI.Bridges.needs_final_touch(::SOS1ToMILPBridge) = true

function MOI.Bridges.final_touch(
    bridge::SOS1ToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    bounds = Dict{MOI.VariableIndex,NTuple{2,T}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    new_bounds = false
    for (i, x) in enumerate(scalars)
        ret = MOI.Utilities.get_bounds(model, bounds, x)
        if ret === nothing
            throw(MOI.Bridges.BridgeRequiresFiniteDomainError(bridge, x))
        end
        if length(bridge.bounds) < i
            # This is the first time calling final_touch
            push!(bridge.bounds, ret)
            new_bounds = true
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
    if new_bounds === false
        return  # Already called
    end
    terms = MOI.ScalarAffineTerm{T}[]
    for i in 1:MOI.output_dimension(bridge.f)
        z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
        push!(bridge.variables, z)
        push!(terms, MOI.ScalarAffineTerm(one(T), z))
    end
    g = MOI.ScalarAffineFunction(terms, zero(T))
    bridge.equal_to = MOI.add_constraint(model, g, MOI.EqualTo(one(T)))
    for (fi, (l, u), z) in zip(scalars, bridge.bounds, bridge.variables)
        push!(
            bridge.less_than,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate!(-, T, l * z, fi),
                MOI.LessThan(zero(T));
                allow_modify_function = true,
            ),
        )
        push!(
            bridge.less_than,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate(-, T, fi, u * z),
                MOI.LessThan(zero(T));
                allow_modify_function = true,
            ),
        )
    end
    return
end
