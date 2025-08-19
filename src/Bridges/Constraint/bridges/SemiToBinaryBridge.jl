# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    SemiToBinaryBridge{T,S} <: Bridges.Constraint.AbstractBridge

`SemiToBinaryBridge` implements the following reformulations:

  * ``x \\in \\{0\\} \\cup [l, u]`` into
    ```math
    \\begin{aligned}
    x \\leq z u \\\\
    x \\geq z l \\\\
    z \\in \\{0, 1\\}
    \\end{aligned}
    ```
  * ``x \\in \\{0\\} \\cup \\{l, \\ldots, u\\}`` into
    ```math
    \\begin{aligned}
    x \\leq z u \\\\
    x \\geq z l \\\\
    z \\in \\{0, 1\\} \\\\
    x \\in \\mathbb{Z}
    \\end{aligned}
    ```

## Source node

`SemiToBinaryBridge` supports:

  * [`MOI.VariableIndex`](@ref) in [`MOI.Semicontinuous{T}`](@ref)
  * [`MOI.VariableIndex`](@ref) in [`MOI.Semiinteger{T}`](@ref)

## Target nodes

`SemiToBinaryBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)
  * [`MOI.VariableIndex{T}`](@ref) in [`MOI.Integer`](@ref) (if `S` is
    [`MOI.Semiinteger{T}`](@ref)
"""
mutable struct SemiToBinaryBridge{
    T,
    S<:Union{MOI.Semicontinuous{T},MOI.Semiinteger{T}},
} <: AbstractBridge
    semi_set::S
    variable::MOI.VariableIndex
    binary_variable::MOI.VariableIndex
    binary_constraint_index::MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}
    lower_bound_index::MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    }
    upper_bound_index::MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{T},
        MOI.LessThan{T},
    }
    integer_index::Union{
        Nothing,
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer},
    }
end

const SemiToBinary{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SemiToBinaryBridge{T},OT}

function bridge_constraint(
    ::Type{SemiToBinaryBridge{T,S}},
    model::MOI.ModelLike,
    f::MOI.VariableIndex,
    s::S,
) where {T<:Real,S<:Union{MOI.Semicontinuous{T},MOI.Semiinteger{T}}}
    binary, binary_con = MOI.add_constrained_variable(model, MOI.ZeroOne())
    # var - LB * bin >= 0
    lb = MOI.Utilities.operate(*, T, -s.lower, binary)
    lb = MOI.Utilities.operate!(+, T, lb, f)
    lb_ci = MOI.add_constraint(model, lb, MOI.GreaterThan{T}(zero(T)))
    # var - UB * bin <= 0
    ub = MOI.Utilities.operate(*, T, -s.upper, binary)
    ub = MOI.Utilities.operate!(+, T, ub, f)
    ub_ci = MOI.add_constraint(model, ub, MOI.LessThan{T}(zero(T)))
    if s isa MOI.Semiinteger{T}
        int_ci = MOI.add_constraint(model, f, MOI.Integer())
    else
        int_ci = nothing
    end
    return SemiToBinaryBridge{T,S}(
        s,
        f,
        binary,
        binary_con,
        lb_ci,
        ub_ci,
        int_ci,
    )
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:SemiToBinaryBridge{T,S}},
) where {T,S}
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SemiToBinaryBridge{T,S}},
) where {T,S<:MOI.Semicontinuous{T}}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
    ]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SemiToBinaryBridge{T,S}},
) where {T,S<:MOI.Semiinteger{T}}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
        (MOI.VariableIndex, MOI.Integer),
    ]
end

function concrete_bridge_type(
    ::Type{<:SemiToBinaryBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {T,S<:Union{MOI.Semicontinuous{T},MOI.Semiinteger{T}}}
    return SemiToBinaryBridge{T,S}
end

function MOI.supports_constraint(
    ::Type{<:SemiToBinaryBridge{T}},
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.Semicontinuous{T},MOI.Semiinteger{T}}},
) where {T}
    return true
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, b::SemiToBinaryBridge)
    return b.semi_set
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::SemiToBinaryBridge{T,S},
    set::S,
) where {T,S}
    bridge.semi_set = set
    MOI.modify(
        model,
        bridge.upper_bound_index,
        MOI.ScalarCoefficientChange(bridge.binary_variable, -set.upper),
    )
    MOI.modify(
        model,
        bridge.lower_bound_index,
        MOI.ScalarCoefficientChange(bridge.binary_variable, -set.lower),
    )
    return
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    b::SemiToBinaryBridge{T},
) where {T}
    return b.variable
end

function MOI.delete(model::MOI.ModelLike, bridge::SemiToBinaryBridge)
    if bridge.integer_index !== nothing
        MOI.delete(model, bridge.integer_index)
    end
    MOI.delete(model, bridge.upper_bound_index)
    MOI.delete(model, bridge.lower_bound_index)
    MOI.delete(model, bridge.binary_variable)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::SemiToBinaryBridge,
)
    return MOI.get(
        model,
        MOI.VariablePrimal(attr.result_index),
        bridge.variable,
    )
end

function MOI.supports(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{SemiToBinaryBridge{T,S}},
) where {T,S}
    ci_1 = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}
    ci_2 = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}
    return MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex) &&
           MOI.supports(model, MOI.ConstraintPrimalStart(), ci_1) &&
           MOI.supports(model, MOI.ConstraintPrimalStart(), ci_2)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    bridge::SemiToBinaryBridge,
)
    return MOI.get(model, MOI.VariablePrimalStart(), bridge.variable)
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    bridge::SemiToBinaryBridge{T},
    value,
) where {T}
    MOI.set(model, MOI.VariablePrimalStart(), bridge.variable, value)
    bin_value = iszero(value) ? zero(T) : one(T)
    MOI.set(model, MOI.VariablePrimalStart(), bridge.binary_variable, bin_value)
    MOI.set(
        model,
        MOI.ConstraintPrimalStart(),
        bridge.upper_bound_index,
        value - bridge.semi_set.upper * bin_value,
    )
    MOI.set(
        model,
        MOI.ConstraintPrimalStart(),
        bridge.lower_bound_index,
        value - bridge.semi_set.lower * bin_value,
    )
    return
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    bridge::SemiToBinaryBridge{T},
    ::Nothing,
) where {T}
    MOI.set(model, MOI.VariablePrimalStart(), bridge.variable, nothing)
    MOI.set(model, MOI.VariablePrimalStart(), bridge.binary_variable, nothing)
    MOI.set(
        model,
        MOI.ConstraintPrimalStart(),
        bridge.upper_bound_index,
        nothing,
    )
    MOI.set(
        model,
        MOI.ConstraintPrimalStart(),
        bridge.lower_bound_index,
        nothing,
    )
    return
end

MOI.get(::SemiToBinaryBridge, ::MOI.NumberOfVariables)::Int64 = 1

function MOI.get(b::SemiToBinaryBridge, ::MOI.ListOfVariableIndices)
    return [b.binary_variable]
end

function MOI.get(
    ::SemiToBinaryBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    ::SemiToBinaryBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.Integer},
)::Int64 where {T,S<:MOI.Semiinteger}
    return 1
end

function MOI.get(
    ::SemiToBinaryBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    ::SemiToBinaryBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    b::SemiToBinaryBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
) where {T,S}
    return [b.binary_constraint_index]
end

function MOI.get(
    b::SemiToBinaryBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer},
) where {T,S<:MOI.Semiinteger}
    return [b.integer_index]
end

function MOI.get(
    b::SemiToBinaryBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T,S}
    return [b.upper_bound_index]
end

function MOI.get(
    b::SemiToBinaryBridge{T,S},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
) where {T,S}
    return [b.lower_bound_index]
end

MOI.Bridges.needs_final_touch(b::SemiToBinaryBridge) = true

function MOI.Bridges.final_touch(
    b::SemiToBinaryBridge{T,S},
    model::MOI.ModelLike,
) where {T,S}
    F, f = MOI.VariableIndex, b.variable
    if MOI.is_valid(model, MOI.ConstraintIndex{F,MOI.GreaterThan{T}}(f.value))
        throw(MOI.LowerBoundAlreadySet{S,MOI.GreaterThan{T}}(f))
    end
    if MOI.is_valid(model, MOI.ConstraintIndex{F,MOI.LessThan{T}}(f.value))
        throw(MOI.UpperBoundAlreadySet{S,MOI.LessThan{T}}(f))
    end
    if MOI.is_valid(model, MOI.ConstraintIndex{F,MOI.EqualTo{T}}(f.value))
        throw(MOI.LowerBoundAlreadySet{S,MOI.EqualTo{T}}(f))
    end
    if MOI.is_valid(model, MOI.ConstraintIndex{F,MOI.Interval{T}}(f.value))
        throw(MOI.LowerBoundAlreadySet{S,MOI.Interval{T}}(f))
    end
    return
end
