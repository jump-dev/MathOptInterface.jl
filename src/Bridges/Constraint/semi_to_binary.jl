const _SemiSets{T} = Union{MOI.Semicontinuous{T},MOI.Semiinteger{T}}

"""
    SemiToBinaryBridge{T, S <: MOI.AbstractScalarSet}

The `SemiToBinaryBridge` replaces a Semicontinuous constraint:
``x \\in \\mathsf{Semicontinuous}(l, u)``
is replaced by:
``z \\in \\{0, 1\\}``,
``x \\leq z \\cdot u ``,
``x \\geq z \\cdot l ``.

The `SemiToBinaryBridge` replaces a Semiinteger constraint:
``x \\in Semiinteger(l, u)``
is replaced by:
``z \\in \\{0, 1\\}``,
``x \\in \\mathbb{Z}``,
``x \\leq z \\cdot u ``,
``x \\geq z \\cdot l ``.
"""
mutable struct SemiToBinaryBridge{T,S<:_SemiSets{T}} <: AbstractBridge
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

function bridge_constraint(
    ::Type{SemiToBinaryBridge{T,S}},
    model::MOI.ModelLike,
    f::MOI.VariableIndex,
    s::S,
) where {T<:Real,S<:_SemiSets{T}}
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
) where {T,S<:_SemiSets}
    return SemiToBinaryBridge{T,S}
end

function MOI.supports_constraint(
    ::Type{<:SemiToBinaryBridge},
    ::Type{MOI.VariableIndex},
    ::Type{<:_SemiSets},
)
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
    MOI.delete(model, bridge.binary_constraint_index)
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
    bin_value = iszero(value) ? 0.0 : 1.0
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
