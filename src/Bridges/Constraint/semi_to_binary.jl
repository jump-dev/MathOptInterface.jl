const SemiSets{T} = Union{MOI.Semicontinuous{T}, MOI.Semiinteger{T}}

"""
    SemiToBinaryBridge{T, S <: MOI.AbstractScalarSet}

The `SemiToBinaryBridge` replaces an Semicontinuous constraint:
``x \\in \\mathsf{Semicontinuous}(l, u)``
is replaced by:
``z \\in \\{0, 1\\}``,
``x \\leq z \\cdot u ``,
``x \\geq z \\cdot l ``.

The `SemiToBinaryBridge` replaces an Semiinteger constraint:
``x \\in Semiinteger(l, u)``
is replaced by:
``z \\in \\{0, 1\\}``,
``x \\in \\Integer``,
``x \\leq z \\cdot u ``,
``x \\geq z \\cdot l ``.
"""
mutable struct SemiToBinaryBridge{T, S <: SemiSets{T}} <: AbstractBridge
    semi_set::S
    variable_index::MOI.VariableIndex
    binary_variable_index::MOI.VariableIndex
    binary_constraint_index::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}
    lower_bound_index::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}
    upper_bound_index::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
    integer_index::Union{Nothing, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}}
end

function bridge_constraint(::Type{SemiToBinaryBridge{T,S}}, model::MOI.ModelLike, f::MOI.SingleVariable, s::S) where {T <: Real, S<:SemiSets{T}}
    binary, binary_con = MOI.add_constrained_variable(model, MOI.ZeroOne())

    # var - LB * bin >= 0
    lb = MOIU.operate(*, T, -s.lower, MOI.SingleVariable(binary))
    lb = MOIU.operate!(+, T, lb, f)
    lb_ci = MOI.add_constraint(model, lb, MOI.GreaterThan{T}(zero(T)))

    # var - UB * bin <= 0
    ub = MOIU.operate(*, T, -s.upper, MOI.SingleVariable(binary))
    ub = MOIU.operate!(+, T, ub, f)
    ub_ci = MOI.add_constraint(model, ub, MOI.LessThan{T}(zero(T)))

    if s isa MOI.Semiinteger{T}
        int_ci = MOI.add_constraint(model, f, MOI.Integer())
    else
        int_ci = nothing
    end

    return SemiToBinaryBridge{T,S}(s, f.variable, binary, binary_con, lb_ci, ub_ci, int_ci)
end

MOIB.watched_variables(bridge::SemiToBinaryBridge) = [bridge.variable_index]
function MOIB.notify_constraint(
    bridge::SemiToBinaryBridge{T, S}, model::MOI.ModelLike,
    func::MOI.SingleVariable, set::MOI.AbstractScalarSet
) where {T, S}
    mask = MOIU.single_variable_flag(S)
    MOIU.throw_if_lower_bound_set(func.variable, typeof(set), mask, T)
    MOIU.throw_if_upper_bound_set(func.variable, typeof(set), mask, T)
end

function MOIB.added_constrained_variable_types(::Type{<:SemiToBinaryBridge{T, S}}) where {T, S}
    return [(MOI.ZeroOne,)]
end

function MOIB.added_constraint_types(::Type{<:SemiToBinaryBridge{T, S}}) where {T, S<:MOI.Semicontinuous{T}}
    return [
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
            ]
end

function MOIB.added_constraint_types(::Type{<:SemiToBinaryBridge{T, S}}) where {T, S <: MOI.Semiinteger{T}}
    return [
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
            (MOI.SingleVariable, MOI.Integer),
            ]
end

function concrete_bridge_type(::Type{<:SemiToBinaryBridge{T}},
                              ::Type{MOI.SingleVariable},
                              ::Type{S}) where {T, S<:SemiSets}
    return SemiToBinaryBridge{T, S}
end

function MOI.supports_constraint(::Type{<:SemiToBinaryBridge},
                                 ::Type{MOI.SingleVariable},
                                 ::Type{<:SemiSets})
    return true
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintSet,
                 b::SemiToBinaryBridge)
    return b.semi_set
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintSet,
    bridge::SemiToBinaryBridge{T, S}, set::S) where {T, S}
    bridge.semi_set = set
    MOI.modify(model, bridge.upper_bound_index,
        MOI.ScalarCoefficientChange(bridge.binary_variable_index, -set.upper))
    MOI.modify(model, bridge.lower_bound_index,
        MOI.ScalarCoefficientChange(bridge.binary_variable_index, -set.lower))
    return
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 b::SemiToBinaryBridge{T}) where {T}
    return MOI.SingleVariable(b.variable_index)
end

function MOI.delete(model::MOI.ModelLike, bridge::SemiToBinaryBridge)
    if bridge.integer_index !== nothing
        MOI.delete(model, bridge.integer_index)
    end
    MOI.delete(model, bridge.upper_bound_index)
    MOI.delete(model, bridge.lower_bound_index)
    MOI.delete(model, bridge.binary_constraint_index)
    MOI.delete(model, bridge.binary_variable_index)
    return
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
    bridge::SemiToBinaryBridge)
    MOI.get(model, MOI.VariablePrimal(attr.N), bridge.variable_index)
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:SemiToBinaryBridge})
    return true
end

function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart, bridge::SemiToBinaryBridge)
    return MOI.get(model, MOI.VariablePrimalStart(), bridge.variable_index)
end

function MOI.set(model::MOI.ModelLike, attr::MOI.ConstraintPrimalStart,
                 bridge::SemiToBinaryBridge{T}, value) where {T}
    MOI.set(model, MOI.VariablePrimalStart(), bridge.variable_index, value)
    bin_value = ifelse(iszero(value), 0.0, 1.0)
    MOI.set(model, MOI.VariablePrimalStart(), bridge.binary_variable_index, bin_value)
    MOI.set(model, MOI.ConstraintPrimalStart(),
        bridge.upper_bound_index, value - bridge.semi_set.upper * bin_value)
    MOI.set(model, MOI.ConstraintPrimalStart(),
        bridge.lower_bound_index, value - bridge.semi_set.lower * bin_value)
    return
end

# Attributes, Bridge acting as a model

function MOI.get(::SemiToBinaryBridge, ::MOI.NumberOfVariables)
    return 1
end

function MOI.get(b::SemiToBinaryBridge, ::MOI.ListOfVariableIndices)
    return [b.binary_variable_index]
end

function MOI.get(::SemiToBinaryBridge{T, S},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.ZeroOne}) where {T, S}
    return 1
end

function MOI.get(::SemiToBinaryBridge{T, S},
    ::MOI.NumberOfConstraints{MOI.SingleVariable, MOI.Integer}) where {T, S<:MOI.Semiinteger}
    return 1
end

function MOI.get(::SemiToBinaryBridge{T, S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T, S}
    return 1
end

function MOI.get(::SemiToBinaryBridge{T, S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T, S}
    return 1
end

function MOI.get(b::SemiToBinaryBridge{T, S},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.ZeroOne}) where {T, S}
    return [b.binary_constraint_index]
end

function MOI.get(b::SemiToBinaryBridge{T, S},
    ::MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer}) where {T, S<:MOI.Semiinteger}
    return [b.integer_index]
end

function MOI.get(b::SemiToBinaryBridge{T, S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T, S}
    return [b.upper_bound_index]
end

function MOI.get(b::SemiToBinaryBridge{T, S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T, S}
    return [b.lower_bound_index]
end
