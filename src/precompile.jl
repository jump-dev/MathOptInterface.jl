function precompile_constraint(model, F, S)
    Base.precompile(get, (model, ListOfConstraintIndices{F,S}))
    Base.precompile(get, (model, ListOfConstraintAttributesSet{F,S}))
    Base.precompile(get, (model, ConstraintSet, ConstraintIndex{F,S}))
    Base.precompile(get, (model, ConstraintSet, Vector{ConstraintIndex{F,S}}))
    Base.precompile(set, (model, ConstraintSet, ConstraintIndex{F,S}, S))

    Base.precompile(get, (model, ConstraintFunction, ConstraintIndex{F,S}))
    Base.precompile(set, (model, ConstraintFunction, ConstraintIndex{F,S}, F))
    Base.precompile(get, (model, ConstraintFunction, Vector{ConstraintIndex{F,S}}))

    Base.precompile(get, (model, ConstraintDual, ConstraintIndex{F,S}))
    Base.precompile(get, (model, ConstraintPrimal, ConstraintIndex{F,S}))

    Base.precompile(get, (model, AbstractConstraintAttribute, ConstraintIndex{F,S}))
    Base.precompile(get, (model, AbstractConstraintAttribute, Vector{ConstraintIndex{F,S}}))

    Base.precompile(add_constraint, (model, F, S))
    Base.precompile(add_constraints, (model, Vector{F}, Vector{S}))
    Base.precompile(delete, (model, ConstraintIndex{F,S}))
    Base.precompile(is_valid, (model, ConstraintIndex{F,S}))
    Base.precompile(get, (model, ConstraintName, ConstraintIndex{F,S}))
    Base.precompile(set, (model, ConstraintName, ConstraintIndex{F,S}, String))
end

function precompile_variables(model)
    Base.precompile(delete, (model, VariableIndex))
    Base.precompile(delete, (model, Vector{VariableIndex}))
    Base.precompile(get, (model, AbstractVariableAttribute, VariableIndex))
    Base.precompile(get, (model, AbstractVariableAttribute, Vector{VariableIndex}))
    Base.precompile(get, (model, VariableName, VariableIndex))
    Base.precompile(set, (model, VariableName, VariableIndex, String))
    Base.precompile(get, (model, VariablePrimalStart, VariableIndex))
    Base.precompile(set, (model, VariablePrimalStart, VariableIndex, Float64))
    Base.precompile(get, (model, VariablePrimalStart, Vector{VariableIndex}))
    Base.precompile(set, (model, VariablePrimalStart, Vector{VariableIndex}, Vector{Float64}))
    Base.precompile(get, (model, VariablePrimal, VariableIndex))
    Base.precompile(get, (model, VariablePrimal, Vector{VariableIndex}))
    Base.precompile(add_constrained_variables, (model, Reals))
end

function precompile_model(model, constraints)
    Base.precompile(empty!, (model,))
    Base.precompile(is_empty, (model,))
    Base.precompile(get, (model, ListOfConstraints))
    Base.precompile(optimize!, (model,))
    Base.precompile(add_variable, (model,))
    Base.precompile(add_variables, (model, Int))
    for attr in (
        ListOfVariableIndices,
        ListOfVariableAttributesSet,
        TerminationStatus,
        DualStatus,
        PrimalStatus,
        ObjectiveValue,
        Silent,
        TimeLimitSec,
        NumberOfVariables,
    )
        Base.precompile(get, (model, attr))
    end

    precompile_variables(model)
    for (F, S) in constraints
        precompile_constraint(model, F, S)
    end
    Base.precompile(Tuple{typeof(add_constrained_variables),model,Reals})
end
