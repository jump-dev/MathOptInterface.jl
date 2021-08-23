# An Int-valued attribute
struct MockModelAttribute <: MOI.AbstractModelAttribute end

# An Int-valued attribute
struct MockVariableAttribute <: MOI.AbstractVariableAttribute end

# An Int-valued attribute
struct MockConstraintAttribute <: MOI.AbstractConstraintAttribute end

# A mock optimizer used for testing.
"""
    MockOptimizer

`MockOptimizer` is a fake optimizer especially useful for testing. Its main
feature is that it can store the values that should be returned for each
attribute.
"""
mutable struct MockOptimizer{MT<:MOI.ModelLike} <: MOI.AbstractOptimizer
    inner_model::MT
    # Flags
    supports_names::Bool # Allows to test with optimizer not supporting names
    add_var_allowed::Bool # If false, the optimizer throws AddVariableNotAllowed
    add_con_allowed::Bool # If false, the optimizer throws AddConstraintNotAllowed
    modify_allowed::Bool # If false, the optimizer throws Modify...NotAllowed
    delete_allowed::Bool # If false, the optimizer throws DeleteNotAllowed
    scalar_function_constant_non_zero::Bool
    # Computes `ObjectiveValue` by evaluating the `ObjectiveFunction` with
    # `VariablePrimal`. See `get_fallback`.
    eval_objective_value::Bool
    # Computes `DualObjectiveValue` using `get_fallback`
    eval_dual_objective_value::Bool
    # Computes `ConstraintDual` of constraints with `SingleVariable` or
    # `VectorOfVariables` functions by evaluating the `ConstraintDual` of
    # constraints having the variable in the function. See `get_fallback`.
    eval_variable_constraint_dual::Bool
    # Attributes
    # The attributes set by `MOI.optimize!` cannot be set to `model`.
    # We detect them with `is_set_by_optimize` and store them in the following:
    optimizer_attributes::Dict{MOI.AbstractOptimizerAttribute,Any}
    model_attributes::Dict{MOI.AbstractModelAttribute,Any}
    submitted::Dict{MOI.AbstractSubmittable,Vector{Tuple}}
    mock_model_attribute::Int
    mock_variable_attribute::Dict{MOI.VariableIndex,Int}
    mock_constraint_attribute::Dict{MOI.ConstraintIndex,Int}
    #
    optimize!::Function
    optimize_called::Bool
    termination_status::MOI.TerminationStatusCode
    result_count::Int
    objective_value::Dict{Int,Float64}
    dual_objective_value::Dict{Int,Float64}
    # Primal solution
    primal_status::Dict{Int,MOI.ResultStatusCode}
    variable_primal::Dict{MOI.VariableIndex,Dict{Int,Float64}}
    callback_variable_primal::Dict{MOI.VariableIndex,Float64}
    # Dual solution
    dual_status::Dict{Int,MOI.ResultStatusCode}
    constraint_dual::Dict{MOI.ConstraintIndex,Dict{Int,Any}}
    # Constraint conflicts
    compute_conflict_called::Bool
    conflict_status::MOI.ConflictStatusCode
    constraint_conflict_status::Dict{
        MOI.ConstraintIndex,
        MOI.ConflictParticipationStatusCode,
    }
    # Basis status
    constraint_basis_status::Dict{
        MOI.ConstraintIndex,
        Dict{Int,MOI.BasisStatusCode},
    }
    variable_basis_status::Dict{MOI.VariableIndex,Dict{Int,MOI.BasisStatusCode}}
end

function MockOptimizer(
    inner_model::MOI.ModelLike;
    supports_names = true,
    add_var_allowed = true,
    add_con_allowed = true,
    eval_objective_value = true,
    eval_dual_objective_value = true,
    eval_variable_constraint_dual = true,
    scalar_function_constant_non_zero = false,
)
    return MockOptimizer(
        inner_model,
        # Flags
        supports_names,
        add_var_allowed,
        add_con_allowed,
        true,
        true,
        scalar_function_constant_non_zero,
        eval_objective_value,
        eval_dual_objective_value,
        eval_variable_constraint_dual,
        # Attributes
        Dict{MOI.AbstractOptimizerAttribute,Any}(),
        Dict{MOI.AbstractModelAttribute,Any}(),
        Dict{MOI.AbstractSubmittable,Vector{Tuple}}(),
        0,
        Dict{MOI.VariableIndex,Int}(),
        Dict{MOI.ConstraintIndex,Int}(),
        #
        (::MockOptimizer) -> nothing,
        false,
        MOI.OPTIMIZE_NOT_CALLED,
        1,
        Dict{Int,Float64}(),
        Dict{Int,Float64}(),
        # PrimalStatus
        Dict{Int,MOI.ResultStatusCode}(),
        Dict{MOI.VariableIndex,Dict{Int,Float64}}(),
        Dict{MOI.VariableIndex,Float64}(),
        # DualStatus
        Dict{Int,MOI.ResultStatusCode}(),
        Dict{MOI.ConstraintIndex,Dict{Int,Any}}(),
        #
        false,
        MOI.COMPUTE_CONFLICT_NOT_CALLED,
        Dict{MOI.ConstraintIndex,MOI.ConflictParticipationStatusCode}(),
        # Basis status
        Dict{MOI.ConstraintIndex,Dict{Int,MOI.BasisStatusCode}}(),
        Dict{MOI.VariableIndex,Dict{Int,MOI.BasisStatusCode}}(),
    )
end

"""
All user-facing indices are xor'd with this mask to produce unusual indices.
This is good at catching bugs in solvers which assume indices are ordered 1, 2,
3, ...
"""
const _INTERNAL_XOR_MASK = Int64(12345678)

xor_index(vi::VI) = VI(xor(vi.value, _INTERNAL_XOR_MASK))
xor_index(ci::CI{F,S}) where {F,S} = CI{F,S}(xor(ci.value, _INTERNAL_XOR_MASK))
xor_indices(x) = map_indices(xor_index, x)

function MOI.add_variable(mock::MockOptimizer)
    if mock.add_var_allowed
        return xor_index(MOI.add_variable(mock.inner_model))
    else
        throw(MOI.AddVariableNotAllowed())
    end
end

function MOI.add_constraint(
    mock::MockOptimizer,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    if mock.add_con_allowed
        ci = MOI.add_constraint(mock.inner_model, xor_indices(func), set)
        return xor_index(ci)
    else
        throw(MOI.AddConstraintNotAllowed{typeof(func),typeof(set)}())
    end
end

function MOI.add_constraint(
    mock::MockOptimizer,
    func::MOI.ScalarAffineFunction{T},
    set::MOI.AbstractSet,
) where {T}
    if !mock.add_con_allowed
        throw(MOI.AddConstraintNotAllowed{typeof(func),typeof(set)}())
    elseif mock.scalar_function_constant_non_zero && !iszero(func.constant)
        throw(
            MOI.ScalarFunctionConstantNotZero{T,typeof(func),typeof(set)}(
                func.constant,
            ),
        )
    end
    ci = MOI.add_constraint(mock.inner_model, xor_indices(func), set)
    return xor_index(ci)
end

function MOI.add_constraint(
    mock::MockOptimizer,
    func::MOI.SingleVariable,
    set::MOI.AbstractSet,
)
    if !mock.add_con_allowed
        throw(MOI.AddConstraintNotAllowed{typeof(func),typeof(set)}())
    end
    try
        ci = MOI.add_constraint(mock.inner_model, xor_indices(func), set)
        return xor_index(ci)
    catch err
        if (err isa MOI.LowerBoundAlreadySet) ||
           (err isa MOI.UpperBoundAlreadySet)
            throw(typeof(err)(xor_index(err.vi)))
        else
            rethrow(err)
        end
    end
end

function MOI.optimize!(mock::MockOptimizer)
    mock.optimize_called = true
    mock.optimize!(mock)
    return
end

function MOI.compute_conflict!(model::MockOptimizer)
    model.compute_conflict_called = true
    return
end

function throw_mock_unsupported_names(attr)
    return throw(
        MOI.UnsupportedAttribute(
            attr,
            "The MockOptimizer was configured not to support names for " *
            "testing purpose using the `support_names=false` constructor keyword " *
            "argument.",
        ),
    )
end

function MOI.supports(
    ::MockOptimizer,
    ::Union{MOI.VariablePrimal,MockVariableAttribute},
    ::Type{MOI.VariableIndex},
)
    return true
end

function MOI.supports(
    mock::MockOptimizer,
    attr::MOI.AbstractVariableAttribute,
    IdxT::Type{MOI.VariableIndex},
)
    return MOI.supports(mock.inner_model, attr, IdxT)
end

function MOI.supports(
    ::MockOptimizer,
    ::Union{MOI.ConstraintDual,MockConstraintAttribute},
    ::Type{<:MOI.ConstraintIndex},
)
    return true
end

function MOI.supports(
    mock::MockOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    IdxT::Type{<:MOI.ConstraintIndex},
)
    return MOI.supports(mock.inner_model, attr, IdxT)
end

MOI.supports(mock::MockOptimizer, ::MockModelAttribute) = true

function MOI.set(
    mock::MockOptimizer,
    ::MOI.TerminationStatus,
    value::MOI.TerminationStatusCode,
)
    mock.termination_status = value
    return
end

function MOI.set(mock::MockOptimizer, attr::MOI.ObjectiveValue, value::Real)
    mock.objective_value[attr.result_index] = value
    return
end

function MOI.set(mock::MockOptimizer, attr::MOI.DualObjectiveValue, value::Real)
    mock.dual_objective_value[attr.result_index] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.PrimalStatus,
    value::MOI.ResultStatusCode,
)
    mock.primal_status[attr.result_index] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.DualStatus,
    value::MOI.ResultStatusCode,
)
    mock.dual_status[attr.result_index] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MOI.ConflictStatus,
    value::MOI.ConflictStatusCode,
)
    mock.conflict_status = value
    return
end

function MOI.get(mock::MockOptimizer, ::MOI.ConflictStatus)
    if mock.compute_conflict_called
        return mock.conflict_status
    end
    return MOI.COMPUTE_CONFLICT_NOT_CALLED
end

function MOI.set(mock::MockOptimizer, ::MockModelAttribute, value::Integer)
    mock.mock_model_attribute = value
    return
end

function MOI.supports(mock::MockOptimizer, attr::MOI.AbstractOptimizerAttribute)
    # `supports` is not defined if `is_set_by_optimize(attr)` so we pass it to
    # `mock.inner_model`.
    return MOI.supports(mock.inner_model, attr)
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.AbstractOptimizerAttribute,
    value,
)
    if MOI.is_set_by_optimize(attr)
        mock.optimizer_attributes[attr] = value
    else
        MOI.set(mock.inner_model, attr, xor_indices(value))
    end
    return
end

function MOI.supports(mock::MockOptimizer, attr::MOI.AbstractModelAttribute)
    # `supports` is not defined if `is_set_by_optimize(attr)` so we pass it to
    # `mock.inner_model`.
    return MOI.supports(mock.inner_model, attr)
end

function MOI.set(mock::MockOptimizer, attr::MOI.AbstractModelAttribute, value)
    if MOI.is_set_by_optimize(attr)
        mock.model_attributes[attr] = value
    else
        MOI.set(mock.inner_model, attr, xor_indices(value))
    end
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.AbstractVariableAttribute,
    idx::MOI.VariableIndex,
    value,
)
    MOI.set(mock.inner_model, attr, xor_index(idx), xor_indices(value))
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.VariablePrimal,
    idx::MOI.VariableIndex,
    value,
)
    _safe_set_result(mock.variable_primal, attr, idx, value)
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MOI.CallbackVariablePrimal,
    idx::MOI.VariableIndex,
    value,
)
    mock.callback_variable_primal[xor_index(idx)] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MockVariableAttribute,
    idx::MOI.VariableIndex,
    value,
)
    mock.mock_variable_attribute[xor_index(idx)] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    idx::MOI.ConstraintIndex,
    value,
)
    MOI.set(mock.inner_model, attr, xor_index(idx), value)
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MockConstraintAttribute,
    idx::MOI.ConstraintIndex,
    value,
)
    mock.mock_constraint_attribute[xor_index(idx)] = value
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.ConstraintDual,
    idx::MOI.ConstraintIndex,
    value,
)
    _safe_set_result(mock.constraint_dual, attr, idx, value)
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.ConstraintBasisStatus,
    idx::MOI.ConstraintIndex,
    value,
)
    _safe_set_result(mock.constraint_basis_status, attr, idx, value)
    return
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.VariableBasisStatus,
    idx::MOI.VariableIndex,
    value,
)
    _safe_set_result(mock.variable_basis_status, attr, idx, value)
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MOI.ConstraintConflictStatus,
    idx::MOI.ConstraintIndex,
    value,
)
    mock.constraint_conflict_status[xor_index(idx)] = value
    return
end

MOI.get(mock::MockOptimizer, ::MOI.RawSolver) = mock

function MOI.get(mock::MockOptimizer, attr::MOI.AbstractOptimizerAttribute)
    if MOI.is_set_by_optimize(attr)
        return mock.optimizer_attributes[attr]
    else
        return xor_indices(MOI.get(mock.inner_model, attr))
    end
end
function MOI.get(mock::MockOptimizer, attr::MOI.AbstractModelAttribute)
    if MOI.is_set_by_optimize(attr)
        return mock.model_attributes[attr]
    else
        return xor_indices(MOI.get(mock.inner_model, attr))
    end
end

#####
##### Names
#####

function MOI.supports(mock::MockOptimizer, attr::MOI.Name)
    return mock.supports_names && MOI.supports(mock.inner_model, attr)
end

function MOI.set(mock::MockOptimizer, attr::MOI.Name, value)
    if !mock.supports_names
        throw_mock_unsupported_names(attr)
    end
    MOI.set(mock.inner_model, attr, value)
    return
end

function MOI.supports(
    mock::MockOptimizer,
    attr::MOI.VariableName,
    IdxT::Type{MOI.VariableIndex},
)
    return mock.supports_names && MOI.supports(mock.inner_model, attr, IdxT)
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.VariableName,
    index::MOI.VariableIndex,
    value,
)
    if !mock.supports_names
        throw_mock_unsupported_names(attr)
    end
    MOI.set(mock.inner_model, attr, xor_index(index), value)
    return
end

function MOI.supports(
    mock::MockOptimizer,
    attr::MOI.ConstraintName,
    IdxT::Type{<:MOI.ConstraintIndex},
)
    return mock.supports_names && MOI.supports(mock.inner_model, attr, IdxT)
end

function MOI.set(
    mock::MockOptimizer,
    attr::MOI.ConstraintName,
    index::MOI.ConstraintIndex,
    value,
)
    if !mock.supports_names
        throw_mock_unsupported_names(attr)
    end
    MOI.set(mock.inner_model, attr, xor_index(index), value)
    return
end

function MOI.get(b::MockOptimizer, IdxT::Type{<:MOI.Index}, name::String)
    index = MOI.get(b.inner_model, IdxT, name)
    return index === nothing ? nothing : xor_index(index)
end

#####
##### Results
#####

MOI.get(mock::MockOptimizer, ::MOI.ResultCount) = mock.result_count

function MOI.set(mock::MockOptimizer, ::MOI.ResultCount, x)
    mock.result_count = x
    return
end

MOI.get(mock::MockOptimizer, ::MOI.TerminationStatus) = mock.termination_status

function MOI.get(mock::MockOptimizer, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(mock, attr)
    if mock.eval_objective_value
        return get_fallback(mock, attr)
    end
    return get(mock.objective_value, attr.result_index, NaN)
end

function MOI.get(mock::MockOptimizer, attr::MOI.DualObjectiveValue)
    MOI.check_result_index_bounds(mock, attr)
    if mock.eval_dual_objective_value
        return get_fallback(mock, attr, Float64)
    end
    return get(mock.dual_objective_value, attr.result_index, NaN)
end

function MOI.get(mock::MockOptimizer, attr::MOI.PrimalStatus)
    if attr.result_index > mock.result_count
        return MOI.NO_SOLUTION
    end
    return get(mock.primal_status, attr.result_index, MOI.NO_SOLUTION)
end

function MOI.get(mock::MockOptimizer, attr::MOI.DualStatus)
    if attr.result_index > mock.result_count
        return MOI.NO_SOLUTION
    end
    return get(mock.dual_status, attr.result_index, MOI.NO_SOLUTION)
end

MOI.get(mock::MockOptimizer, ::MockModelAttribute) = mock.mock_model_attribute

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.AbstractVariableAttribute,
    idx::MOI.VariableIndex,
)
    return xor_indices(MOI.get(mock.inner_model, attr, xor_index(idx)))
end

function MOI.get(
    mock::MockOptimizer,
    ::MockVariableAttribute,
    idx::MOI.VariableIndex,
)
    return mock.mock_variable_attribute[xor_index(idx)]
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.VariablePrimal,
    idx::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    return _safe_get_result(mock.variable_primal, attr, idx, "primal")
end

function MOI.get(
    mock::MockOptimizer,
    ::MOI.CallbackVariablePrimal,
    idx::MOI.VariableIndex,
)
    if !MOI.is_valid(mock, idx)
        throw(MOI.InvalidIndex(idx))
    end
    primal = get(mock.callback_variable_primal, xor_index(idx), nothing)
    if primal === nothing
        error("No mock callback primal is set for variable `", idx, "`.")
    end
    return primal
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.ConstraintPrimal,
    idx::MOI.ConstraintIndex,
)
    MOI.check_result_index_bounds(mock, attr)
    return get_fallback(mock, attr, idx)
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    idx::MOI.ConstraintIndex,
)
    # If it is thrown by `mock.inner_model`, the index will be xor'ed.
    MOI.throw_if_not_valid(mock, idx)
    return MOI.get(mock.inner_model, attr, xor_index(idx))
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.CanonicalConstraintFunction,
    idx::MOI.ConstraintIndex,
)
    # If it is thrown by `mock.inner_model`, the index will be xor'ed.
    MOI.throw_if_not_valid(mock, idx)
    # After xoring the indices, the order might not be respected anymore
    return canonical(
        xor_indices(MOI.get(mock.inner_model, attr, xor_index(idx))),
    )
end

function MOI.get(
    mock::MockOptimizer,
    attr::Union{MOI.CanonicalConstraintFunction,MOI.ConstraintFunction},
    idx::MOI.ConstraintIndex,
)
    # If it is thrown by `mock.inner_model`, the index will be xor'ed.
    MOI.throw_if_not_valid(mock, idx)
    return xor_indices(MOI.get(mock.inner_model, attr, xor_index(idx)))
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.ConstraintDual,
    idx::MOI.ConstraintIndex{F},
) where {F}
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    if mock.eval_variable_constraint_dual &&
       (F == MOI.SingleVariable || F == MOI.VectorOfVariables)
        return get_fallback(mock, attr, idx)
    else
        return _safe_get_result(mock.constraint_dual, attr, idx, "dual")
    end
end
function MOI.get(
    mock::MockOptimizer,
    ::MockConstraintAttribute,
    idx::MOI.ConstraintIndex,
)
    return mock.mock_constraint_attribute[xor_index(idx)]
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.ConstraintBasisStatus,
    idx::MOI.ConstraintIndex,
)
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    return _safe_get_result(
        mock.constraint_basis_status,
        attr,
        idx,
        "basis status",
    )
end

function MOI.get(
    mock::MockOptimizer,
    attr::MOI.VariableBasisStatus,
    idx::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    return _safe_get_result(
        mock.variable_basis_status,
        attr,
        idx,
        "basis status",
    )
end

function MOI.get(
    mock::MockOptimizer,
    ::MOI.ConstraintConflictStatus,
    idx::MOI.ConstraintIndex,
)
    MOI.throw_if_not_valid(mock, idx)
    return mock.constraint_conflict_status[xor_index(idx)]
end

function _safe_set_result(
    dict::Dict{K,V},
    attr::MOI.AnyAttribute,
    index::K,
    value,
) where {K,V}
    xored = xor_index(index)
    if !haskey(dict, xored)
        dict[xored] = V()
    end
    return dict[xored][attr.result_index] = value
end

function _safe_get_result(
    dict::Dict,
    attr::MOI.AnyAttribute,
    index::MOI.Index,
    name::String,
)
    index_name = index isa MOI.VariableIndex ? "variable" : "constraint"
    result_to_value = get(dict, xor_index(index), nothing)
    if result_to_value === nothing
        error("No mock $name is set for ", index_name, " `", index, "`.")
    end
    value = get(result_to_value, attr.result_index, nothing)
    if value === nothing
        error(
            "No mock $name is set for $(index_name) `$(index)` at result " *
            "index `$(attr.result_index)`.",
        )
    end
    return value
end

MOI.get(::MockOptimizer, ::MOI.SolverName) = "Mock"

function MOI.empty!(mock::MockOptimizer)
    MOI.empty!(mock.inner_model)
    mock.mock_model_attribute = 0
    empty!(mock.mock_variable_attribute)
    empty!(mock.mock_constraint_attribute)
    mock.optimize_called = false
    mock.termination_status = MOI.OPTIMIZE_NOT_CALLED
    mock.compute_conflict_called = false
    mock.conflict_status = MOI.COMPUTE_CONFLICT_NOT_CALLED
    empty!(mock.constraint_conflict_status)
    empty!(mock.objective_value)
    empty!(mock.dual_objective_value)
    empty!(mock.primal_status)
    empty!(mock.dual_status)
    empty!(mock.variable_primal)
    empty!(mock.callback_variable_primal)
    empty!(mock.constraint_dual)
    empty!(mock.constraint_basis_status)
    empty!(mock.variable_basis_status)
    empty!(mock.optimizer_attributes)
    empty!(mock.model_attributes)
    empty!(mock.submitted)
    return
end

function MOI.is_empty(mock::MockOptimizer)
    # Assumes that variable and constraint attributes can't be set if
    # mock.inner_model is empty.
    # TODO: Default values are currently copied in three places, not good.
    return MOI.is_empty(mock.inner_model) &&
           mock.mock_model_attribute == 0 &&
           !mock.optimize_called &&
           mock.termination_status == MOI.OPTIMIZE_NOT_CALLED &&
           isempty(mock.objective_value) &&
           isempty(mock.dual_objective_value) &&
           isempty(mock.primal_status) &&
           isempty(mock.dual_status) &&
           isempty(mock.constraint_basis_status) &&
           isempty(mock.variable_basis_status) &&
           isempty(mock.optimizer_attributes) &&
           isempty(mock.model_attributes) &&
           isempty(mock.submitted)
end

function MOI.is_valid(mock::MockOptimizer, idx::MOI.Index)
    return MOI.is_valid(mock.inner_model, xor_index(idx))
end

function MOI.delete(mock::MockOptimizer, index::MOI.VariableIndex)
    if !mock.delete_allowed
        throw(MOI.DeleteNotAllowed(index))
    end
    if !MOI.is_valid(mock, index)
        # The index thrown by `mock.inner_model` would be xored
        throw(MOI.InvalidIndex(index))
    end
    MOI.delete(mock.inner_model, xor_index(index))
    delete!(mock.variable_primal, index)
    delete!(mock.callback_variable_primal, index)
    delete!(mock.variable_basis_status, index)
    return
end

function MOI.delete(mock::MockOptimizer, indices::Vector{MOI.VariableIndex})
    if !mock.delete_allowed && !isempty(indices)
        throw(MOI.DeleteNotAllowed(first(indices)))
    end
    for index in indices
        # The index thrown by `mock.inner_model` would be xored
        MOI.throw_if_not_valid(mock, index)
    end
    MOI.delete(mock.inner_model, xor_index.(indices))
    for index in indices
        delete!(mock.variable_primal, index)
        delete!(mock.callback_variable_primal, index)
        delete!(mock.variable_basis_status, index)
    end
    return
end

function MOI.delete(mock::MockOptimizer, index::MOI.ConstraintIndex)
    if !mock.delete_allowed
        throw(MOI.DeleteNotAllowed(index))
    end
    if !MOI.is_valid(mock, index)
        # The index thrown by `mock.inner_model` would be xored
        throw(MOI.InvalidIndex(index))
    end
    MOI.delete(mock.inner_model, xor_index(index))
    delete!(mock.constraint_dual, index)
    delete!(mock.constraint_basis_status, index)
    return
end

function MOI.modify(
    mock::MockOptimizer,
    c::CI,
    change::MOI.AbstractFunctionModification,
)
    if !mock.modify_allowed
        throw(MOI.ModifyConstraintNotAllowed(c, change))
    end
    MOI.modify(mock.inner_model, xor_index(c), xor_indices(change))
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MOI.ConstraintSet,
    c::CI{<:MOI.AbstractFunction,S},
    set::S,
) where {S<:MOI.AbstractSet}
    MOI.set(mock.inner_model, MOI.ConstraintSet(), xor_index(c), set)
    return
end

function MOI.set(
    mock::MockOptimizer,
    ::MOI.ConstraintFunction,
    c::CI{F},
    func::F,
) where {F<:MOI.AbstractFunction}
    MOI.set(
        mock.inner_model,
        MOI.ConstraintFunction(),
        xor_index(c),
        xor_indices(func),
    )
    return
end

function MOI.modify(
    mock::MockOptimizer,
    obj::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if !mock.modify_allowed
        throw(MOI.ModifyObjectiveNotAllowed(change))
    end
    MOI.modify(mock.inner_model, obj, xor_indices(change))
    return
end

MOI.supports(::MockOptimizer, ::MOI.AbstractSubmittable) = true

function MOI.submit(mock::MockOptimizer, sub::MOI.AbstractSubmittable, args...)
    if !haskey(mock.submitted, sub)
        mock.submitted[sub] = Tuple[]
    end
    push!(mock.submitted[sub], args)
    return
end

# TODO: transform

function MOI.supports_constraint(
    mock::MockOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.supports_constraint(mock.inner_model, F, S)
end

function MOI.supports_add_constrained_variable(
    mock::MockOptimizer,
    S::Type{<:MOI.AbstractScalarSet},
)
    return MOI.supports_add_constrained_variable(mock.inner_model, S)
end

function MOI.supports_add_constrained_variables(
    mock::MockOptimizer,
    S::Type{<:MOI.AbstractVectorSet},
)
    return MOI.supports_add_constrained_variables(mock.inner_model, S)
end

# Add this method to avoid ambiguity
function MOI.supports_add_constrained_variables(
    mock::MockOptimizer,
    ::Type{MOI.Reals},
)
    return MOI.supports_add_constrained_variables(mock.inner_model, MOI.Reals)
end

function MOI.copy_to(dest::MockOptimizer, src::MOI.ModelLike; kwargs...)
    return default_copy_to(dest, src; kwargs...)
end

function MOI.supports_incremental_interface(mock::MockOptimizer)
    return MOI.supports_incremental_interface(mock.inner_model)
end

function final_touch(uf::MockOptimizer, index_map)
    return final_touch(uf.inner_model, index_map)
end

"""
    set_mock_optimize!(mock::MockOptimizer, opt::Function...)

Sets multiple optimize! function. The first is to be used the first time
`MOI.optimize!(mock)` is called, the second function is to be used the second
time, ...
"""
function set_mock_optimize!(mock::MockOptimizer, opts::Function...)
    mock.optimize! = _recursive_mock_optimize(opts...)
    return
end

_recursive_mock_optimize(optimize::Function) = optimize

function _recursive_mock_optimize(optimize::Function, tail::Function...)
    return (mock::MockOptimizer) -> begin
        optimize(mock)
        mock.optimize! = _recursive_mock_optimize(tail...)
        return
    end
end

"""
    mock_optimize!(
        mock::MockOptimizer,
        termination_status::MOI.TerminationStatusCode,
        primal::Union{
            Tuple{MOI.ResultStatusCode,<:Vector},
            MOI.ResultStatusCode,
            <:Vector,
        }
        dual_status::MOI.ResultStatusCode,
        constraint_duals::Pair{Tuple{DataTypeDataType},<:Vector}...;
        constraint_basis_status = Pair{Tuple{DataTypeDataType},<:Vector}[],
        variable_basis_status = MOI.BasisStatusCode[],
        constraint_conflict_status = Pair{Tuple{Type,Type},<:Vector}[],
    )

Fake the result of a call to `optimize!` in the mock optimizer by storing the
solution.

## Arguments

 * `termination_status`: defaults to `MOI.OPTIMAL` if not provided.

 * `primal`: pass one of the following: a tuple of `(status, result)`
   corresponding to the `MOI.PrimalStatus` and `MOI.VariablePrimal` attributes,
   or pass a `status` with no result vector, or pass only the result vector. If
   the status is omitted, it is assumed to be `MOI.FEASIBLE_POINT`. The `result`
   vector should correspond to the order of the primal values given by
   `MOI.ListOfVariableIndices`.

 * `dual_status`: corresponds to the `MOI.DualStatus` attribute. If not
   provided, it defaults to `MOI.FEASIBLE_POINT` if constriant duals are
   provided and `MOI.NO_SOLUTION` otherwise.

 * `constraint_duals`: the remaining positional arguments are passed as pairs.
   Each pair is of the form `(F, S) => result`, where `result` is the the vector
   of `MOI.ConstraintDual` values for the constraints `F`-in-`S` in the order
   returned by `MOI.ListOfConstraintIndices{F,S}`.

 * `constraint_basis_status`: a vector of pairs similar to `constraint_duals`,
   except this time for the `MOI.ConstraintBasisStatus` attribute.

 * `variable_basis_status`: a vector of `MOI.BasisStatusCode`, corresponding to
   the `MOI.VariableBasisStatus` attribute of the variables in the order
   returned by `MOI.ListOfVariableIndices`.

 * `constraint_conflict_status`: a vector of pairs similar to
   `constraint_duals`, except this time for the `MOI.ConstraintConflictStatus`
   attribute.
"""
function mock_optimize!(
    mock::MockOptimizer,
    termination_status::MOI.TerminationStatusCode,
    primal::Union{
        Tuple{MOI.ResultStatusCode,<:Vector},
        MOI.ResultStatusCode,
        <:Vector,
    },
    dual_status_constraint_duals...;
    constraint_basis_status = [],
    variable_basis_status = MOI.BasisStatusCode[],
    constraint_conflict_status = [],
    var_basis = nothing,
    con_basis = nothing,
)
    if var_basis !== nothing
        @warn(
            "var_basis is deprecated. Use variable_basis_status instead.",
            maxlog = 1,
        )
        variable_basis_status = var_basis
    end
    if con_basis !== nothing
        @warn(
            "con_basis is deprecated. Use constraint_basis_status instead.",
            maxlog = 1,
        )
        constraint_basis_status = con_basis
    end
    MOI.set(mock, MOI.TerminationStatus(), termination_status)
    MOI.set(mock, MOI.ResultCount(), 1)
    _set_mock_primal(mock, primal)
    _set_mock_dual(mock, dual_status_constraint_duals...)
    for con_basis_pair in constraint_basis_status
        F, S = con_basis_pair.first
        indices = MOI.get(mock, MOI.ListOfConstraintIndices{F,S}())
        for (i, ci) in enumerate(indices)
            MOI.set(
                mock,
                MOI.ConstraintBasisStatus(),
                ci,
                con_basis_pair.second[i],
            )
        end
    end
    for con_conflict_pair in constraint_conflict_status
        F, S = con_conflict_pair.first
        indices = MOI.get(mock, MOI.ListOfConstraintIndices{F,S}())
        for (i, ci) in enumerate(indices)
            MOI.set(
                mock,
                MOI.ConstraintConflictStatus(),
                ci,
                con_conflict_pair.second[i],
            )
        end
    end
    if length(variable_basis_status) > 0
        variables = MOI.get(mock, MOI.ListOfVariableIndices())
        @assert length(variable_basis_status) == length(variables)
        MOI.set.(
            mock,
            MOI.VariableBasisStatus(),
            variables,
            variable_basis_status,
        )
    end
    return
end

# The fallback for default termination_status
function mock_optimize!(
    mock::MockOptimizer,
    primal::Union{
        Tuple{MOI.ResultStatusCode,<:Vector},
        MOI.ResultStatusCode,
        <:Vector,
    },
    args...;
    kwargs...,
)
    mock_optimize!(mock, MOI.OPTIMAL, primal, args...; kwargs...)
    return
end

# The fallback if no primal solution is provided
function mock_optimize!(mock::MockOptimizer, status::MOI.TerminationStatusCode)
    MOI.set(mock, MOI.TerminationStatus(), status)
    MOI.set(mock, MOI.ResultCount(), 0)
    return
end

# _set_mock_primal

function _set_mock_primal(
    mock::MockOptimizer,
    primal::Tuple{MOI.ResultStatusCode,<:Vector},
)
    MOI.set(mock, MOI.PrimalStatus(), primal[1])
    x = MOI.get(mock, MOI.ListOfVariableIndices())
    MOI.set(mock, MOI.VariablePrimal(), x, primal[2])
    return
end

function _set_mock_primal(mock::MockOptimizer, primal::MOI.ResultStatusCode)
    MOI.set(mock, MOI.PrimalStatus(), primal)
    return
end

function _set_mock_primal(mock::MockOptimizer, primal::Vector)
    MOI.set(mock, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    x = MOI.get(mock, MOI.ListOfVariableIndices())
    MOI.set(mock, MOI.VariablePrimal(), x, primal)
    return
end

# _set_mock_dual

function _set_mock_dual(mock::MockOptimizer)
    MOI.set(mock, MOI.DualStatus(), MOI.NO_SOLUTION)
    return
end

function _set_mock_dual(
    mock::MockOptimizer,
    dual_status::MOI.ResultStatusCode,
    constraint_duals::Pair...,
)
    MOI.set(mock, MOI.DualStatus(), dual_status)
    for ((F, S), result) in constraint_duals
        indices = MOI.get(mock, MOI.ListOfConstraintIndices{F,S}())
        for (i, ci) in enumerate(indices)
            MOI.set(mock, MOI.ConstraintDual(), ci, result[i])
        end
    end
    return
end

# fallback for no status
function _set_mock_dual(mock::MockOptimizer, args::Pair...)
    _set_mock_dual(mock, MOI.FEASIBLE_POINT, args...)
    return
end
