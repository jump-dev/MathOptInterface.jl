# An Int-valued attribute
struct MockModelAttribute <: MOI.AbstractModelAttribute
end

# An Int-valued attribute
struct MockVariableAttribute <: MOI.AbstractVariableAttribute
end

# An Int-valued attribute
struct MockConstraintAttribute <: MOI.AbstractConstraintAttribute
end

# A mock optimizer used for testing.
mutable struct MockOptimizer{MT<:MOI.ModelLike} <: MOI.AbstractOptimizer
    inner_model::MT
    attribute::Int # MockModelAttribute
    varattribute::Dict{MOI.VariableIndex,Int} # MockVariableAttribute
    conattribute::Dict{MOI.ConstraintIndex,Int} # MockConstraintAttribute
    supports_names::Bool # Allows to test with optimizer not supporting names
    needs_allocate_load::Bool # Allows to tests the Allocate-Load interface, see copy_to
    add_var_allowed::Bool # If false, the optimizer throws AddVariableNotAllowed
    add_con_allowed::Bool # If false, the optimizer throws AddConstraintNotAllowed
    modify_allowed::Bool # If false, the optimizer throws Modify...NotAllowed
    delete_allowed::Bool # If false, the optimizer throws DeleteNotAllowed
    optimize!::Function
    solved::Bool
    hasprimal::Bool
    hasdual::Bool
    result_count::Int
    terminationstatus::MOI.TerminationStatusCode
    # Computes `ObjectiveValue` by evaluating the `ObjectiveFunction` with
    # `VariablePrimal`. See `get_fallback`.
    eval_objective_value::Bool
    objective_value::Dict{Int,Float64} # set this using MOI.set(model, MOI.ObjectiveValue(), value)
    # Computes `DualObjectiveValue` using `get_fallback`
    eval_dual_objective_value::Bool
    dual_objective_value::Dict{Int,Float64} # set this using MOI.set(model, MOI.DualObjectiveValue(), value)
    primal_status::Dict{Int,MOI.ResultStatusCode}
    dual_status::Dict{Int,MOI.ResultStatusCode}
    varprimal::Dict{MOI.VariableIndex,Dict{Int,Float64}}
    callback_variable_primal::Dict{MOI.VariableIndex, Float64}
    # Computes `ConstraintDual` of constraints with `SingleVariable` or
    # `VectorOfVariables` functions by evaluating the `ConstraintDual` of
    # constraints having the variable in the function. See `get_fallback`.
    eval_variable_constraint_dual::Bool
    condual::Dict{MOI.ConstraintIndex,Dict{Int,Any}}
    con_basis::Dict{MOI.ConstraintIndex,Dict{Int,MOI.BasisStatusCode}}
    # The attributes set by `MOI.optimize!` cannot be set to `model`.
    # We detect them with `is_set_by_optimize` and store them in the following:
    optimizer_attributes::Dict{MOI.AbstractOptimizerAttribute, Any}
    model_attributes::Dict{MOI.AbstractModelAttribute, Any}
    submitted::Dict{MOI.AbstractSubmittable, Vector{Tuple}}
end

# All user-facing indices are xor'd with this mask to produce unusual indices.
# This is good at catching bugs.
const internal_xor_mask = Int64(12345678)
xor_index(vi::VI) = VI(xor(vi.value, internal_xor_mask))
xor_index(ci::CI{F,S}) where {F,S} = CI{F,S}(xor(ci.value, internal_xor_mask))
xor_indices(x) = map_indices(xor_index, x)

function MockOptimizer(inner_model::MOI.ModelLike; supports_names=true,
                       needs_allocate_load=false,
                       add_var_allowed=!needs_allocate_load,
                       add_con_allowed=!needs_allocate_load,
                       eval_objective_value=true,
                       eval_dual_objective_value=true,
                       eval_variable_constraint_dual=true)
    return MockOptimizer(inner_model,
                         0,
                         Dict{MOI.VariableIndex,Int}(),
                         Dict{MOI.ConstraintIndex,Int}(),
                         supports_names,
                         needs_allocate_load,
                         add_var_allowed,
                         add_con_allowed,
                         true,
                         true,
                         (::MockOptimizer) -> begin end,
                         false,
                         false,
                         false,
                         1,
                         MOI.OPTIMIZE_NOT_CALLED,
                         eval_objective_value,
                         Dict{Int,Float64}(),
                         eval_dual_objective_value,
                         Dict{Int,Float64}(),
                         Dict{Int,MOI.ResultStatusCode}(),
                         Dict{Int,MOI.ResultStatusCode}(),
                         Dict{MOI.VariableIndex,Dict{Int,Float64}}(),
                         Dict{MOI.VariableIndex,Float64}(),
                         eval_variable_constraint_dual,
                         Dict{MOI.ConstraintIndex,Dict{Int,Any}}(),
                         Dict{MOI.ConstraintIndex,Dict{Int,MOI.BasisStatusCode}}(),
                         Dict{MOI.AbstractOptimizerAttribute, Any}(),
                         Dict{MOI.AbstractModelAttribute, Any}(),
                         Dict{MOI.AbstractSubmittable, Vector{Tuple}}())
end

function MOI.add_variable(mock::MockOptimizer)
    if mock.add_var_allowed
        return xor_index(MOI.add_variable(mock.inner_model))
    else
        throw(MOI.AddVariableNotAllowed())
    end
end

function MOI.add_constraint(mock::MockOptimizer,
                            func::MOI.AbstractFunction,
                            set::MOI.AbstractSet)
    if mock.add_con_allowed
        ci = MOI.add_constraint(mock.inner_model, xor_indices(func), set)
        return xor_index(ci)
    else
        throw(MOI.AddConstraintNotAllowed{typeof(func), typeof(set)}())
    end
end
function MOI.optimize!(mock::MockOptimizer)
    mock.solved = true
    mock.hasprimal = true
    mock.hasdual = true
    mock.optimize!(mock)
end

function throw_mock_unsupported_names(attr)
    throw(MOI.UnsupportedAttribute(
        attr, "The MockOptimizer was configured not to support names for " *
        "testing purpose using the `support_names=false` constructor keyword " *
        "argument."))
end

function MOI.supports(mock::MockOptimizer,
                      ::Union{MOI.VariablePrimal, MockVariableAttribute},
                      ::Type{MOI.VariableIndex})
    return true
end
function MOI.supports(mock::MockOptimizer,
                      attr::MOI.AbstractVariableAttribute,
                      IdxT::Type{MOI.VariableIndex})
    return MOI.supports(mock.inner_model, attr, IdxT)
end
function MOI.supports(mock::MockOptimizer,
                      ::Union{MOI.ConstraintDual, MockConstraintAttribute},
                      ::Type{<:MOI.ConstraintIndex})
    return true
end
function MOI.supports(mock::MockOptimizer,
                      attr::MOI.AbstractConstraintAttribute,
                      IdxT::Type{<:MOI.ConstraintIndex})
    return MOI.supports(mock.inner_model, attr, IdxT)
end

MOI.supports(mock::MockOptimizer, ::MockModelAttribute) = true
MOI.set(mock::MockOptimizer, ::MOI.TerminationStatus, value::MOI.TerminationStatusCode) = (mock.terminationstatus = value)
function MOI.set(mock::MockOptimizer, attr::MOI.ObjectiveValue, value::Real)
    mock.objective_value[attr.result_index] = value
end
function MOI.set(mock::MockOptimizer, attr::MOI.DualObjectiveValue, value::Real)
    mock.dual_objective_value[attr.result_index] = value
end
function MOI.set(mock::MockOptimizer, attr::MOI.PrimalStatus, value::MOI.ResultStatusCode)
    mock.primal_status[attr.result_index] = value
end
function MOI.set(mock::MockOptimizer, attr::MOI.DualStatus, value::MOI.ResultStatusCode)
    mock.dual_status[attr.result_index] = value
end
MOI.set(mock::MockOptimizer, ::MockModelAttribute, value::Integer) = (mock.attribute = value)
function MOI.supports(mock::MockOptimizer, attr::MOI.AbstractOptimizerAttribute)
    # `supports` is not defined if `is_set_by_optimize(attr)` so we pass it to
    # `mock.inner_model`.
    return MOI.supports(mock.inner_model, attr)
end
function MOI.set(mock::MockOptimizer, attr::MOI.AbstractOptimizerAttribute,
                 value)
    if MOI.is_set_by_optimize(attr)
        mock.optimizer_attributes[attr] = value
    else
        MOI.set(mock.inner_model, attr, xor_indices(value))
    end
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
end

function MOI.set(mock::MockOptimizer, attr::MOI.AbstractVariableAttribute,
                 idx::MOI.VariableIndex, value)
    MOI.set(mock.inner_model, attr, xor_index(idx), xor_indices(value))
end
function MOI.set(mock::MockOptimizer, attr::MOI.VariablePrimal,
                 idx::MOI.VariableIndex, value)
    _safe_set_result(mock.varprimal, attr, idx, value)
end
function MOI.set(mock::MockOptimizer, ::MOI.CallbackVariablePrimal,
                 idx::MOI.VariableIndex, value)
    mock.callback_variable_primal[xor_index(idx)] = value
end
function MOI.set(mock::MockOptimizer, ::MockVariableAttribute,
                 idx::MOI.VariableIndex, value)
    mock.varattribute[xor_index(idx)] = value
end
function MOI.set(mock::MockOptimizer, attr::MOI.AbstractConstraintAttribute,
                 idx::MOI.ConstraintIndex, value)
    MOI.set(mock.inner_model, attr, xor_index(idx), value)
end
function MOI.set(mock::MockOptimizer, ::MockConstraintAttribute,
                 idx::MOI.ConstraintIndex, value)
    mock.conattribute[xor_index(idx)] = value
end
function MOI.set(mock::MockOptimizer, attr::MOI.ConstraintDual,
                 idx::MOI.ConstraintIndex, value)
    _safe_set_result(mock.condual, attr, idx, value)
end
function MOI.set(mock::MockOptimizer, attr::MOI.ConstraintBasisStatus,
                 idx::MOI.ConstraintIndex, value)
    _safe_set_result(mock.con_basis, attr, idx, value)
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
    if mock.supports_names
        MOI.set(mock.inner_model, attr, value)
    else
        throw_mock_unsupported_names(attr)
    end
end
function MOI.supports(mock::MockOptimizer, attr::MOI.VariableName,
                      IdxT::Type{MOI.VariableIndex})
    return mock.supports_names && MOI.supports(mock.inner_model, attr, IdxT)
end
function MOI.set(mock::MockOptimizer,
                 attr::MOI.VariableName,
                 index::MOI.VariableIndex, value)
    if mock.supports_names
        MOI.set(mock.inner_model, attr, xor_index(index), value)
    else
        throw_mock_unsupported_names(attr)
    end
end
function MOI.supports(mock::MockOptimizer, attr::MOI.ConstraintName,
                      IdxT::Type{<:MOI.ConstraintIndex})
    return mock.supports_names && MOI.supports(mock.inner_model, attr, IdxT)
end
function MOI.set(mock::MockOptimizer,
                 attr::MOI.ConstraintName,
                 index::MOI.ConstraintIndex, value)
    if mock.supports_names
        MOI.set(mock.inner_model, attr, xor_index(index), value)
    else
        throw_mock_unsupported_names(attr)
    end
end
function MOI.get(b::MockOptimizer, IdxT::Type{<:MOI.Index}, name::String)
    index = MOI.get(b.inner_model, IdxT, name)
    if index === nothing
        return nothing
    else
        return xor_index(index)
    end
end

#####
##### Results
#####

MOI.get(mock::MockOptimizer, ::MOI.ResultCount) = mock.result_count
MOI.set(mock::MockOptimizer, ::MOI.ResultCount, x) = (mock.result_count = x)

MOI.get(mock::MockOptimizer, ::MOI.TerminationStatus) = mock.terminationstatus
function MOI.get(mock::MockOptimizer, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(mock, attr)
    if mock.eval_objective_value
        return get_fallback(mock, attr)
    else
        return get(mock.objective_value, attr.result_index, NaN)
    end
end
function MOI.get(mock::MockOptimizer, attr::MOI.DualObjectiveValue)
    MOI.check_result_index_bounds(mock, attr)
    if mock.eval_dual_objective_value
        return get_fallback(mock, attr, Float64)
    else
        return get(mock.dual_objective_value, attr.result_index, NaN)
    end
end
function MOI.get(mock::MockOptimizer, attr::MOI.PrimalStatus)
    if attr.result_index > mock.result_count
        return MOI.NO_SOLUTION
    else
        return get(mock.primal_status, attr.result_index, MOI.NO_SOLUTION)
    end
end
function MOI.get(mock::MockOptimizer, attr::MOI.DualStatus)
    if attr.result_index > mock.result_count
        return MOI.NO_SOLUTION
    else
        return get(mock.dual_status, attr.result_index, MOI.NO_SOLUTION)
    end
end
MOI.get(mock::MockOptimizer, ::MockModelAttribute) = mock.attribute

function MOI.get(mock::MockOptimizer, attr::MOI.AbstractVariableAttribute,
                 idx::MOI.VariableIndex)
    return xor_indices(MOI.get(mock.inner_model, attr, xor_index(idx)))
end
MOI.get(mock::MockOptimizer, ::MockVariableAttribute, idx::MOI.VariableIndex) = mock.varattribute[xor_index(idx)]

function MOI.get(
    mock::MockOptimizer, attr::MOI.VariablePrimal, idx::MOI.VariableIndex
)
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    return _safe_get_result(mock.varprimal, attr, idx, "primal")
end

function MOI.get(
    mock::MockOptimizer, attr::MOI.CallbackVariablePrimal, idx::MOI.VariableIndex
)
    primal = get(mock.callback_variable_primal, xor_index(idx), nothing)
    if primal !== nothing
        return primal
    elseif MOI.is_valid(mock, idx)
        error("No mock callback primal is set for variable `", idx, "`.")
    else
        throw(MOI.InvalidIndex(idx))
    end
end

function MOI.get(
    mock::MockOptimizer, attr::MOI.ConstraintPrimal, idx::MOI.ConstraintIndex
)
    MOI.check_result_index_bounds(mock, attr)
    return get_fallback(mock, attr, idx)
end

function MOI.get(mock::MockOptimizer, attr::MOI.AbstractConstraintAttribute,
                 idx::MOI.ConstraintIndex)
    # If it is thrown by `mock.inner_model`, the index will be xor'ed.
    MOI.throw_if_not_valid(mock, idx)
    return MOI.get(mock.inner_model, attr, xor_index(idx))
end
function MOI.get(mock::MockOptimizer, attr::MOI.ConstraintFunction,
                 idx::MOI.ConstraintIndex)
    # If it is thrown by `mock.inner_model`, the index will be xor'ed.
    MOI.throw_if_not_valid(mock, idx)
    return xor_indices(MOI.get(mock.inner_model, attr, xor_index(idx)))
end

function MOI.get(
    mock::MockOptimizer, attr::MOI.ConstraintDual, idx::MOI.ConstraintIndex{F}
) where {F}
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    if mock.eval_variable_constraint_dual &&
        (F == MOI.SingleVariable || F == MOI.VectorOfVariables)
        return get_fallback(mock, attr, idx)
    else
        return _safe_get_result(mock.condual, attr, idx, "dual")
    end
end
MOI.get(mock::MockOptimizer, ::MockConstraintAttribute, idx::MOI.ConstraintIndex) = mock.conattribute[xor_index(idx)]
function MOI.get(mock::MockOptimizer, attr::MOI.ConstraintBasisStatus, idx::MOI.ConstraintIndex)
    MOI.check_result_index_bounds(mock, attr)
    MOI.throw_if_not_valid(mock, idx)
    return _safe_get_result(mock.con_basis, attr, idx, "basis status")
end

function _safe_set_result(dict::Dict{K,V}, attr::MOI.AnyAttribute, index::K,
                          value) where {K, V}
    xored = xor_index(index)
    if !haskey(dict, xored)
        dict[xored] = V()
    end
    dict[xored][attr.result_index] = value
end
function _safe_get_result(dict::Dict, attr::MOI.AnyAttribute, index::MOI.Index,
                          name::String)
    index_name = index isa MOI.VariableIndex ? "variable" : "constraint"
    result_to_value = get(dict, xor_index(index), nothing)
    if result_to_value === nothing
        error("No mock $name is set for ", index_name, " `", index, "`.")
    end
    value = get(result_to_value, attr.result_index, nothing)
    if value === nothing
        error("No mock $name is set for ", index_name, " `", index, "` at result index `", attr.result_index, "`.")
    end
    return value
end

MOI.get(::MockOptimizer, ::MOI.SolverName) = "Mock"

function MOI.empty!(mock::MockOptimizer)
    MOI.empty!(mock.inner_model)
    mock.attribute = 0
    empty!(mock.varattribute)
    empty!(mock.conattribute)
    mock.solved = false
    mock.hasprimal = false
    mock.hasdual = false
    mock.terminationstatus = MOI.OPTIMIZE_NOT_CALLED
    empty!(mock.objective_value)
    empty!(mock.dual_objective_value)
    empty!(mock.primal_status)
    empty!(mock.dual_status)
    empty!(mock.varprimal)
    empty!(mock.callback_variable_primal)
    empty!(mock.condual)
    empty!(mock.con_basis)
    empty!(mock.optimizer_attributes)
    empty!(mock.model_attributes)
    empty!(mock.submitted)
    return
end

function MOI.is_empty(mock::MockOptimizer)
    # Assumes that variable and constraint attributes can't be set if
    # mock.inner_model is empty.
    # TODO: Default values are currently copied in three places, not good.
    return MOI.is_empty(mock.inner_model) && mock.attribute == 0 &&
        !mock.solved && !mock.hasprimal && !mock.hasdual &&
        mock.terminationstatus == MOI.OPTIMIZE_NOT_CALLED &&
        isempty(mock.objective_value) &&
        isempty(mock.dual_objective_value) &&
        isempty(mock.primal_status) &&
        isempty(mock.dual_status) &&
        isempty(mock.con_basis) && isempty(mock.optimizer_attributes) &&
        isempty(mock.model_attributes) && isempty(mock.submitted)
end

MOI.is_valid(mock::MockOptimizer, idx::MOI.Index) = MOI.is_valid(mock.inner_model, xor_index(idx))

function MOI.delete(mock::MockOptimizer, index::MOI.VariableIndex)
    if !mock.delete_allowed
        throw(MOI.DeleteNotAllowed(index))
    end
    if !MOI.is_valid(mock, index)
        # The index thrown by `mock.inner_model` would be xored
        throw(MOI.InvalidIndex(index))
    end
    MOI.delete(mock.inner_model, xor_index(index))
    delete!(mock.varprimal, index)
    delete!(mock.callback_variable_primal, index)
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
        delete!(mock.varprimal, index)
        delete!(mock.callback_variable_primal, index)
    end
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
    delete!(mock.condual, index)
    delete!(mock.con_basis, index)
end

function MOI.modify(mock::MockOptimizer, c::CI, change::MOI.AbstractFunctionModification)
    if !mock.modify_allowed
        throw(MOI.ModifyConstraintNotAllowed(c, change))
    end
    MOI.modify(mock.inner_model, xor_index(c), xor_indices(change))
end

function MOI.set(mock::MockOptimizer, ::MOI.ConstraintSet,
                 c::CI{<:MOI.AbstractFunction, S},
                 set::S) where S<:MOI.AbstractSet
    MOI.set(mock.inner_model, MOI.ConstraintSet(), xor_index(c), set)
end
function MOI.set(mock::MockOptimizer, ::MOI.ConstraintFunction, c::CI{F},
                 func::F) where F<:MOI.AbstractFunction
    MOI.set(mock.inner_model, MOI.ConstraintFunction(), xor_index(c),
            xor_indices(func))
end

function MOI.modify(mock::MockOptimizer, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification)
    if !mock.modify_allowed
        throw(MOI.ModifyObjectiveNotAllowed(change))
    end
    MOI.modify(mock.inner_model, obj, xor_indices(change))
end

MOI.supports(::MockOptimizer, ::MOI.AbstractSubmittable) = true
function MOI.submit(mock::MockOptimizer, sub::MOI.AbstractSubmittable, args...)
    if !haskey(mock.submitted, sub)
        mock.submitted[sub] = Tuple[]
    end
    push!(mock.submitted[sub], args)
end

# TODO: transform

MOI.supports_constraint(mock::MockOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.supports_constraint(mock.inner_model, F, S)
MOI.supports_add_constrained_variable(mock::MockOptimizer, S::Type{<:MOI.AbstractScalarSet}) = MOI.supports_add_constrained_variable(mock.inner_model, S)
MOI.supports_add_constrained_variables(mock::MockOptimizer, S::Type{<:MOI.AbstractVectorSet}) = MOI.supports_add_constrained_variables(mock.inner_model, S)
# Add this method to avoid ambiguity
MOI.supports_add_constrained_variables(mock::MockOptimizer, ::Type{MOI.Reals}) = MOI.supports_add_constrained_variables(mock.inner_model, MOI.Reals)

function MOI.copy_to(mock::MockOptimizer, src::MOI.ModelLike; kws...)
    automatic_copy_to(mock, src; kws...)
end
function supports_default_copy_to(mock::MockOptimizer, copy_names::Bool)
    return !mock.needs_allocate_load && supports_default_copy_to(mock.inner_model, copy_names)
end

# Allocate-Load Interface
function supports_allocate_load(mock::MockOptimizer, copy_names::Bool)
    return supports_allocate_load(mock.inner_model, copy_names)
end

function allocate_variables(mock::MockOptimizer, nvars)
    return xor_index.(allocate_variables(mock.inner_model, nvars))
end
allocate(mock::MockOptimizer, attr::MOI.AnyAttribute, value) = allocate(mock.inner_model, attr, xor_indices(value))
allocate(mock::MockOptimizer, attr::MOI.AnyAttribute, idx::MOI.Index, value) = allocate(mock.inner_model, attr, xor_index(idx), xor_indices(value))
allocate_constraint(mock::MockOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet) = xor_index(allocate_constraint(mock.inner_model, xor_indices(f), s))

load_variables(mock::MockOptimizer, nvars) = load_variables(mock.inner_model, nvars)
load(mock::MockOptimizer, attr::MOI.AnyAttribute, value) = load(mock.inner_model, attr, xor_indices(value))
load(mock::MockOptimizer, attr::MOI.AnyAttribute, idx::MOI.Index, value) = load(mock.inner_model, attr, xor_index(idx), xor_indices(value))
load_constraint(mock::MockOptimizer, ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = load_constraint(mock.inner_model, xor_index(ci), xor_indices(f), s)

"""
    set_mock_optimize!(mock::MockOptimizer, opt::Function...)

Sets multiple optimize! function. The first is to be used the first time `MOI.optimize!(mock)` is called, the second function is to be used the second time, ...
"""
function set_mock_optimize!(mock::MockOptimizer, opts::Function...)
    mock.optimize! = rec_mock_optimize(mock, opts...)
end
function rec_mock_optimize(mock::MockOptimizer, opt::Function, opts::Function...)
    (mock::MockOptimizer) -> (opt(mock); mock.optimize! = rec_mock_optimize(mock, opts...))
end
rec_mock_optimize(mock::MockOptimizer, opt::Function) = opt

"""
    mock_optimize!(mock::MockOptimizer, termstatus::MOI.TerminationStatusCode, (primstatus::MOI.ResultStatusCode, varprim::Vector), dual_status::MOI.ResultStatusCode, conduals::Pair...)

Sets the termination status of `mock` to `termstatus` and the primal (resp. dual) status to `primstatus` (resp. `dual_status`).
The primal values of the variables in the order returned by `ListOfVariableIndices` are set to `varprim`.
If `termstatus` is missing, it is assumed to be `MOI.OPTIMAL`.
If `primstatus` is missing, it is assumed to be `MOI.FEASIBLE_POINT`.
If `dual_status` is missing, it is assumed to be `MOI.FEASIBLE_POINT` if there is a primal solution and `primstatus` is not `MOI.INFEASIBLE_POINT`, otherwise it is `MOI.INFEASIBILITY_CERTIFICATE`.
The dual values are set to the values specified by `conduals`. Each pair is of the form `(F,S)=>[...]` where `[...]` is the the vector of dual values for the constraints `F`-in-`S` in the order returned by `ListOfConstraintIndices{F,S}`.
The bases status are set to the status specified by `con_basis`. A vector of pairs, each of the form `(F,S)=>[...]`, where `[...]` is the the vector of basis status for the constraints `F`-in-`S` in the order returned by `ListOfConstraintIndices{F,S}`.
"""
function mock_optimize!(mock::MockOptimizer, termstatus::MOI.TerminationStatusCode, primal, dual...; con_basis = [])
    MOI.set(mock, MOI.TerminationStatus(), termstatus)
    MOI.set(mock, MOI.ResultCount(), 1)
    mock_primal!(mock, primal)
    mock_dual!(mock, dual...)
    for con_basis_pair in con_basis
        mock_basis_status!(mock, con_basis_pair)
    end
end
# Default termination status
mock_optimize!(mock::MockOptimizer, primdual...; kws...) = mock_optimize!(mock, MOI.OPTIMAL, primdual...; kws...)
function mock_optimize!(mock::MockOptimizer, termstatus::MOI.TerminationStatusCode)
    MOI.set(mock, MOI.TerminationStatus(), termstatus)
    MOI.set(mock, MOI.ResultCount(), 0)
end

# Primal
mock_primal!(mock, primal::Tuple) = mock_primal!(mock, primal...)
function mock_primal!(mock::MockOptimizer, primstatus::MOI.ResultStatusCode, varprim::Vector...)
    MOI.set(mock, MOI.PrimalStatus(), primstatus)
    mock_varprimal!(mock, varprim...)
end
# Default primal status
mock_primal!(mock::MockOptimizer, varprim::Vector) = mock_primal!(mock, MOI.FEASIBLE_POINT, varprim)
function mock_primal!(mock::MockOptimizer)
    # No primal solution
    mock.hasprimal = false
end

# Sets variable primal to varprim
function mock_varprimal!(mock::MockOptimizer) end
function mock_varprimal!(mock::MockOptimizer, varprim::Vector)
    MOI.set(mock, MOI.VariablePrimal(), MOI.get(mock, MOI.ListOfVariableIndices()), varprim)
end

# Dual
function mock_dual!(mock::MockOptimizer, dual_status::MOI.ResultStatusCode, conduals::Pair...)
    MOI.set(mock, MOI.DualStatus(), dual_status)
    mock_condual!(mock, conduals...)
end
# Default dual status
function mock_dual!(mock::MockOptimizer, conduals::Pair...)
    status = !mock.hasprimal || MOI.get(mock, MOI.PrimalStatus()) == MOI.INFEASIBLE_POINT ? MOI.INFEASIBILITY_CERTIFICATE : MOI.FEASIBLE_POINT
    mock_dual!(mock, status, conduals...)
end
function mock_dual!(mock::MockOptimizer)
    # No dual solution
    mock.hasdual = false
end

# Sets constraint dual to conduals
function mock_condual!(mock::MockOptimizer) end
function mock_condual!(mock::MockOptimizer, condual::Pair, conduals...)
    F, S = condual.first
    duals = condual.second
    for (i, ci) in enumerate(MOI.get(mock, MOI.ListOfConstraintIndices{F, S}()))
        MOI.set(mock, MOI.ConstraintDual(), ci, duals[i])
    end
    mock_condual!(mock, conduals...)
end
# Set the basis status of the provided constraints.
function mock_basis_status!(mock::MockOptimizer, con_basis::Pair)
    F, S = con_basis.first
    bases = con_basis.second
    for (i, ci) in enumerate(MOI.get(mock, MOI.ListOfConstraintIndices{F, S}()))
        MOI.set(mock, MOI.ConstraintBasisStatus(), ci, bases[i])
    end
end
