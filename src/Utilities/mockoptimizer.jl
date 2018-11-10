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
    needs_allocate_load::Bool # Allows to tests the Allocate-Load interface, see copy_to
    add_var_allowed::Bool
    add_con_allowed::Bool # If false, the optimizer throws AddConstraintNotAllowed
    modify_allowed::Bool # If false, the optimizer throws Modify...NotAllowed
    delete_allowed::Bool # If false, the optimizer throws DeleteNotAllowed
    optimize!::Function
    solved::Bool
    hasprimal::Bool
    hasdual::Bool
    terminationstatus::MOI.TerminationStatusCode
    resultcount::Int
    # Computes `ObjectiveValue` by evaluating the `ObjectiveFunction` with
    # `VariablePrimal`. See `get_fallback`.
    eval_objective_value::Bool
    objectivevalue::Float64
    objectivebound::Float64  # set this using MOI.set(model, MOI.ObjectiveBound(), value)
    primalstatus::MOI.ResultStatusCode
    dualstatus::MOI.ResultStatusCode
    varprimal::Dict{MOI.VariableIndex,Float64}
    # Computes `ConstraintDual` of constraints with `SingleVariable` or
    # `VectorOfVariables` functions by evaluating the `ConstraintDual` of
    # constraints having the variable in the function. See `get_fallback`.
    eval_variable_constraint_dual::Bool
    condual::Dict{MOI.ConstraintIndex,Any}
end

# All user-facing indices are xor'd with this mask to produce unusual indices.
# This is good at catching bugs.
const internal_xor_mask = Int64(12345678)
xor_index(vi::VI) = VI(xor(vi.value, internal_xor_mask))
xor_index(ci::CI{F,S}) where {F,S} = CI{F,S}(xor(ci.value, internal_xor_mask))
xor_variables(f) = mapvariables(xor_index, f)

function MockOptimizer(inner_model::MOI.ModelLike; needs_allocate_load=false,
                       eval_objective_value=true,
                       eval_variable_constraint_dual=true)
    return MockOptimizer(inner_model,
                         0,
                         Dict{MOI.VariableIndex,Int}(),
                         Dict{MOI.ConstraintIndex,Int}(),
                         needs_allocate_load,
                         true,
                         true,
                         true,
                         true,
                         (::MockOptimizer) -> begin end,
                         false,
                         false,
                         false,
                         MOI.Success,
                         0,
                         eval_objective_value,
                         NaN,
                         NaN,
                         MOI.UnknownResultStatus,
                         MOI.UnknownResultStatus,
                         Dict{MOI.VariableIndex,Float64}(),
                         eval_variable_constraint_dual,
                         Dict{MOI.ConstraintIndex,Any}())
end

function MOI.add_variable(mock::MockOptimizer)
    if mock.add_var_allowed
        return xor_index(MOI.add_variable(mock.inner_model))
    else
        throw(MOI.AddVariableNotAllowed())
    end
end
function MOI.add_variables(mock::MockOptimizer, n::Int)
    if mock.add_var_allowed
        return xor_index.(MOI.add_variables(mock.inner_model, n))
    else
        throw(MOI.AddVariableNotAllowed())
    end
end
function MOI.add_constraint(mock::MockOptimizer,
                            func::MOI.AbstractFunction,
                            set::MOI.AbstractSet)
    if mock.add_con_allowed
        ci = MOI.add_constraint(mock.inner_model, xor_variables(func), set)
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

MOI.supports(mock::MockOptimizer, ::Union{MOI.VariablePrimal,MockVariableAttribute}, ::Type{MOI.VariableIndex}) = true
MOI.supports(mock::MockOptimizer, attr::MOI.AbstractVariableAttribute, IdxT::Type{MOI.VariableIndex}) = MOI.supports(mock.inner_model, attr, IdxT)
MOI.supports(mock::MockOptimizer, ::Union{MOI.ConstraintDual,MockConstraintAttribute}, ::Type{<:MOI.ConstraintIndex}) = true
MOI.supports(mock::MockOptimizer, attr::MOI.AbstractConstraintAttribute, IdxT::Type{<:MOI.ConstraintIndex}) = MOI.supports(mock.inner_model, attr, IdxT)

MOI.supports(mock::MockOptimizer, ::Union{MOI.ResultCount,MOI.TerminationStatus,MOI.ObjectiveValue,MOI.PrimalStatus,MOI.DualStatus,MockModelAttribute}) = true
MOI.set(mock::MockOptimizer, ::MOI.ResultCount, value::Integer) = (mock.resultcount = value)
MOI.set(mock::MockOptimizer, ::MOI.TerminationStatus, value::MOI.TerminationStatusCode) = (mock.terminationstatus = value)
MOI.set(mock::MockOptimizer, ::MOI.ObjectiveValue, value::Real) = (mock.objectivevalue = value)
MOI.set(mock::MockOptimizer, ::MOI.PrimalStatus, value::MOI.ResultStatusCode) = (mock.primalstatus = value)
MOI.set(mock::MockOptimizer, ::MOI.DualStatus, value::MOI.ResultStatusCode) = (mock.dualstatus = value)
MOI.set(mock::MockOptimizer, ::MockModelAttribute, value::Integer) = (mock.attribute = value)
MOI.supports(mock::MockOptimizer, attr::MOI.AbstractModelAttribute) = MOI.supports(mock.inner_model, attr)
MOI.set(mock::MockOptimizer, attr::MOI.AbstractModelAttribute, value) = MOI.set(mock.inner_model, attr, value)
MOI.set(mock::MockOptimizer, attr::MOI.ObjectiveFunction, value) = MOI.set(mock.inner_model, attr, xor_variables(value))

MOI.set(mock::MockOptimizer, attr::MOI.AbstractVariableAttribute, idx::MOI.VariableIndex, value) = MOI.set(mock.inner_model, attr, xor_index(idx), value)
MOI.set(mock::MockOptimizer, ::MOI.VariablePrimal, idx::MOI.VariableIndex, value) = (mock.varprimal[xor_index(idx)] = value)
MOI.set(mock::MockOptimizer, ::MockVariableAttribute, idx::MOI.VariableIndex, value) = (mock.varattribute[xor_index(idx)] = value)
MOI.set(mock::MockOptimizer, attr::MOI.AbstractConstraintAttribute, idx::MOI.ConstraintIndex, value) = MOI.set(mock.inner_model, attr, xor_index(idx), value)
MOI.set(mock::MockOptimizer, ::MockConstraintAttribute, idx::MOI.ConstraintIndex, value) = (mock.conattribute[xor_index(idx)] = value)
MOI.set(mock::MockOptimizer, ::MOI.ConstraintDual, idx::MOI.ConstraintIndex, value) = (mock.condual[xor_index(idx)] = value)

MOI.get(mock::MockOptimizer, attr::MOI.AbstractModelAttribute) = MOI.get(mock.inner_model, attr)
MOI.get(mock::MockOptimizer, attr::Union{MOI.ListOfVariableIndices,
                                         MOI.ListOfConstraintIndices}) = xor_index.(MOI.get(mock.inner_model, attr))
MOI.get(mock::MockOptimizer, attr::MOI.ObjectiveFunction) = xor_variables(MOI.get(mock.inner_model, attr))

MOI.get(mock::MockOptimizer, attr::Union{MOI.ConstraintSet}, idx::MOI.ConstraintIndex) = MOI.get(mock.inner_model, attr, xor_index(idx))
MOI.get(mock::MockOptimizer, attr::Union{MOI.ConstraintFunction}, idx::MOI.ConstraintIndex) = xor_variables(MOI.get(mock.inner_model, attr, xor_index(idx)))

# Name
function MOI.get(b::MockOptimizer, IdxT::Type{<:MOI.Index}, name::String)
    index = MOI.get(b.inner_model, IdxT, name)
    if index === nothing
        return nothing
    else
        return xor_index(index)
    end
end

MOI.get(mock::MockOptimizer, ::MOI.ResultCount) = mock.resultcount
MOI.get(mock::MockOptimizer, ::MOI.TerminationStatus) = mock.terminationstatus
function MOI.get(mock::MockOptimizer, attr::MOI.ObjectiveValue)
    if mock.eval_objective_value
        return get_fallback(mock, attr)
    else
        return mock.objectivevalue
    end
end
MOI.get(mock::MockOptimizer, ::MOI.PrimalStatus) = mock.primalstatus
MOI.get(mock::MockOptimizer, ::MOI.DualStatus) = mock.dualstatus
MOI.get(mock::MockOptimizer, ::MockModelAttribute) = mock.attribute

MOI.get(mock::MockOptimizer, attr::MOI.AbstractVariableAttribute, idx::MOI.VariableIndex) = MOI.get(mock.inner_model, attr, xor_index(idx))
MOI.get(mock::MockOptimizer, ::MockVariableAttribute, idx::MOI.VariableIndex) = mock.varattribute[xor_index(idx)]
MOI.get(mock::MockOptimizer, ::MOI.VariablePrimal, idx::MOI.VariableIndex) = mock.varprimal[xor_index(idx)]
function MOI.get(mock::MockOptimizer, attr::MOI.ConstraintPrimal,
                 idx::MOI.ConstraintIndex)
    return get_fallback(mock, attr, idx)
end
MOI.get(mock::MockOptimizer, attr::MOI.AbstractConstraintAttribute, idx::MOI.ConstraintIndex) = MOI.get(mock.inner_model, attr, xor_index(idx))
function MOI.get(mock::MockOptimizer, attr::MOI.ConstraintDual,
                 idx::MOI.ConstraintIndex{F}) where F
    if mock.eval_variable_constraint_dual &&
        (F == MOI.SingleVariable || F == MOI.VectorOfVariables)
        return get_fallback(mock, attr, idx)
    else
        return mock.condual[xor_index(idx)]
    end
end
MOI.get(mock::MockOptimizer, ::MockConstraintAttribute, idx::MOI.ConstraintIndex) = mock.conattribute[xor_index(idx)]

MOI.supports(mock::MockOptimizer, ::MOI.ObjectiveBound) = true
MOI.get(mock::MockOptimizer, ::MOI.ObjectiveBound) = mock.objectivebound
function MOI.set(mock::MockOptimizer, ::MOI.ObjectiveBound, value::Float64)
    mock.objectivebound = value
end

MOI.get(mock::MockOptimizer, ::MOI.SolverName) = "Mock"

function MOI.empty!(mock::MockOptimizer)
    MOI.empty!(mock.inner_model)
    mock.attribute = 0
    mock.varattribute = Dict{MOI.VariableIndex,Int}()
    mock.conattribute = Dict{MOI.ConstraintIndex,Int}()
    mock.solved = false
    mock.hasprimal = false
    mock.hasdual = false
    mock.terminationstatus = MOI.Success
    mock.resultcount = 0
    mock.objectivevalue = NaN
    mock.objectivebound = NaN
    mock.primalstatus = MOI.UnknownResultStatus
    mock.dualstatus = MOI.UnknownResultStatus
    mock.varprimal = Dict{MOI.VariableIndex,Float64}()
    mock.condual = Dict{MOI.ConstraintIndex,Any}()
    return
end

function MOI.is_empty(mock::MockOptimizer)
    # Assumes that variable and constraint attributes can't be set if
    # mock.inner_model is empty.
    # TODO: Default values are currently copied in three places, not good.
    return MOI.is_empty(mock.inner_model) && mock.attribute == 0 &&
        !mock.solved && !mock.hasprimal && !mock.hasdual &&
        mock.terminationstatus == MOI.Success &&
        mock.resultcount == 0 && isnan(mock.objectivevalue) &&
        isnan(mock.objectivebound) &&
        mock.primalstatus == MOI.UnknownResultStatus &&
        mock.dualstatus == MOI.UnknownResultStatus
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
end

function MOI.modify(mock::MockOptimizer, c::CI, change::MOI.AbstractFunctionModification)
    if !mock.modify_allowed
        throw(MOI.ModifyConstraintNotAllowed(c, change))
    end
    MOI.modify(mock.inner_model, xor_index(c), xor_variables(change))
end

function MOI.set(mock::MockOptimizer, ::MOI.ConstraintSet, c::CI{F,S}, set::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.set(mock.inner_model, MOI.ConstraintSet(), xor_index(c), set)
end

function MOI.set(mock::MockOptimizer, ::MOI.ConstraintFunction, c::CI{F,S}, func::F) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.set(mock.inner_model, MOI.ConstraintFunction(), xor_index(c), xor_variables(func))
end

function MOI.modify(mock::MockOptimizer, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification)
    if !mock.modify_allowed
        throw(MOI.ModifyObjectiveNotAllowed(change))
    end
    MOI.modify(mock.inner_model, obj, xor_variables(change))
end

# TODO: transform

MOI.supports_constraint(mock::MockOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.supports_constraint(mock.inner_model, F, S)
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

allocate_variables(mock::MockOptimizer, nvars) = allocate_variables(mock.inner_model, nvars)
allocate(mock::MockOptimizer, attr::MOI.AnyAttribute, value) = allocate(mock.inner_model, attr, value)
allocate(mock::MockOptimizer, attr::MOI.ObjectiveFunction, value) = allocate(mock.inner_model, attr, xor_variables(value))
allocate(mock::MockOptimizer, attr::MOI.AnyAttribute, idx::MOI.Index, value) = allocate(mock.inner_model, attr, xor_index(idx), value)
allocate_constraint(mock::MockOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet) = xor_index(allocate_constraint(mock.inner_model, xor_variables(f), s))

load_variables(mock::MockOptimizer, nvars) = load_variables(mock.inner_model, nvars)
load(mock::MockOptimizer, attr::MOI.AnyAttribute, value) = load(mock.inner_model, attr, value)
load(mock::MockOptimizer, attr::MOI.ObjectiveFunction, value) = load(mock.inner_model, attr, xor_variables(value))
load(mock::MockOptimizer, attr::MOI.AnyAttribute, idx::MOI.Index, value) = load(mock.inner_model, attr, xor_index(idx), value)
load_constraint(mock::MockOptimizer, ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = load_constraint(mock.inner_model, xor_index(ci), xor_variables(f), s)

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
    mock_optimize!(mock::MockOptimizer, termstatus::MOI.TerminationStatusCode, (primstatus::MOI.ResultStatusCode, varprim::Vector), dualstatus::MOI.ResultStatusCode, conduals::Pair...)

Sets the termination status of `mock` to `termstatus` and the primal (resp. dual) status to `primstatus` (resp. `dualstatus`).
The primal values of the variables in the order returned by `ListOfVariableIndices` are set to `varprim`.
If `termstatus` is missing, it is assumed to be `MOI.Success`.
If `primstatus` is missing, it is assumed to be `MOI.FeasiblePoint`.
If `dualstatus` is missing, it is assumed to be `MOI.FeasiblePoint` if there is a primal solution and `primstatus` is not `MOI.InfeasiblePoint`, otherwise it is `MOI.InfeasibilityCertificate`.
The dual values are set to the values specified by `conduals`. Each pair is of the form `(F,S)=>[...]` where `[...]` is the the vector of dual values for the constraints `F`-in-`S` in the order returned by `ListOfConstraintIndices{F,S}`.
"""
function mock_optimize!(mock::MockOptimizer, termstatus::MOI.TerminationStatusCode, primal, dual...)
    MOI.set(mock, MOI.TerminationStatus(), termstatus)
    MOI.set(mock, MOI.ResultCount(), 1)
    mock_primal!(mock, primal)
    mock_dual!(mock, dual...)
end
# Default termination status
mock_optimize!(mock::MockOptimizer, primdual...) = mock_optimize!(mock, MOI.Success, primdual...)
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
mock_primal!(mock::MockOptimizer, varprim::Vector) = mock_primal!(mock, MOI.FeasiblePoint, varprim)
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
function mock_dual!(mock::MockOptimizer, dualstatus::MOI.ResultStatusCode, conduals::Pair...)
    MOI.set(mock, MOI.DualStatus(), dualstatus)
    mock_condual!(mock, conduals...)
end
# Default dual status
function mock_dual!(mock::MockOptimizer, conduals::Pair...)
    status = !mock.hasprimal || MOI.get(mock, MOI.PrimalStatus()) == MOI.InfeasiblePoint ? MOI.InfeasibilityCertificate : MOI.FeasiblePoint
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
