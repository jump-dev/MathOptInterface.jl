"""
    AbstractBridgeOptimizer

A bridge optimizer applies given constraint bridges to a given optimizer thus
extending the types of supported constraints. The attributes of the inner
optimizer are automatically transformed to make the bridges transparent, e.g.
the variables and constraints created by the bridges are hidden.

By convention, the inner optimizer should be stored in a `model` field and
the dictionary mapping constraint indices to bridges should be stored in a
`bridges` field. If a bridge optimizer deviates from these conventions, it
should implement the functions `MOI.optimize!` and `bridge` respectively.
"""
abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end

# AbstractBridgeOptimizer interface

function supports_constraint_bridges end

"""
    is_bridged(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
               S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge `F`-in-`S` constraints
instead of passing it as is to its internal model.

    is_bridged(b::AbstractBridgeOptimizer, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge constrained variables in
`S` instead of passing it as is to its internal model.

    is_bridged(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction})::Bool

Return a `Bool` indicating whether `b` tries to bridge objective functions of
type `F` instead of passing it as is to its internal model.
"""
function is_bridged end

"""
    is_bridged(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)

Return a `Bool` indicating whether `vi` is bridged. The variable is said to be
bridged if it is a variable of `b` but not a variable of `b.model`.
"""
is_bridged(::AbstractBridgeOptimizer, vi::MOI.VariableIndex) = vi.value < 0

"""
    is_bridged(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)

Return a `Bool` indicating whether `ci` is bridged. The constraint is said to be
bridged if it is a constraint of `b` but not a constraint of `b.model`.
"""
function is_bridged(b::AbstractBridgeOptimizer,
                    ci::MOI.ConstraintIndex{F, S}) where {F, S}
    return is_bridged(b, F, S)
end
function is_bridged(b::AbstractBridgeOptimizer,
                    ci::MOI.ConstraintIndex{F, S}) where {
        F<:Union{MOI.SingleVariable, MOI.VectorOfVariables}, S}
    # `ci.value < 0` if it is variable-bridged or force-bridged
    return is_bridged(b, F, S) || ci.value < 0
end

"""
    is_bridged(b::AbstractBridgeOptimizer,
               attr::MOI.ObjectiveFunction)

Return a `Bool` indicating whether `attr` is bridged. The objective function is
said to be bridged the objective function attribute passed to `b.model` is
different to `attr`.
"""
function is_bridged(
    b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)
    return haskey(Objective.bridges(b), attr)
end

const ObjectiveAttribute = Union{
    MOI.ObjectiveSense, MOI.ObjectiveFunction, MOI.ObjectiveFunctionType
}

"""
    supports_bridging_constrained_variable(
        ::AbstractBridgeOptimizer, ::Type{<:MOI.AbstractSet})

Return a `Bool` indicating whether `b` supports bridging constrained variable in
`S`.
"""
function supports_bridging_constrained_variable(
    ::AbstractBridgeOptimizer, ::Type{<:MOI.AbstractSet})
    return false
end

"""
    supports_bridging_constraint(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` supports bridging `F`-in-`S` constraints.
"""
function supports_bridging_constraint(
    ::AbstractBridgeOptimizer, ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet})
    return false
end

"""
    supports_bridging_objective_function(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractScalarFunction})::Bool

Return a `Bool` indicating whether `b` supports bridging objective functions of
type `F`.
"""
function supports_bridging_objective_function(
    ::AbstractBridgeOptimizer, ::Type{<:MOI.AbstractScalarFunction})
    return false
end

"""
    bridge_type(b::AbstractBridgeOptimizer,
                F::Type{<:MOI.AbstractFunction},
                S::Type{<:MOI.AbstractSet})

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `is_bridged(b, F, S)`.
"""
function bridge_type end

"""
    is_variable_bridged(b::AbstractBridgeOptimizer,
                        ci::MOI.ConstraintIndex)

Returns whether `ci` is the constraint of a bridged constrained variable. That
is, if it was returned by `Variable.add_key_for_bridge` or
`Variable.add_keys_for_bridge`. Note that it is not equivalent to
`ci.value < 0` as, it can also simply be a constraint on a bridged variable.
"""
is_variable_bridged(::AbstractBridgeOptimizer, ::MOI.ConstraintIndex) = false
function is_variable_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{<:Union{MOI.SingleVariable, MOI.VectorOfVariables}})
    # It can be a constraint corresponding to bridged constrained variables so
    # we `check` with `haskey(Constraint.bridges(b), ci)` whether this is the
    # case.
    return ci.value < 0 && !haskey(Constraint.bridges(b), ci)
end

"""
    bridge(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)

Return the `Variable.AbstractBridge` used to bridge the variable with index
`vi`.
"""
function bridge(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    return Variable.bridges(b)[vi]
end

"""
    bridge(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)

Return the `AbstractBridge` used to bridge the constraint with index `ci`.
"""
function bridge(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)
    if is_variable_bridged(b, ci)
        return bridge(b, MOI.VariableIndex(ci.value))
    else
        return Constraint.bridges(b)[ci]
    end
end

"""
    bridge(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)

Return the `Objective.AbstractBridge` used to bridge the objective function
`attr`.
"""
function bridge(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)
    return Objective.bridges(b)[attr]
end
function _functionize_bridge(b::AbstractBridgeOptimizer, bridge_type)
    func, name = _func_name(bridge_type)
    error("Need to apply a `$bridge_type` to a `$func` $name because the",
          " variable is bridged but $name bridges are not supported by",
          " `$(typeof(b))`.")
end
function constraint_scalar_functionize_bridge(b::AbstractBridgeOptimizer)
    _functionize_bridge(b, Constraint.ScalarFunctionizeBridge)
end
function constraint_vector_functionize_bridge(b::AbstractBridgeOptimizer)
    _functionize_bridge(b, Constraint.VectorFunctionizeBridge)
end
function objective_functionize_bridge(b::AbstractBridgeOptimizer)
    _functionize_bridge(b, Objective.FunctionizeBridge)
end

# Implementation of the MOI interface for AbstractBridgeOptimizer

MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.model)
# By convention, the model should be stored in a `model` field

function MOI.is_empty(b::AbstractBridgeOptimizer)
    return isempty(Variable.bridges(b)) && isempty(Constraint.bridges(b)) &&
           MOI.is_empty(b.model)
end
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    if Variable.has_bridges(Variable.bridges(b))
        empty!(b.var_to_name)
        b.name_to_var = nothing
    end
    if Variable.has_bridges(Variable.bridges(b)) ||
        Constraint.has_bridges(Constraint.bridges(b))
        empty!(b.con_to_name)
        b.name_to_con = nothing
    end
    empty!(Variable.bridges(b))
    empty!(Constraint.bridges(b))
    empty!(Objective.bridges(b))
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::Union{MOI.AbstractModelAttribute,
                                  MOI.AbstractOptimizerAttribute})
    return MOI.supports(b.model, attr)
end

function MOI.copy_to(mock::AbstractBridgeOptimizer, src::MOI.ModelLike; kws...)
    MOIU.automatic_copy_to(mock, src; kws...)
end
function MOIU.supports_default_copy_to(b::AbstractBridgeOptimizer,
                                       copy_names::Bool)
    return MOIU.supports_default_copy_to(b.model, copy_names)
end

# References
function MOI.is_valid(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    if is_bridged(b, vi)
        return haskey(Variable.bridges(b), vi)
    else
        return MOI.is_valid(b.model, vi)
    end
end
function MOI.is_valid(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    if is_bridged(b, ci)
        if is_variable_bridged(b, ci)
            vi = MOI.VariableIndex(ci.value)
            return MOI.is_valid(b, vi) &&
                Variable.constrained_set(Variable.bridges(b), vi) == S
        else
            return haskey(Constraint.bridges(b), ci)
        end
    else
        return MOI.is_valid(b.model, ci)
    end
end
function MOI.delete(b::AbstractBridgeOptimizer, vis::Vector{MOI.VariableIndex})
    if Constraint.has_bridges(Constraint.bridges(b))
        # Delete all `MOI.VectorOfVariables` constraints of these variables.
        for ci in Constraint.variable_constraints(Constraint.bridges(b), vis)
            if vis == MOI.get(b, MOI.ConstraintFunction(), ci).variables
                MOI.delete(b, ci)
            end
        end
        # Delete all `MOI.SingleVariable` constraints of these variables.
        for vi in vis
            for ci in Constraint.variable_constraints(Constraint.bridges(b), vi)
                MOI.delete(b, ci)
            end
        end
    end
    if any(vi -> is_bridged(b, vi), vis)
        for vi in vis
            MOI.throw_if_not_valid(b, vi)
        end
        if all(vi -> is_bridged(b, vi), vis) && Variable.has_keys(Variable.bridges(b), vis)
            MOI.delete(b, bridge(b, first(vis)))
            b.name_to_var = nothing
            for vi in vis
                delete!(b.var_to_name, vi)
            end
            ci = Variable.constraint(Variable.bridges(b), first(vis))
            b.name_to_con = nothing
            delete!(b.con_to_name, ci)
            delete!(Variable.bridges(b), vis)
        else
            for vi in vis
                MOI.delete(b, vi)
            end
        end
    else
        MOI.delete(b.model, vis)
    end
end
function MOI.delete(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    if Constraint.has_bridges(Constraint.bridges(b))
        # Delete all `MOI.SingleVariable` constraints of this variable
        for ci in Constraint.variable_constraints(Constraint.bridges(b), vi)
            MOI.delete(b, ci)
        end
    end
    if is_bridged(b, vi)
        MOI.throw_if_not_valid(b, vi)
        if Variable.length_of_vector_of_variables(Variable.bridges(b), vi) > 1
            if MOI.supports_dimension_update(Variable.constrained_set(Variable.bridges(b), vi))
                MOI.delete(b, bridge(b, vi), _index(b, vi)...)
            else
                MOIU.throw_delete_variable_in_vov(vi)
            end
        else
            MOI.delete(b, bridge(b, vi))
            ci = Variable.constraint(Variable.bridges(b), vi)
            b.name_to_con = nothing
            delete!(b.con_to_name, ci)
        end
        delete!(Variable.bridges(b), vi)
        b.name_to_var = nothing
        delete!(b.var_to_name, vi)
    else
        MOI.delete(b.model, vi)
    end
end
function MOI.delete(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        br = bridge(b, ci)
        if is_variable_bridged(b, ci)
            error("Cannot delete constraint index of bridged constrained",
                  " variables. Delete the scalar variable or the vector of",
                  " variables instead.")
        else
            delete!(Constraint.bridges(b), ci)
        end
        MOI.delete(b, br)
        b.name_to_con = nothing
        delete!(b.con_to_name, ci)
    else
        MOI.delete(b.model, ci)
    end
end

# Attributes

"""
    function reduce_bridged(
        b::AbstractBridgeOptimizer, args,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
        init, operate_variable_bridges!,
        operate_constraint_bridges!)

If `F`-in-`S` constraints may be added to `b.model`,
starts with `value = MOI.get(b.model, args...)`, otherwise, starts with
`value = init()`. Then
* if `F`-in-`S` constraints may correspond to
    bridged variables, modify it with `operate_variable_bridges!`;
* if `F`-in-`S` constraints may correspond to
    bridged constraints, modify it with `operate_constraint_bridges!`;
then return the final `value`.

For instance, [`MOI.supports`](@ref) calls this function with
`init = () -> true`, `operate_variable_bridges(ok) =
ok && MOI.supports(b, attr, Variable.concrete_bridge_type(b, S))` and
`operate_constraint_bridges(ok) = ok &&
MOI.supports(b, attr, Constraint.concrete_bridge_type(b, F, S))`.
"""
function reduce_bridged(
    b::AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
    init, model_value,
    operate_variable_bridges!,
    operate_constraint_bridges!)
    if is_bridged(b, F, S)
        value = init()
    else
        value = model_value()
    end
    variable_function = F == MOIU.variable_function_type(S)
    if variable_function && is_bridged(b, S)
        value = operate_variable_bridges!(value)
    end
    # Even it it is not bridged, it may have been force-bridged because one of the
    # variable in the function was bridged.
    if is_bridged(b, F, S) || (variable_function && supports_constraint_bridges(b))
        value = operate_constraint_bridges!(value)
    end
    return value
end

# List of indices of all constraints, including those bridged
function get_all_including_bridged(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfVariableIndices)
    list = MOI.get(b.model, attr)
    if !isempty(Variable.bridges(b))
        list = append!(copy(list), keys(Variable.bridges(b)))
    end
    return list
end
function get_all_including_bridged(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    return reduce_bridged(
         b, F, S, () -> MOI.ConstraintIndex{F, S}[],
         () -> MOI.get(b.model, attr),
         list -> append!(list, Variable.constraints_with_set(
            Variable.bridges(b), S)),
         list -> append!(list, Constraint.keys_of_type(
            Constraint.bridges(b), MOI.ConstraintIndex{F, S}))
    )
end
# Remove constraints bridged by `bridge` from `list`
function _remove_bridged(list, bridge, attr)
    for c in MOI.get(bridge, attr)
        i = something(findfirst(isequal(c), list), 0)
        if !iszero(i)
            MOI.deleteat!(list, i)
        end
    end
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::Union{MOI.ListOfConstraintIndices,
                             MOI.ListOfVariableIndices})
    list = get_all_including_bridged(b, attr)
    for bridge in values(Variable.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    return list
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfVariables)
    s = MOI.get(b.model, attr) + Variable.number_of_variables(Variable.bridges(b))
    for bridge in values(Variable.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    return s
end

# Number of all constraints, including those bridged
function get_all_including_bridged(
    b::AbstractBridgeOptimizer,
    attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    return reduce_bridged(
         b, F, S, () -> 0, () -> MOI.get(b.model, attr),
         num -> num + Variable.number_with_set(Variable.bridges(b), S),
         num -> num + Constraint.number_of_type(Constraint.bridges(b),
                                                MOI.ConstraintIndex{F, S})
    )
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    s = get_all_including_bridged(b, attr)
    # The constraints counted in `s` may have been added by bridges
    for bridge in values(Variable.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    return s
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    if Constraint.has_bridges(Constraint.bridges(b)) && Variable.has_bridges(Variable.bridges(b))
        set_of_types = Constraint.list_of_key_types(Constraint.bridges(b))
        union!(set_of_types, Variable.list_of_constraint_types(Variable.bridges(b)))
        # There may be types already in `list_of_types` of a supported constraint
        # that was force-bridged because a variable in the `SingleVariable` or
        # `VectorOfVariables` function was bridged even though the constraint type
        # is supported by `b.model`. As `set_of_types` is a set, these duplicates
        # are merge automatically.
        union!(set_of_types, MOI.get(b.model, attr))
        list_of_types = collect(set_of_types)
    elseif Constraint.has_bridges(Constraint.bridges(b))
        # There should be no duplicate so no need to do `Set` union.
        list_of_types = [
            MOI.get(b.model, attr);
            collect(Constraint.list_of_key_types(Constraint.bridges(b)))
        ]
    elseif Variable.has_bridges(Variable.bridges(b))
        set_of_types = Variable.list_of_constraint_types(Variable.bridges(b))
        union!(set_of_types, MOI.get(b.model, attr))
        list_of_types = collect(set_of_types)
    else
        list_of_types = copy(MOI.get(b.model, attr))
    end
    # Some constraint types show up in `list_of_types` including when all the
    # constraints of that type have been created by bridges and not by the user.
    # The code in `NumberOfConstraints` takes care of removing these constraints
    # from the counter so we can rely on it to remove these constraint types.
    types_to_remove = findall(iszero.(
        map(FS -> MOI.get(b, MOI.NumberOfConstraints{FS...}()), list_of_types)))
    deleteat!(list_of_types, types_to_remove)
    return list_of_types
end

# Model an optimizer attributes
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ListOfModelAttributesSet)
    list = MOI.get(b.model, attr)
    if is_objective_bridged(b)
        list = copy(list)
        # There should be a `MOI.ObjectiveFunction` in `list` otherwise
        # `is_objective_bridged` would return `false`.
        deleteat!(list, findfirst(attr -> attr isa MOI.ObjectiveFunction, list))
        push!(list, MOI.ObjectiveFunction{MOI.get(b, MOI.ObjectiveFunctionType())}())
    end
    return unbridged_function(b, list)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::Union{MOI.AbstractModelAttribute,
                             MOI.AbstractOptimizerAttribute})
    return unbridged_function(b, MOI.get(b.model, attr))
end
function MOI.set(b::AbstractBridgeOptimizer,
                 attr::Union{MOI.AbstractModelAttribute,
                             MOI.AbstractOptimizerAttribute},
                 value)
    return MOI.set(b.model, attr, bridged_function(b, value))
end

# Objective

"""
    is_objective_bridged(b::AbstractBridgeOptimizer)

Return a `Bool` indicating whether the objective is bridged. The objective is
said to be bridged if the value of `MOI.ObjectiveFunctionType` is different for
`b` and `b.model`.
"""
is_objective_bridged(b) = !isempty(Objective.bridges(b))
function _delete_objective_bridges(b)
    MOI.delete(b, Objective.root_bridge(Objective.bridges(b)))
    empty!(Objective.bridges(b))
end

function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::MOI.ObjectiveFunction{F}) where F
    if is_bridged(b, F)
        return supports_bridging_objective_function(b, F)
    else
        return MOI.supports(b.model, attr)
    end
end

"""
    struct ObjectiveFunctionValue{F<:MOI.AbstractScalarFunction} end

Attribute for the value of the objective function of type `F`. If the objective
of the objective function does not depend on `F`, the type `F` determines
whether the computation is redirected to an objective bridge or to the
underlying model.
"""
struct ObjectiveFunctionValue{F<:MOI.AbstractScalarFunction} end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::ObjectiveFunctionValue{F}) where F
    obj_attr = MOI.ObjectiveFunction{F}()
    if is_bridged(b, obj_attr)
        return MOI.get(b, attr, bridge(b, obj_attr))
    else
        return MOI.get(b.model, MOI.ObjectiveValue())
    end
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ObjectiveValue)
    if is_objective_bridged(b)
        F =  Objective.function_type(Objective.bridges(b))
        return MOI.get(b, ObjectiveFunctionValue{F}())
    else
        return MOI.get(b.model, attr)
    end
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ObjectiveFunctionType)
    if is_objective_bridged(b)
        return Objective.function_type(Objective.bridges(b))
    else
        return MOI.get(b.model, attr)
    end
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ObjectiveSense)
    return MOI.get(b.model, attr)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ObjectiveFunction)
    value = if is_bridged(b, attr)
        MOI.get(b, attr, bridge(b, attr))
    else
        MOI.get(b.model, attr)
    end
    return unbridged_function(b, value)
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveSense,
                 value::MOI.OptimizationSense)
    MOI.set(b.model, attr, value)
    if is_objective_bridged(b)
        if value == MOI.FEASIBILITY_SENSE
            _delete_objective_bridges(b)
        else
            for bridge in values(Objective.bridges(b))
                MOI.set(b, attr, bridge, value)
            end
        end
    end
end
function _bridge_objective(b, BridgeType, func)
    bridge = Objective.bridge_objective(BridgeType, b, func)
    Objective.add_key_for_bridge(Objective.bridges(b), bridge, func)
end
function MOI.set(b::AbstractBridgeOptimizer,
                 attr::MOI.ObjectiveFunction,
                 func::MOI.AbstractScalarFunction)
    if is_objective_bridged(b)
        # Clear objective function by setting sense to `MOI.FEASIBILITY_SENSE`
        # first. This is needed if the objective function of `b.model` is
        # `MOI.SingleVariable(slack)` where `slack` is the slack variable
        # created by `Objective.SlackBridge`.
        sense = MOI.get(b.model, MOI.ObjectiveSense())
        MOI.set(b.model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        _delete_objective_bridges(b)
        if sense != MOI.FEASIBILITY_SENSE
            MOI.set(b.model, MOI.ObjectiveSense(), sense)
        end
    end
    if Variable.has_bridges(Variable.bridges(b))
        if func isa MOI.SingleVariable
            if is_bridged(b, func.variable)
                BridgeType = Objective.concrete_bridge_type(
                    objective_functionize_bridge(b), typeof(func))
                _bridge_objective(b, BridgeType, func)
                return
            end
        else
            func = bridged_function(b, func)::typeof(func)
        end
    end
    if is_bridged(b, typeof(func))
        BridgeType = Objective.concrete_bridge_type(b, typeof(func))
        _bridge_objective(b, BridgeType, func)
    else
        MOI.set(b.model, attr, func)
    end
end
function MOI.modify(b::AbstractBridgeOptimizer, obj::MOI.ObjectiveFunction,
                     change::MOI.AbstractFunctionModification)
    if is_bridged(b, change)
        modify_bridged_change(b, obj, change)
    else
        if is_bridged(b, obj)
            MOI.modify(b, bridge(b, obj), change)
        else
            MOI.modify(b.model, obj, change)
        end
    end
end

# Variable attributes
function _index(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    i = Variable.index_in_vector_of_variables(Variable.bridges(b), vi)
    if iszero(i.value)
        return tuple()
    else
        return (i,)
    end
end

function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 index::MOI.VariableIndex)
    if is_bridged(b, index)
        value = MOI.get(b, attr, bridge(b, index), _index(b, index)...)
    else
        value = MOI.get(b.model, attr, index)
    end
    return unbridged_function(b, value)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 indices::Vector{MOI.VariableIndex})
    if any(index -> is_bridged(b, index), indices)
        return MOI.get.(b, attr, indices)
    else
        return unbridged_function.(b, MOI.get(b.model, attr, indices))
    end
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::MOI.AbstractVariableAttribute,
                      ::Type{MOI.VariableIndex})
    return MOI.supports(b.model, attr, MOI.VariableIndex)
end
function MOI.set(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 index::MOI.Index, value)
    value = bridged_function(b, value)
    if is_bridged(b, index)
        return MOI.set(b, attr, bridge(b, index), value, _index(b, index)...)
    else
        return MOI.set(b.model, attr, index, value)
    end
end
function MOI.set(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 indices::Vector{<:MOI.Index}, values::Vector)
    if any(index -> is_bridged(b, index), indices)
        return MOI.set.(b, attr, indices, values)
    else
        return MOI.set(b.model, attr, indices, bridged_function.(b, values))
    end
end

# Constraint attributes
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintFunction,
                 ci::MOI.ConstraintIndex{F}, func::F) where F
    if Variable.has_bridges(Variable.bridges(b))
        set = MOI.get(b, MOI.ConstraintSet(), ci)
        func, new_set = bridged_constraint_function(b, func, set)
        if new_set !== set
            _set_substituted(b, MOI.ConstraintSet(), ci, new_set)
        end
    end
    _set_substituted(b, attr, ci, func)
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ConstraintFunction,
                 ci::MOI.ConstraintIndex)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        br = bridge(b, ci)
        if br isa Variable.AbstractBridge
            return Variable.function_for(Variable.bridges(b), ci)
        end
        func = MOI.get(b, attr, br)
    else
        func = MOI.get(b.model, attr, ci)
    end
    return unbridged_constraint_function(b, func)
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintSet,
                 ci::MOI.ConstraintIndex{MOI.SingleVariable, S},
                 value::S) where S <: MOI.AbstractScalarSet
    _set_substituted(b, attr, ci, value)
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ConstraintSet,
                 ci::MOI.ConstraintIndex{MOI.SingleVariable})
    return if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        MOI.get(b, attr, bridge(b, ci))
    else
        MOI.get(b.model, attr, ci)
    end
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintSet,
                 ci::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction, S},
                 set::S) where S <: MOI.AbstractScalarSet
    if Variable.has_bridges(Variable.bridges(b))
        func = MOI.get(b, MOI.ConstraintFunction(), ci)
        new_func, set = bridged_constraint_function(b, func, set)
    end
    _set_substituted(b, attr, ci, set)
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ConstraintSet,
                 ci::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction})
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        set = MOI.get(b, attr, bridge(b, ci))
    else
        set = MOI.get(b.model, attr, ci)
    end
    if Variable.has_bridges(Variable.bridges(b))
        # The function constant of the bridged function was moved to the set,
        # we need to remove it.
        if is_bridged(b, ci)
            func = MOI.get(b, MOI.ConstraintFunction(), bridge(b, ci))
        else
            func = MOI.get(b.model, MOI.ConstraintFunction(), ci)
        end
        f = unbridged_function(b, func)
        set = MOIU.shift_constant(set, -MOI.constant(f))
    end
    return set
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractConstraintAttribute, ci::MOI.ConstraintIndex)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        func = MOI.get(b, attr, bridge(b, ci))
    else
        func = MOI.get(b.model, attr, ci)
    end
    return unbridged_function(b, func)
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::MOI.AbstractConstraintAttribute,
                      IndexType::Type{MOI.ConstraintIndex{F, S}}) where {F, S}
    return reduce_bridged(
        b, F, S, () -> true, () -> MOI.supports(b.model, attr, IndexType),
        ok -> ok && MOI.supports(b, attr, Variable.concrete_bridge_type(b, S)),
        ok -> ok && MOI.supports(b, attr, Constraint.concrete_bridge_type(b, F, S))
    )
end

function _set_substituted(
    b::AbstractBridgeOptimizer, attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex, value)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        MOI.set(b, attr, bridge(b, ci), value)
    else
        MOI.set(b.model, attr, ci, value)
    end
end
function MOI.set(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractConstraintAttribute,
                 ci::MOI.ConstraintIndex, value)
    _set_substituted(b, attr, ci, bridged_function(b, value))
end
## Getting and Setting names
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.VariableName,
                 vi::MOI.VariableIndex)
    if is_bridged(b, vi)
        return get(b.var_to_name, vi, MOIU.EMPTYSTRING)
    else
        return MOI.get(b.model, attr, vi)
    end
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.VariableName,
                 vi::MOI.VariableIndex, name::String)
    if is_bridged(b, vi)
        b.var_to_name[vi] = name
        b.name_to_var = nothing # Invalidate the name map.
    else
        MOI.set(b.model, attr, vi, name)
    end
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                 constraint_index::MOI.ConstraintIndex)
    if is_bridged(b, constraint_index)
        return get(b.con_to_name, constraint_index, MOIU.EMPTYSTRING)
    else
        return MOI.get(b.model, attr, constraint_index)
    end
end
function MOI.supports(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                      Index::Type{MOI.ConstraintIndex{F,S}}) where {F,S}
    return is_bridged(b, F, S) || MOI.supports(b.model, attr, Index)
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                 constraint_index::MOI.ConstraintIndex, name::String)
    if is_bridged(b, constraint_index)
        b.con_to_name[constraint_index] = name
        b.name_to_con = nothing # Invalidate the name map.
    else
        MOI.set(b.model, attr, constraint_index, name)
    end
end

# Query index from name (similar to `UniversalFallback`)
function MOI.get(b::AbstractBridgeOptimizer, ::Type{MOI.VariableIndex},
                 name::String)
    vi = MOI.get(b.model, MOI.VariableIndex, name)
    if Variable.has_bridges(Variable.bridges(b))
        if b.name_to_var === nothing
            b.name_to_var = MOIU.build_name_to_var_map(b.var_to_name)
        end
        vi_bridged = get(b.name_to_var, name, nothing)
        MOIU.throw_if_multiple_with_name(vi_bridged, name)
        return MOIU.check_type_and_multiple_names(
            MOI.VariableIndex, vi_bridged, vi, name)
    else
        return vi
    end
end
function MOI.get(b::AbstractBridgeOptimizer,
                 IdxT::Type{MOI.ConstraintIndex{F, S}},
                 name::String) where {F, S}
    if !Constraint.has_bridges(Constraint.bridges(b)) &&
        !Variable.has_bridges(Variable.bridges(b))
        # `name_to_con` is not defined for `Objective.SingleBridgeOptimizer`.
        return MOI.get(b.model, IdxT, name)
    end
    if b.name_to_con === nothing
        b.name_to_con = MOIU.build_name_to_con_map(b.con_to_name)
    end
    if is_bridged(b, F, S)
        # There is no `F`-in-`S` constraint in `b.model`, `ci` is only got
        # to check for duplicate names.
        ci = MOI.get(b.model, MOI.ConstraintIndex, name)
    else
        ci = MOI.get(b.model, IdxT, name)
    end
    ci_bridged = get(b.name_to_con, name, nothing)
    MOIU.throw_if_multiple_with_name(ci_bridged, name)
    return MOIU.check_type_and_multiple_names(IdxT, ci_bridged, ci, name)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 IdxT::Type{MOI.ConstraintIndex},
                 name::String)
    if !Constraint.has_bridges(Constraint.bridges(b)) &&
        !Variable.has_bridges(Variable.bridges(b))
        # `name_to_con` is not defined for `Objective.SingleBridgeOptimizer`.
        return MOI.get(b.model, IdxT, name)
    end
    if b.name_to_con === nothing
        b.name_to_con = MOIU.build_name_to_con_map(b.con_to_name)
    end
    ci_bridged = get(b.name_to_con, name, nothing)
    MOIU.throw_if_multiple_with_name(ci_bridged, name)
    return MOIU.check_type_and_multiple_names(
        IdxT, ci_bridged, MOI.get(b.model, IdxT, name), name)
end

# Constraints
function MOI.supports_constraint(b::AbstractBridgeOptimizer,
                                 F::Type{<:MOI.AbstractFunction},
                                 S::Type{<:MOI.AbstractSet})
    if is_bridged(b, F, S)
        if F == MOIU.variable_function_type(S) &&
            supports_bridging_constrained_variable(b, S)
            return true
        end
        return supports_bridging_constraint(b, F, S)
    else
        return MOI.supports_constraint(b.model, F, S)
    end
end
function MOI.add_constraint(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction,
                            s::MOI.AbstractSet)
    if Variable.has_bridges(Variable.bridges(b))
        if f isa MOI.SingleVariable
            if is_bridged(b, f.variable)
                if MOI.is_valid(b, MOI.ConstraintIndex{MOI.SingleVariable, typeof(s)}(f.variable.value))
                    # The other constraint could have been through a variable bridge.
                    error("Cannot add two `SingleVariable`-in-`$(typeof(s))`",
                          " on the same variable $(f.variable).")
                end
                BridgeType = Constraint.concrete_bridge_type(
                    constraint_scalar_functionize_bridge(b), typeof(f), typeof(s))
                bridge = Constraint.bridge_constraint(BridgeType, b, f, s)
                return Constraint.add_key_for_bridge(Constraint.bridges(b), bridge, f, s)
            end
        elseif f isa MOI.VectorOfVariables
            if any(vi -> is_bridged(b, vi), f.variables)
                if MOI.is_valid(b, MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(s)}(first(f.variables).value))
                    # The other constraint could have been through a variable bridge.
                    error("Cannot add two `VectorOfVariables`-in-`$(typeof(s))`",
                          " on the same first variable $(first(f.variables)).")
                end
                if !is_bridged(b, first(f.variables)) && !is_bridged(b, typeof(f), typeof(s))
                    # The index of the contraint will have positive value hence
                    # it would clash with the index space of `b.model` since
                    # the constraint type is normally not bridged.
                    error("Cannot `VectorOfVariables`-in-`$(typeof(s))` for",
                          " which some variables are bridged but not the",
                          " first one `$(first(f.variables))`.")
                end
                BridgeType = Constraint.concrete_bridge_type(
                    constraint_vector_functionize_bridge(b), typeof(f), typeof(s))
                bridge = Constraint.bridge_constraint(BridgeType, b, f, s)
                return Constraint.add_key_for_bridge(Constraint.bridges(b), bridge, f, s)
            end
        else
            f, s = bridged_constraint_function(b, f, s)
        end
    end
    if is_bridged(b, typeof(f), typeof(s))
        # We compute `BridgeType` first as `concrete_bridge_type` calls
        # `bridge_type` which might throw an `UnsupportedConstraint` error in
        # which case, we do not want any modification to have been done
        BridgeType = Constraint.concrete_bridge_type(b, typeof(f), typeof(s))
        # `add_constraint` might throw an `UnsupportedConstraint` but no
        # modification has been done in the previous line
        bridge = Constraint.bridge_constraint(BridgeType, b, f, s)
        return Constraint.add_key_for_bridge(Constraint.bridges(b), bridge, f, s)
    else
        return MOI.add_constraint(b.model, f, s)
    end
end
function MOI.add_constraints(b::AbstractBridgeOptimizer, f::Vector{F},
                             s::Vector{S}) where { F <: MOI.AbstractFunction,
                             S <: MOI.AbstractSet}
    if is_bridged(b, F, S)
        return MOI.add_constraint.(b, f, s)
    else
        if Variable.has_bridges(Variable.bridges(b))
            if S == MOI.SingleVariable
                if any(func -> is_bridged(b, func.variable), f)
                    return MOI.add_constraint.(b, f, s)
                end
            elseif S == MOI.VectorOfVariables
                if any(func -> any(vi -> is_bridged(b, vi), func.variables), f)
                    return MOI.add_constraint.(b, f, s)
                end
            else
                f = F[bridged_function(b, func)::F for func in f]
            end
        end
        return MOI.add_constraints(b.model, f, s)
    end
end
function is_bridged(::AbstractBridgeOptimizer,
                    ::Union{MOI.ScalarConstantChange, MOI.VectorConstantChange})
    return false
end
function is_bridged(b::AbstractBridgeOptimizer,
                    change::Union{MOI.ScalarCoefficientChange, MOI.MultirowChange})
    return is_bridged(b, change.variable)
end
function modify_bridged_change(b::AbstractBridgeOptimizer, obj,
                               change::MOI.MultirowChange)
    func = bridged_variable_function(b, change.variable)::MOI.ScalarAffineFunction
    if !iszero(func.constant)
        # We would need to get the constant in the function, and the
        # coefficient of `change.variable` to remove its contribution
        # to the constant and then modify the constant.
        MOI.throw_modify_not_allowed(
            obj, change, "The change $change contains variables bridged into" *
            " a function with nonzero constant.")
    end
    for t in func.terms
        coefs = [(i, coef * t.coefficient) for (i, coef) in change.new_coefficients]
        MOI.modify(b, obj, MOI.MultirowChange(t.variable_index, coefs))
    end
end
function modify_bridged_change(b::AbstractBridgeOptimizer, obj,
                               change::MOI.ScalarCoefficientChange)
    func = bridged_variable_function(b, change.variable)::MOI.ScalarAffineFunction
    if !iszero(func.constant)
        # We would need to get the constant in the set, and the
        # coefficient of `change.variable` to remove its contribution
        # to the constant and then modify the constant.
        MOI.throw_modify_not_allowed(
            obj, change, "The change $change contains variables bridged into" *
            " a function with nonzero constant.")
    end
    for t in func.terms
        coef = t.coefficient * change.new_coefficient
        MOI.modify(b, obj, MOI.ScalarCoefficientChange(t.variable_index, coef))
    end
end
function MOI.modify(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex,
                    change::MOI.AbstractFunctionModification)
    if is_bridged(b, change)
        modify_bridged_change(b, ci, change)
    else
        if is_bridged(b, ci)
            MOI.modify(b, bridge(b, ci), change)
        else
            MOI.modify(b.model, ci, change)
        end
    end
end

# Variables
function MOI.add_variable(b::AbstractBridgeOptimizer)
    if is_bridged(b, MOI.Reals)
        variables, constraint = MOI.add_constrained_variables(b, MOI.Reals(1))
        @assert isone(length(variables))
        return first(variables)
    else
        return MOI.add_variable(b.model)
    end
end
function MOI.add_variables(b::AbstractBridgeOptimizer, n)
    if is_bridged(b, MOI.Reals)
        variables, constraint = MOI.add_constrained_variables(b, MOI.Reals(n))
        return variables
    else
        return MOI.add_variables(b.model, n)
    end
end

function MOI.add_constrained_variables(b::AbstractBridgeOptimizer,
                                       set::MOI.AbstractVectorSet)
    if is_bridged(b, typeof(set)) ||
        is_bridged(b, MOI.VectorOfVariables, typeof(set))
        if set isa MOI.Reals || supports_bridging_constrained_variable(b, typeof(set))
            BridgeType = Variable.concrete_bridge_type(b, typeof(set))
            bridge = Variable.bridge_constrained_variable(BridgeType, b, set)
            return Variable.add_keys_for_bridge(Variable.bridges(b), bridge, set)
        else
            variables = MOI.add_variables(b, MOI.dimension(set))
            constraint = MOI.add_constraint(b, MOI.VectorOfVariables(variables), set)
            return variables, constraint
        end
    else
        return MOI.add_constrained_variables(b.model, set)
    end
end
function MOI.add_constrained_variable(b::AbstractBridgeOptimizer,
                                      set::MOI.AbstractScalarSet)
    if is_bridged(b, typeof(set)) ||
        is_bridged(b, MOI.SingleVariable, typeof(set))
        if supports_bridging_constrained_variable(b, typeof(set))
            BridgeType = Variable.concrete_bridge_type(b, typeof(set))
            bridge = Variable.bridge_constrained_variable(BridgeType, b, set)
            return Variable.add_key_for_bridge(Variable.bridges(b), bridge, set)
        else
            variable = MOI.add_variable(b)
            constraint = MOI.add_constraint(b, MOI.SingleVariable(variable), set)
            return variable, constraint
        end
    else
        return MOI.add_constrained_variable(b.model, set)
    end
end


"""
    bridged_variable_function(b::AbstractBridgeOptimizer,
                              vi::MOI.VariableIndex)

Return a `MOI.AbstractScalarFunction` of variables of `b.model` that equals
`vi`. That is, if the variable `vi` is bridged, it returns its expression in
terms of the variables of `b.model`. Otherwise, it returns
`MOI.SingleVariable(vi)`.
"""
function bridged_variable_function(b::AbstractBridgeOptimizer,
                                   vi::MOI.VariableIndex)
    if is_bridged(b, vi)
        func = bridged_function(bridge(b, vi), _index(b, vi)...)
        # If two variable bridges are chained, `func` may still contain
        # bridged variables.
        return bridged_function(b, func)
    else
        return MOI.SingleVariable(vi)
    end
end

"""
    bridged_function(b::AbstractBridgeOptimizer, value)::typeof(value)

Substitute any bridged [`MOI.VariableIndex`](@ref) in `value` by an equivalent
expression in terms of variables of `b.model`.
"""
function bridged_function(bridge::AbstractBridgeOptimizer, value)
    if !Variable.has_bridges(Variable.bridges(bridge))
        # Shortcut, this allows performance to be unaltered when no variable
        # bridges are used.
        return value
    end
    # We assume that the type of `value` is not altered. This restricts
    # variable bridges to only return `ScalarAffineFunction` but otherwise,
    # the peformance would be bad.
    return MOIU.substitute_variables(
        vi -> bridged_variable_function(bridge, vi), value)::typeof(value)
end
function bridged_function(b::AbstractBridgeOptimizer,
                          func::MOI.SingleVariable)
    # Should not be called by `add_constraint` as it force-bridges it
    # but could be called by attributes
    if is_bridged(b, func.variable)
        # It could be solved by force-bridging the attribues (e.g. objective).
        error("Using bridged variable in `SingleVariable` function.")
    end
    return func
end
# Shortcut to avoid `Variable.throw_if_cannot_unbridge(Variable.bridges(b))`
function bridge_function(
    ::AbstractBridgeOptimizer, value::MOIU.ObjectOrTupleOrArrayWithoutIndex)
    return value
end


"""
    unbridged_variable_function(b::AbstractBridgeOptimizer,
                                vi::MOI.VariableIndex)

Return a `MOI.AbstractScalarFunction` of variables of `b` that equals `vi`.
That is, if the variable `vi` is an internal variable of `b.model` created by a
but not visible to the user, it returns its expression in terms of the variables
of bridged variables. Otherwise, it returns `MOI.SingleVariable(vi)`.
"""
function unbridged_variable_function(b::AbstractBridgeOptimizer,
                                     vi::MOI.VariableIndex)
    func = Variable.unbridged_function(Variable.bridges(b), vi)
    if func === nothing
        return MOI.SingleVariable(vi)
    else
        # If two variable bridges are chained, `func` may still contain
        # variables to unbridge.
        return unbridged_function(b, func)
    end
end

"""
    unbridged_function(b::AbstractBridgeOptimizer, value)::typeof(value)

Substitute any internal [`MOI.VariableIndex`](@ref) of `b.model` created by a
variable bridge but not visible to the user in `value` by an equivalent
expression in terms of bridged variables.
"""
function unbridged_function(b::AbstractBridgeOptimizer, value)
    if !Variable.has_bridges(Variable.bridges(b))
        return value
    end
    # If `value` does not contain any variable, this will never call
    # `unbridged_variable_function` hence it might silently return an incorrect
    # value so we call `throw_if_cannot_unbridge` here.
    Variable.throw_if_cannot_unbridge(Variable.bridges(b))
    return MOIU.substitute_variables(
        vi -> unbridged_variable_function(b, vi), value)::typeof(value)
end
function unbridged_function(bridge::AbstractBridgeOptimizer,
                            func::Union{MOI.SingleVariable, MOI.VectorOfVariables})
    return func # bridged variables are not allowed in non-bridged constraints
end
# Shortcut to avoid `Variable.throw_if_cannot_unbridge(Variable.bridges(b))`
function unbridged_function(
    ::AbstractBridgeOptimizer, value::MOIU.ObjectOrTupleOrArrayWithoutIndex)
    return value
end

"""
    bridged_constraint_function(
       b::AbstractBridgeOptimizer,
       func::MOI.AbstractFunction,
       set::MOI.AbstractSet
    )

Similar to `bridged_function(b, func)` but move function constant to the set if
it is scalar. Also return the modified set.
"""
function bridged_constraint_function end

function bridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.AbstractVectorFunction,
    set::MOI.AbstractVectorSet)
    return bridged_function(b, func), set
end
function bridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.AbstractScalarFunction,
    set::MOI.AbstractScalarSet)
    if !Variable.has_bridges(Variable.bridges(b))
        return func, set
    end
    if func isa MOI.AbstractScalarFunction
        constant = MOI.constant(func)
        if !iszero(constant)
            # We use the fact that the initial function constant was zero to
            # implement getters for `MOI.ConstraintFunction` and
            # `MOI.ConstraintSet`. See `unbridged_constraint_function`.
            throw(MOI.ScalarFunctionConstantNotZero{
                typeof(constant), typeof(func), typeof(set)}(constant))
        end
    end
    f = bridged_function(b, func)::typeof(func)
    return MOIU.normalize_constant(f, set)
end
function bridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.SingleVariable,
    set::MOI.AbstractScalarSet)
    return bridged_function(b, func), set
end

"""
    unbridged_constraint_function(
       b::AbstractBridgeOptimizer,
       func::MOI.AbstractFunction
    )

Similar to `unbridged_function(b, func)` but zero the function constant if it is
scalar.
"""
function unbridged_constraint_function end

function unbridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.AbstractVectorFunction)
    return unbridged_function(b, func)
end
function unbridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.AbstractScalarFunction)
    if !Variable.has_bridges(Variable.bridges(b))
        return func
    end
    f = unbridged_function(b, func)::typeof(func)
    if !iszero(MOI.constant(f))
        f = copy(f)
        f.constant = zero(f.constant)
    end
    return f
end
function unbridged_constraint_function(
    b::AbstractBridgeOptimizer, func::MOI.SingleVariable)
    return unbridged_function(b, func)
end


# TODO add transform
