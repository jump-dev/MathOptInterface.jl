## Storage of constraints
#
# All `F`-in-`S` constraints are stored in a vector of `ConstraintEntry{F, S}`.
# The index in this vector of a constraint of index
# `ci::MOI.ConstraintIndex{F, S}` is given by `model.constrmap[ci.value]`. The
# advantage of this representation is that it does not require any dictionary
# hence it never needs to compute a hash.
#
# It may seem redundant to store the constraint index `ci` as well as the
# function and sets in the tuple but it is used to efficiently implement the
# getter for `MOI.ListOfConstraintIndices{F, S}`. It is also used to implement
# `MOI.delete`. Indeed, when a constraint is deleted, it is removed from the
# vector hence the index in the vector of all the functions that were stored
# after must be decreased by one. As the constraint index is stored in the
# vector, it readily gives the entries of `model.constrmap` that need to be
# updated.
const ConstraintEntry{F, S} = Tuple{CI{F, S}, F, S}

const EMPTYSTRING = ""

# Implementation of MOI for vector of constraint
function _add_constraint(constrs::Vector{ConstraintEntry{F, S}}, ci::CI, f::F,
                         s::S) where {F, S}
    push!(constrs, (ci, f, s))
    length(constrs)
end

function _delete(constrs::Vector, ci::CI, i::Int)
    deleteat!(constrs, i)
    @view constrs[i:end] # will need to shift it in constrmap
end

_getindex(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = ci
function _getindex(constrs::Vector, ci::CI, i::Int)
    _getindex(constrs[i]...)
end

_getfun(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = f
function _getfunction(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _getfun(constrs[i]...)
end

_gets(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = s
function _getset(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _gets(constrs[i]...)
end

_modifyconstr(ci::CI{F, S}, f::F, s::S, change::F) where {F, S} = (ci, change, s)
_modifyconstr(ci::CI{F, S}, f::F, s::S, change::S) where {F, S} = (ci, f, change)
_modifyconstr(ci::CI{F, S}, f::F, s::S, change::MOI.AbstractFunctionModification) where {F, S} = (ci, modify_function(f, change), s)
function _modify(constrs::Vector{ConstraintEntry{F, S}}, ci::CI{F}, i::Int,
                 change) where {F, S}
    constrs[i] = _modifyconstr(constrs[i]..., change)
end

function _getnoc(constrs::Vector{ConstraintEntry{F, S}},
                 ::MOI.NumberOfConstraints{F, S}) where {F, S}
    return length(constrs)
end
# Might be called when calling NumberOfConstraint with different coefficient type than the one supported
_getnoc(::Vector, ::MOI.NumberOfConstraints) = 0

function _getloc(constrs::Vector{ConstraintEntry{F, S}})::Vector{Tuple{DataType, DataType}} where {F, S}
    isempty(constrs) ? [] : [(F, S)]
end

function _getlocr(constrs::Vector{ConstraintEntry{F, S}},
                  ::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    return map(constr -> constr[1], constrs)
end
function _getlocr(constrs::Vector{<:ConstraintEntry},
                  ::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    return CI{F, S}[]
end

# Implementation of MOI for AbstractModel
abstract type AbstractModel{T} <: MOI.ModelLike end

getconstrloc(model::AbstractModel, ci::CI) = model.constrmap[ci.value]

# Variables
function MOI.get(model::AbstractModel, ::MOI.NumberOfVariables)::Int64
    if model.variable_indices === nothing
        model.num_variables_created
    else
        length(model.variable_indices)
    end
end
function MOI.add_variable(model::AbstractModel{T}) where T
    vi = VI(model.num_variables_created += 1)
    push!(model.single_variable_mask, 0x0)
    push!(model.lower_bound, zero(T))
    push!(model.upper_bound, zero(T))
    if model.variable_indices !== nothing
        push!(model.variable_indices, vi)
    end
    return vi
end
function MOI.add_variables(model::AbstractModel, n::Integer)
    return [MOI.add_variable(model) for i in 1:n]
end

"""
    remove_variable(f::MOI.AbstractFunction, s::MOI.AbstractSet, vi::MOI.VariableIndex)

Return a tuple `(g, t)` representing the constraint `f`-in-`s` with the
variable `vi` removed. That is, the terms containing the variable `vi` in the
function `f` are removed and the dimension of the set `s` is updated if
needed (e.g. when `f` is a `VectorOfVariables` with `vi` being one of the
variables).
"""
remove_variable(f, s, vi::VI) = remove_variable(f, vi), s
function remove_variable(f::MOI.VectorOfVariables, s, vi::VI)
    g = remove_variable(f, vi)
    if length(g.variables) != length(f.variables)
        t = MOI.update_dimension(s, length(g.variables))
    else
        t = s
    end
    return g, t
end
function _remove_variable(constrs::Vector, vi::VI)
    for i in eachindex(constrs)
        ci, f, s = constrs[i]
        constrs[i] = (ci, remove_variable(f, s, vi)...)
    end
    return CI{MOI.SingleVariable}[]
end
function _vector_of_variables_with(::Vector, ::Union{VI, MOI.Vector{VI}})
    return CI{MOI.VectorOfVariables}[]
end
function throw_delete_variable_in_vov(vi::VI)
    message = string("Cannot delete variable as it is constrained with other",
                     " variables in a `MOI.VectorOfVariables`.")
    throw(MOI.DeleteNotAllowed(vi, message))
end
function _vector_of_variables_with(
    constrs::Vector{<:ConstraintEntry{MOI.VectorOfVariables}}, vi::VI)
    rm = CI{MOI.VectorOfVariables}[]
    for (ci, f, s) in constrs
        if vi in f.variables
            if length(f.variables) > 1
                # If `supports_dimension_update(s)` then the variable will be
                # removed in `_remove_variable`.
                if !MOI.supports_dimension_update(typeof(s))
                    throw_delete_variable_in_vov(vi)
                end
            else
                push!(rm, ci)
            end
        end
    end
    return rm
end
function _vector_of_variables_with(
    constrs::Vector{<:ConstraintEntry{MOI.VectorOfVariables}},
    vis::Vector{VI}
)
    rm = CI{MOI.VectorOfVariables}[]
    for (ci, f, s) in constrs
        if vis == f.variables
            push!(rm, ci)
        end
    end
    return rm
end
function MOI.delete(model::AbstractModel{T}, vi::VI) where T
    MOI.throw_if_not_valid(model, vi)
    model.objective = remove_variable(model.objective, vi)
    # If a variable is removed, the `VectorOfVariables` constraints using this
    # variable only need to be removed too. `vov_to_remove` is the list of
    # indices of the `VectorOfVariables` constraints of `vi`.
    vov_to_remove = broadcastvcat(constrs -> _vector_of_variables_with(constrs, vi), model)
    for ci in vov_to_remove
        MOI.delete(model, ci)
    end
    # `VectorOfVariables` constraints with sets not supporting dimension update
    # were either deleted or an error was thrown. The rest is modified now.
    broadcastcall(constrs -> _remove_variable(constrs, vi), model)
    model.single_variable_mask[vi.value] = 0x0
    if model.variable_indices === nothing
        model.variable_indices = Set(MOI.get(model,
                                             MOI.ListOfVariableIndices()))
    end
    delete!(model.variable_indices, vi)
    model.name_to_var = nothing
    delete!(model.var_to_name, vi)
    model.name_to_con = nothing
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Semicontinuous{T}}(vi.value))
    delete!(model.con_to_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Semiinteger{T}}(vi.value))
end
function MOI.delete(model::AbstractModel, vis::Vector{VI})
    # Delete `VectorOfVariables(vis)` constraints as otherwise, it will error
    # when removing variables one by one.
    vov_to_remove = broadcastvcat(constrs -> _vector_of_variables_with(constrs, vis), model)
    for ci in vov_to_remove
        MOI.delete(model, ci)
    end
    for vi in vis
        MOI.delete(model, vi)
    end
end

function MOI.is_valid(model::AbstractModel,
                      ci::CI{MOI.SingleVariable, S}) where {S}
    return 1 ≤ ci.value ≤ length(model.single_variable_mask) &&
           !iszero(model.single_variable_mask[ci.value] & single_variable_flag(S))
end
function MOI.is_valid(model::AbstractModel, ci::CI{F, S}) where {F, S}
    if ci.value > length(model.constrmap)
        false
    else
        loc = getconstrloc(model, ci)
        if iszero(loc) # This means that it has been deleted
            false
        elseif loc > MOI.get(model, MOI.NumberOfConstraints{F, S}())
            false
        else
            ci == _getindex(model, ci, getconstrloc(model, ci))
        end
    end
end
function MOI.is_valid(model::AbstractModel, vi::VI)
    if model.variable_indices === nothing
        return 1 ≤ vi.value ≤ model.num_variables_created
    else
        return in(vi, model.variable_indices)
    end
end

function MOI.get(model::AbstractModel, ::MOI.ListOfVariableIndices)
    if model.variable_indices === nothing
        return VI.(1:model.num_variables_created)
    else
        vis = collect(model.variable_indices)
        sort!(vis, by=vi->vi.value) # It needs to be sorted by order of creation
        return vis
    end
end

# Names
MOI.supports(::AbstractModel, ::MOI.Name) = true
function MOI.set(model::AbstractModel, ::MOI.Name, name::String)
    model.name = name
end
MOI.get(model::AbstractModel, ::MOI.Name) = model.name

MOI.supports(::AbstractModel, ::MOI.VariableName, vi::Type{VI}) = true
function MOI.set(model::AbstractModel, ::MOI.VariableName, vi::VI, name::String)
    model.var_to_name[vi] = name
    model.name_to_var = nothing # Invalidate the name map.
end
MOI.get(model::AbstractModel, ::MOI.VariableName, vi::VI) = get(model.var_to_name, vi, EMPTYSTRING)

"""
    build_name_to_var_map(con_to_name::Dict{MOI.VariableIndex, String})

Create and return a reverse map from name to variable index, given a map from
variable index to name. The special value `MOI.VariableIndex(0)` is used to
indicate that multiple variables have the same name.
"""
function build_name_to_var_map(var_to_name::Dict{VI, String})
    name_to_var = Dict{String, VI}()
    for (var, var_name) in var_to_name
        if haskey(name_to_var, var_name)
            # 0 is a special value that means this string does not map to
            # a unique variable name.
            name_to_var[var_name] = VI(0)
        else
            name_to_var[var_name] = var
        end
    end
    return name_to_var
end

function throw_multiple_name_error(::Type{MOI.VariableIndex}, name::String)
    error("Multiple variables have the name $name.")
end
function throw_multiple_name_error(::Type{<:MOI.ConstraintIndex}, name::String)
    error("Multiple constraints have the name $name.")
end
function throw_if_multiple_with_name(::Nothing, ::String) end
function throw_if_multiple_with_name(index::MOI.Index, name::String)
    if iszero(index.value)
        throw_multiple_name_error(typeof(index), name)
    end
end

function MOI.get(model::AbstractModel, ::Type{VI}, name::String)
    if model.name_to_var === nothing
        # Rebuild the map.
        model.name_to_var = build_name_to_var_map(model.var_to_name)
    end
    result = get(model.name_to_var, name, nothing)
    throw_if_multiple_with_name(result, name)
    return result
end

function MOI.get(model::AbstractModel, ::MOI.ListOfVariableAttributesSet)::Vector{MOI.AbstractVariableAttribute}
    isempty(model.var_to_name) ? [] : [MOI.VariableName()]
end

MOI.supports(model::AbstractModel, ::MOI.ConstraintName, ::Type{<:CI}) = true
function MOI.set(model::AbstractModel, ::MOI.ConstraintName, ci::CI, name::String)
    model.con_to_name[ci] = name
    model.name_to_con = nothing # Invalidate the name map.
end
MOI.get(model::AbstractModel, ::MOI.ConstraintName, ci::CI) = get(model.con_to_name, ci, EMPTYSTRING)

"""
    build_name_to_con_map(con_to_name::Dict{MOI.ConstraintIndex, String})

Create and return a reverse map from name to constraint index, given a map from
constraint index to name. The special value
`MOI.ConstraintIndex{Nothing, Nothing}(0)` is used to indicate that multiple
constraints have the same name.
"""
function build_name_to_con_map(con_to_name::Dict{CI, String})
    name_to_con = Dict{String, CI}()
    for (con, con_name) in con_to_name
        if haskey(name_to_con, con_name)
            name_to_con[con_name] = CI{Nothing, Nothing}(0)
        else
            name_to_con[con_name] = con
        end
    end
    return name_to_con
end


function MOI.get(model::AbstractModel, ConType::Type{<:CI}, name::String)
    if model.name_to_con === nothing
        # Rebuild the map.
        model.name_to_con = build_name_to_con_map(model.con_to_name)
    end
    ci = get(model.name_to_con, name, nothing)
    throw_if_multiple_with_name(ci, name)
    if ci isa ConType
        return ci
    else
        return nothing
    end
end

function MOI.get(model::AbstractModel, ::MOI.ListOfConstraintAttributesSet)::Vector{MOI.AbstractConstraintAttribute}
    isempty(model.con_to_name) ? [] : [MOI.ConstraintName()]
end

# Objective
MOI.get(model::AbstractModel, ::MOI.ObjectiveSense) = model.sense
MOI.supports(model::AbstractModel, ::MOI.ObjectiveSense) = true
function MOI.set(model::AbstractModel, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    model.senseset = true
    model.sense = sense
end
function MOI.get(model::AbstractModel, ::MOI.ObjectiveFunctionType)
    return MOI.typeof(model.objective)
end
function MOI.get(model::AbstractModel, ::MOI.ObjectiveFunction{T})::T where T
    return model.objective
end
MOI.supports(model::AbstractModel, ::MOI.ObjectiveFunction) = true
function MOI.set(model::AbstractModel, ::MOI.ObjectiveFunction, f::MOI.AbstractFunction)
    model.objectiveset = true
    # f needs to be copied, see #2
    model.objective = copy(f)
end

function MOI.modify(model::AbstractModel, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification)
    model.objective = modify_function(model.objective, change)
end

MOI.get(::AbstractModel, ::MOI.ListOfOptimizerAttributesSet) = MOI.AbstractOptimizerAttribute[]
function MOI.get(model::AbstractModel, ::MOI.ListOfModelAttributesSet)::Vector{MOI.AbstractModelAttribute}
    listattr = MOI.AbstractModelAttribute[]
    if model.senseset
        push!(listattr, MOI.ObjectiveSense())
    end
    if model.objectiveset
        push!(listattr, MOI.ObjectiveFunction{typeof(model.objective)}())
    end
    if !isempty(model.name)
        push!(listattr, MOI.Name())
    end
    listattr
end

# Constraints
single_variable_flag(::Type{<:MOI.EqualTo}) = 0x1
single_variable_flag(::Type{<:MOI.GreaterThan}) = 0x2
single_variable_flag(::Type{<:MOI.LessThan}) = 0x4
single_variable_flag(::Type{<:MOI.Interval}) = 0x8
single_variable_flag(::Type{MOI.Integer}) = 0x10
single_variable_flag(::Type{MOI.ZeroOne}) = 0x20
single_variable_flag(::Type{<:MOI.Semicontinuous}) = 0x40
single_variable_flag(::Type{<:MOI.Semiinteger}) = 0x80
# If a set is added here, a line should be added in
# `MOI.delete(::AbstractModel, ::MOI.VariableIndex)`

function flag_to_set_type(flag::UInt8, T::Type)
    if flag == 0x1
        return MOI.EqualTo{T}
    elseif flag == 0x2
        return MOI.GreaterThan{T}
    elseif flag == 0x4
        return MOI.LessThan{T}
    elseif flag == 0x8
        return MOI.Interval{T}
    elseif flag == 0x10
        return MOI.Integer
    elseif flag == 0x20
        return MOI.ZeroOne
    elseif flag == 0x40
        return MOI.Semicontinuous{T}
    elseif flag == 0x80
        return MOI.Semiinteger{T}
    else
        # $flag would print it in decimal
        error("Invalid flag `$(sprint(show, flag))`.")
    end
end

function throw_if_lower_bound_set(variable, S2, mask, T)
    flag = single_variable_flag(S2)
    if !iszero(flag & LOWER_BOUND_MASK) && !iszero(mask & LOWER_BOUND_MASK)
       S1 = flag_to_set_type(mask & LOWER_BOUND_MASK, T)
       throw(MOI.LowerBoundAlreadySet{S1, S2}(variable))
    end
end

function throw_if_upper_bound_set(variable, S2, mask, T)
    flag = single_variable_flag(S2)
    if !iszero(flag & UPPER_BOUND_MASK) && !iszero(mask & UPPER_BOUND_MASK)
       S1 = flag_to_set_type(mask & UPPER_BOUND_MASK, T)
       throw(MOI.UpperBoundAlreadySet{S1, S2}(variable))
    end
end

# Sets setting lower bound:
extract_lower_bound(set::MOI.EqualTo) = set.value
function extract_lower_bound(set::Union{MOI.GreaterThan, MOI.Interval,
                                        MOI.Semicontinuous, MOI.Semiinteger})
    return set.lower
end
# 0xb = 0x80 | 0x40 | 0x8 | 0x2 | 0x1
const LOWER_BOUND_MASK = 0xcb

# Sets setting upper bound:
extract_upper_bound(set::MOI.EqualTo) = set.value
function extract_upper_bound(set::Union{MOI.LessThan, MOI.Interval,
                                        MOI.Semicontinuous, MOI.Semiinteger})
    return set.upper
end
# 0xd = 0x80 | 0x40 | 0x8 | 0x4 | 0x1
const UPPER_BOUND_MASK = 0xcd

function MOI.supports_constraint(
    ::AbstractModel{T}, ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.EqualTo{T}, MOI.GreaterThan{T}, MOI.LessThan{T},
                   MOI.Interval{T}, MOI.Integer, MOI.ZeroOne}}) where T
    return true
end
function MOI.add_constraint(model::AbstractModel{T}, f::MOI.SingleVariable,
                            s::MOI.AbstractScalarSet) where T
    if MOI.supports_constraint(model, MOI.SingleVariable, typeof(s))
        flag = single_variable_flag(typeof(s))
        index = f.variable.value
        mask = model.single_variable_mask[index]
        throw_if_lower_bound_set(f.variable, typeof(s), mask, T)
        throw_if_upper_bound_set(f.variable, typeof(s), mask, T)
        # No error should be thrown now, we can modify `model`.
        if !iszero(flag & LOWER_BOUND_MASK)
            model.lower_bound[index] = extract_lower_bound(s)
        end
        if !iszero(flag & UPPER_BOUND_MASK)
            model.upper_bound[index] = extract_upper_bound(s)
        end
        model.single_variable_mask[index] = mask | flag
        return CI{MOI.SingleVariable, typeof(s)}(index)
    else
        throw(MOI.UnsupportedConstraint{MOI.SingleVariable, typeof(s)}())
    end
end
function MOI.add_constraint(model::AbstractModel, f::F, s::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    if MOI.supports_constraint(model, F, S)
        # We give the index value `nextconstraintid + 1` to the new constraint.
        # As the same counter is used for all pairs of F-in-S constraints,
        # the index value is unique across all constraint types as mentioned in
        # `@model`'s doc.
        ci = CI{F, S}(model.nextconstraintid += 1)
        # f needs to be copied, see #2
        push!(model.constrmap, _add_constraint(model, ci, copy(f), copy(s)))
        return ci
    else
        throw(MOI.UnsupportedConstraint{F, S}())
    end
end

function _delete_constraint(model::AbstractModel,
                            ci::CI{MOI.SingleVariable, S}) where S
    model.single_variable_mask[ci.value] &= ~single_variable_flag(S)
end
function _delete_constraint(model::AbstractModel, ci::CI)
    for (ci_next, _, _) in _delete(model, ci, getconstrloc(model, ci))
        model.constrmap[ci_next.value] -= 1
    end
    model.constrmap[ci.value] = 0
end
function MOI.delete(model::AbstractModel, ci::CI)
    MOI.throw_if_not_valid(model, ci)
    _delete_constraint(model, ci)
    model.name_to_con = nothing
    delete!(model.con_to_name, ci)
end

function MOI.modify(model::AbstractModel, ci::CI, change::MOI.AbstractFunctionModification)
    _modify(model, ci, getconstrloc(model, ci), change)
end

function MOI.set(model::AbstractModel, ::MOI.ConstraintFunction, ci::CI{MOI.SingleVariable},
                 change::MOI.AbstractFunction)
    throw(MOI.SettingSingleVariableFunctionNotAllowed())
end
function MOI.set(model::AbstractModel, ::MOI.ConstraintFunction, ci::CI, change::MOI.AbstractFunction)
    _modify(model, ci, getconstrloc(model, ci), change)
end
function MOI.set(model::AbstractModel, ::MOI.ConstraintSet,
                 ci::CI{MOI.SingleVariable}, change::MOI.AbstractSet)
    MOI.throw_if_not_valid(model, ci)
    flag = single_variable_flag(typeof(change))
    if !iszero(flag & LOWER_BOUND_MASK)
        model.lower_bound[ci.value] = extract_lower_bound(change)
    end
    if !iszero(flag & UPPER_BOUND_MASK)
        model.upper_bound[ci.value] = extract_upper_bound(change)
    end
end
function MOI.set(model::AbstractModel, ::MOI.ConstraintSet, ci::CI, change::MOI.AbstractSet)
    _modify(model, ci, getconstrloc(model, ci), change)
end

function MOI.get(model::AbstractModel,
                 ::MOI.NumberOfConstraints{MOI.SingleVariable, S}) where S
    flag = single_variable_flag(S)
    return count(mask -> !iszero(flag & mask), model.single_variable_mask)
end
MOI.get(model::AbstractModel, noc::MOI.NumberOfConstraints) = _getnoc(model, noc)

function _add_contraint_type(list, model::AbstractModel,
                             S::Type{<:MOI.AbstractScalarSet})
    flag = single_variable_flag(S)
    if any(mask -> !iszero(flag & mask), model.single_variable_mask)
        push!(list, (MOI.SingleVariable, S))
    end
    return
end
function MOI.get(model::AbstractModel{T}, loc::MOI.ListOfConstraints) where T
    list = broadcastvcat(_getloc, model)
    _add_contraint_type(list, model, MOI.EqualTo{T})
    _add_contraint_type(list, model, MOI.GreaterThan{T})
    _add_contraint_type(list, model, MOI.LessThan{T})
    _add_contraint_type(list, model, MOI.Interval{T})
    _add_contraint_type(list, model, MOI.Integer)
    _add_contraint_type(list, model, MOI.ZeroOne)
    return list
end

function MOI.get(model::AbstractModel,
                 ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}) where S
    list = CI{MOI.SingleVariable, S}[]
    flag = single_variable_flag(S)
    for (index, mask) in enumerate(model.single_variable_mask)
        if !iszero(mask & flag)
            push!(list, CI{MOI.SingleVariable, S}(index))
        end
    end
    return list
end
function MOI.get(model::AbstractModel, loc::MOI.ListOfConstraintIndices)
    broadcastvcat(constrs -> _getlocr(constrs, loc), model)
end

function MOI.get(model::AbstractModel, ::MOI.ConstraintFunction,
                 ci::CI{MOI.SingleVariable})
    MOI.throw_if_not_valid(model, ci)
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end
function MOI.get(model::AbstractModel, ::MOI.ConstraintFunction, ci::CI)
    _getfunction(model, ci, getconstrloc(model, ci))
end

function _get_single_variable_set(model::AbstractModel, S::Type{<:MOI.EqualTo},
                                  index)
    return MOI.EqualTo(model.lower_bound[index])
end
function _get_single_variable_set(model::AbstractModel,
                                  S::Type{<:Union{MOI.GreaterThan,
                                                  MOI.EqualTo}},
                                  index)
    # Lower and upper bounds are equal for `EqualTo`, we can take either of them.
    return S(model.lower_bound[index])
end
function _get_single_variable_set(model::AbstractModel, S::Type{<:MOI.LessThan},
                                  index)
    return S(model.upper_bound[index])
end
function _get_single_variable_set(model::AbstractModel,
                                  S::Type{<:Union{MOI.Interval,
                                                  MOI.Semicontinuous,
                                                  MOI.Semiinteger}},
                                  index)
    return S(model.lower_bound[index], model.upper_bound[index])
end
function _get_single_variable_set(model::AbstractModel,
                                  S::Type{<:Union{MOI.Integer, MOI.ZeroOne}},
                                  index)
    return S()
end
function MOI.get(model::AbstractModel, ::MOI.ConstraintSet,
                 ci::CI{MOI.SingleVariable, S}) where S
    MOI.throw_if_not_valid(model, ci)
    return _get_single_variable_set(model, S, ci.value)
end
function MOI.get(model::AbstractModel, ::MOI.ConstraintSet, ci::CI)
    _getset(model, ci, getconstrloc(model, ci))
end

function MOI.is_empty(model::AbstractModel)
    isempty(model.name) && !model.senseset && !model.objectiveset &&
    isempty(model.objective.terms) && iszero(model.objective.constant) &&
    iszero(model.num_variables_created) && iszero(model.nextconstraintid)
end

function MOI.copy_to(dest::AbstractModel, src::MOI.ModelLike; kws...)
    return automatic_copy_to(dest, src; kws...)
end
supports_default_copy_to(model::AbstractModel, copy_names::Bool) = true

# Allocate-Load Interface
# Even if the model does not need it and use default_copy_to, it could be used
# by a layer that needs it
supports_allocate_load(model::AbstractModel, copy_names::Bool) = true

function allocate_variables(model::AbstractModel, nvars)
    return MOI.add_variables(model, nvars)
end
allocate(model::AbstractModel, attr...) = MOI.set(model, attr...)
function allocate_constraint(model::AbstractModel, f::MOI.AbstractFunction,
                             s::MOI.AbstractSet)
    return MOI.add_constraint(model, f, s)
end

function load_variables(::AbstractModel, nvars) end
function load(::AbstractModel, attr...) end
function load_constraint(::AbstractModel, ::CI, ::MOI.AbstractFunction,
                         ::MOI.AbstractSet)
end

# Can be used to access constraints of a model
"""
broadcastcall(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model.

# Examples

To add all constraints of the model to a solver `solver`, one can do
```julia
_addcon(solver, ci, f, s) = MOI.add_constraint(solver, f, s)
function _addcon(solver, constrs::Vector)
    for constr in constrs
        _addcon(solver, constr...)
    end
end
MOIU.broadcastcall(constrs -> _addcon(solver, constrs), model)
```
"""
function broadcastcall end

"""
broadcastvcat(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model and concatenate the results with `vcat` (this is used internally for `ListOfConstraints`).

# Examples

To get the list of all functions:
```julia
_getfun(ci, f, s) = f
_getfun(cindices::Tuple) = _getfun(cindices...)
_getfuns(constrs::Vector) = _getfun.(constrs)
MOIU.broadcastvcat(_getfuns, model)
```
"""
function broadcastvcat end

# Macro to generate Model
abstract type Constraints{F} end

abstract type SymbolFS end
struct SymbolFun <: SymbolFS
    s::Union{Symbol, Expr}
    typed::Bool
    cname::Expr # `esc(scname)` or `esc(vcname)`
end
struct SymbolSet <: SymbolFS
    s::Union{Symbol, Expr}
    typed::Bool
end

# QuoteNode prevents s from being interpolated and keeps it as a symbol
# Expr(:., MOI, s) would be MOI.s
# Expr(:., MOI, $s) would be Expr(:., MOI, EqualTo)
# Expr(:., MOI, :($s)) would be Expr(:., MOI, :EqualTo)
# Expr(:., MOI, :($(QuoteNode(s)))) is Expr(:., MOI, :(:EqualTo)) <- what we want

# (MOI, :Zeros) -> :(MOI.Zeros)
# (:Zeros) -> :(MOI.Zeros)
_set(s::SymbolSet) = esc(s.s)
_fun(s::SymbolFun) = esc(s.s)
function _typedset(s::SymbolSet)
    if s.typed
        :($(_set(s)){T})
    else
        _set(s)
    end
end
function _typedfun(s::SymbolFun)
    if s.typed
        :($(_fun(s)){T})
    else
        _fun(s)
    end
end

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
using Unicode

_field(s::SymbolFS) = Symbol(replace(lowercase(string(s.s)), "." => "_"))

_getC(s::SymbolSet) = :(ConstraintEntry{F, $(_typedset(s))})
_getC(s::SymbolFun) = _typedfun(s)

_getCV(s::SymbolSet) = :($(_getC(s))[])
_getCV(s::SymbolFun) = :($(s.cname){T, $(_getC(s))}())

_callfield(f, s::SymbolFS) = :($f(model.$(_field(s))))
_broadcastfield(b, s::SymbolFS) = :($b(f, model.$(_field(s))))

# This macro is for expert/internal use only. Prefer the concrete Model type
# instantiated below.
"""
    macro model(model_name, scalar_sets, typed_scalar_sets, vector_sets, typed_vector_sets, scalar_functions, typed_scalar_functions, vector_functions, typed_vector_functions)

Creates a type `model_name` implementing the MOI model interface and containing
`scalar_sets` scalar sets `typed_scalar_sets` typed scalar sets, `vector_sets`
vector sets, `typed_vector_sets` typed vector sets, `scalar_functions` scalar
functions, `typed_scalar_functions` typed scalar functions, `vector_functions`
vector functions and `typed_vector_functions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

The function [`MathOptInterface.SingleVariable`](@ref) should not be given in
`scalar_functions`. The model supports [`MathOptInterface.SingleVariable`](@ref)-in-`F`
constraints where `F` is [`MathOptInterface.EqualTo`](@ref),
[`MathOptInterface.GreaterThan`](@ref), [`MathOptInterface.LessThan`](@ref),
[`MathOptInterface.Interval`](@ref), [`MathOptInterface.Integer`](@ref),
[`MathOptInterface.ZeroOne`](@ref), [`MathOptInterface.Semicontinuous`](@ref)
or [`MathOptInterface.Semiinteger`](@ref). The sets supported
with the [`MathOptInterface.SingleVariable`](@ref) cannot be controlled from the
macro, use the [`UniversalFallback`](@ref) to support more sets.

This macro creates a model specialized for specific types of constraint,
by defining specialized structures and methods. To create a model that,
in addition to be optimized for specific constraints, also support arbitrary
constraints and attributes, use [`UniversalFallback`](@ref).

This implementation of the MOI model certifies that the constraint indices, in
addition to being different between constraints `F`-in-`S` for the same types
`F` and `S`, are also different between constraints for different types `F` and
`S`. This means that for constraint indices `ci1`, `ci2` of this model,
`ci1 == ci2` if and only if `ci1.value == ci2.value`. This fact can be used to
use the the value of the index directly in a dictionary representing a mapping
between constraint indices and something else.

### Examples

The model describing an linear program would be:
```julia
@model(LPModel,                                                   # Name of model
      (),                                                         # untyped scalar sets
      (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval), #   typed scalar sets
      (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives),            # untyped vector sets
      (),                                                         #   typed vector sets
      (),                                                         # untyped scalar functions
      (MOI.ScalarAffineFunction,),                                #   typed scalar functions
      (MOI.VectorOfVariables,),                                   # untyped vector functions
      (MOI.VectorAffineFunction,))                                #   typed vector functions
```

Let `MOI` denote `MathOptInterface`, `MOIU` denote `MOI.Utilities` and
`MOIU.ConstraintEntry{F, S}` be defined as `MOI.Tuple{MOI.ConstraintIndex{F, S}, F, S}`.
The macro would create the types:
```julia
struct LPModelScalarConstraints{T, F <: MOI.AbstractScalarFunction} <: MOIU.Constraints{F}
    equalto::Vector{MOIU.ConstraintEntry{F, MOI.EqualTo{T}}}
    greaterthan::Vector{MOIU.ConstraintEntry{F, MOI.GreaterThan{T}}}
    lessthan::Vector{MOIU.ConstraintEntry{F, MOI.LessThan{T}}}
    interval::Vector{MOIU.ConstraintEntry{F, MOI.Interval{T}}}
end
struct LPModelVectorConstraints{T, F <: MOI.AbstractVectorFunction} <: MOIU.Constraints{F}
    zeros::Vector{MOIU.ConstraintEntry{F, MOI.Zeros}}
    nonnegatives::Vector{MOIU.ConstraintEntry{F, MOI.Nonnegatives}}
    nonpositives::Vector{MOIU.ConstraintEntry{F, MOI.Nonpositives}}
end
mutable struct LPModel{T} <: MOIU.AbstractModel{T}
    name::String
    sense::MOI.OptimizationSense
    objective::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}
    num_variables_created::Int64
    # If nothing, no variable has been deleted so the indices of the
    # variables are VI.(1:num_variables_created)
    variable_indices::Union{Nothing, Set{MOI.VariableIndex}}
    # Union of flags of `S` such that a `SingleVariable`-in-`S`
    # constraint was added to the model and not deleted yet.
    single_variable_mask::Vector{UInt8}
    # Lower bound set by `SingleVariable`-in-`S` where `S`is
    # `GreaterThan{T}`, `EqualTo{T}` or `Interval{T}`.
    lower_bound::Vector{T}
    # Lower bound set by `SingleVariable`-in-`S` where `S`is
    # `LessThan{T}`, `EqualTo{T}` or `Interval{T}`.
    upper_bound::Vector{T}
    var_to_name::Dict{MOI.VariableIndex, String}
    # If `nothing`, the dictionary hasn't been constructed yet.
    name_to_var::Union{Dict{String, MOI.VariableIndex}, Nothing}
    nextconstraintid::Int64
    con_to_name::Dict{MOI.ConstraintIndex, String}
    name_to_con::Union{Dict{String, MOI.ConstraintIndex}, Nothing}
    constrmap::Vector{Int}
    scalaraffinefunction::LPModelScalarConstraints{T, MOI.ScalarAffineFunction{T}}
    vectorofvariables::LPModelVectorConstraints{T, MOI.VectorOfVariables}
    vectoraffinefunction::LPModelVectorConstraints{T, MOI.VectorAffineFunction{T}}
end
```
The type `LPModel` implements the MathOptInterface API except methods specific
to solver models like `optimize!` or `getattribute` with `VariablePrimal`.
"""
macro model(model_name, ss, sst, vs, vst, sf, sft, vf, vft)
    scalar_sets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vector_sets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = esc(Symbol(string(model_name) * "ScalarConstraints"))
    vcname = esc(Symbol(string(model_name) * "VectorConstraints"))
    esc_model_name = esc(model_name)

    scalar_funs = [SymbolFun.(sf.args, false, Ref(scname));
                  SymbolFun.(sft.args, true, Ref(scname))]
    vector_funs = [SymbolFun.(vf.args, false, Ref(vcname));
                  SymbolFun.(vft.args, true, Ref(vcname))]
    funs = [scalar_funs; vector_funs]

    scalarconstraints = :(struct $scname{T, F<:$MOI.AbstractScalarFunction} <: Constraints{F}; end)
    vectorconstraints = :(struct $vcname{T, F<:$MOI.AbstractVectorFunction} <: Constraints{F}; end)
    for (c, sets) in ((scalarconstraints, scalar_sets), (vectorconstraints, vector_sets))
        for s in sets
            field = _field(s)
            push!(c.args[3].args, :($field::Vector{$(_getC(s))}))
        end
    end

    modeldef = quote
        mutable struct $esc_model_name{T} <: AbstractModel{T}
            name::String
            senseset::Bool
            sense::$MOI.OptimizationSense
            objectiveset::Bool
            objective::Union{$MOI.SingleVariable, $MOI.ScalarAffineFunction{T}, $MOI.ScalarQuadraticFunction{T}}
            num_variables_created::Int64
            # If nothing, no variable has been deleted so the indices of the
            # variables are VI.(1:num_variables_created)
            variable_indices::Union{Nothing, Set{$VI}}
            # Union of flags of `S` such that a `SingleVariable`-in-`S`
            # constraint was added to the model and not deleted yet.
            single_variable_mask::Vector{UInt8}
            # Lower bound set by `SingleVariable`-in-`S` where `S`is
            # `GreaterThan{T}`, `EqualTo{T}` or `Interval{T}`.
            lower_bound::Vector{T}
            # Lower bound set by `SingleVariable`-in-`S` where `S`is
            # `LessThan{T}`, `EqualTo{T}` or `Interval{T}`.
            upper_bound::Vector{T}
            var_to_name::Dict{$VI, String}
            # If `nothing`, the dictionary hasn't been constructed yet.
            name_to_var::Union{Dict{String, $VI}, Nothing}
            nextconstraintid::Int64
            con_to_name::Dict{$CI, String}
            name_to_con::Union{Dict{String, $CI}, Nothing}
            constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
        end
    end
    for f in funs
        cname = f.cname
        field = _field(f)
        push!(modeldef.args[2].args[3].args, :($field::$cname{T, $(_getC(f))}))
    end

    code = quote
        function $MOIU.broadcastcall(f::Function, model::$esc_model_name)
            $(Expr(:block, _broadcastfield.(Ref(:(broadcastcall)), funs)...))
        end
        function $MOIU.broadcastvcat(f::Function, model::$esc_model_name)
            vcat($(_broadcastfield.(Ref(:(broadcastvcat)), funs)...))
        end
        function $MOI.empty!(model::$esc_model_name{T}) where T
            model.name = ""
            model.senseset = false
            model.sense = $MOI.FEASIBILITY_SENSE
            model.objectiveset = false
            model.objective = $SAF{T}(MOI.ScalarAffineTerm{T}[], zero(T))
            model.num_variables_created = 0
            model.variable_indices = nothing
            model.single_variable_mask = UInt8[]
            model.lower_bound = T[]
            model.upper_bound = T[]
            empty!(model.var_to_name)
            model.name_to_var = nothing
            model.nextconstraintid = 0
            empty!(model.con_to_name)
            model.name_to_con = nothing
            empty!(model.constrmap)
            $(Expr(:block, _callfield.(Ref(:($MOI.empty!)), funs)...))
        end
    end
    for (cname, sets) in ((scname, scalar_sets), (vcname, vector_sets))
        code = quote
            $code
            function $MOIU.broadcastcall(f::Function, model::$cname)
                $(Expr(:block, _callfield.(:f, sets)...))
            end
            function $MOIU.broadcastvcat(f::Function, model::$cname)
                vcat($(_callfield.(:f, sets)...))
            end
            function $MOI.empty!(model::$cname)
                $(Expr(:block, _callfield.(Ref(:(Base.empty!)), sets)...))
            end
        end
    end

    for (funct, T) in ((:_add_constraint, CI), (:_modify, CI), (:_delete, CI), (:_getindex, CI), (:_getfunction, CI), (:_getset, CI), (:_getnoc, MOI.NumberOfConstraints))
        for (c, sets) in ((scname, scalar_sets), (vcname, vector_sets))
            for s in sets
                set = _set(s)
                field = _field(s)
                code = quote
                    $code
                    $MOIU.$funct(model::$c, ci::$T{F, <:$set}, args...) where F = $funct(model.$field, ci, args...)
                end
            end
        end

        for f in funs
            fun = _fun(f)
            field = _field(f)
            code = quote
                $code
                $MOIU.$funct(model::$esc_model_name, ci::$T{<:$fun}, args...) = $funct(model.$field, ci, args...)
            end
        end
    end

    code = quote
        $scalarconstraints
        function $scname{T, F}() where {T, F}
            $scname{T, F}($(_getCV.(scalar_sets)...))
        end

        $vectorconstraints
        function $vcname{T, F}() where {T, F}
            $vcname{T, F}($(_getCV.(vector_sets)...))
        end

        $modeldef
        function $esc_model_name{T}() where T
            $esc_model_name{T}("", false, $MOI.FEASIBILITY_SENSE, false,
                              $SAF{T}($MOI.ScalarAffineTerm{T}[], zero(T)), 0,
                              nothing, UInt8[], T[], T[], Dict{$VI, String}(),
                              nothing, 0, Dict{$CI, String}(), nothing, Int[],
                              $(_getCV.(funs)...))
        end

        $MOI.supports_constraint(model::$esc_model_name{T}, ::Type{<:Union{$(_typedfun.(scalar_funs)...)}}, ::Type{<:Union{$(_typedset.(scalar_sets)...)}}) where T = true
        $MOI.supports_constraint(model::$esc_model_name{T}, ::Type{<:Union{$(_typedfun.(vector_funs)...)}}, ::Type{<:Union{$(_typedset.(vector_sets)...)}}) where T = true

        $code
    end
    return code
end

const LessThanIndicatorSetOne{T} = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, MOI.LessThan{T}}
const LessThanIndicatorSetZero{T} = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, MOI.LessThan{T}}

@model(Model,
       (MOI.ZeroOne, MOI.Integer),
       (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
        MOI.Semicontinuous, MOI.Semiinteger),
       (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
        MOI.NormInfinityCone, MOI.NormOneCone,
        MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone, MOI.ExponentialCone, MOI.DualExponentialCone,
        MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
        MOI.RootDetConeTriangle, MOI.RootDetConeSquare, MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare),
       (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2,
        LessThanIndicatorSetOne, LessThanIndicatorSetZero),
       (),
       (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
       (MOI.VectorOfVariables,),
       (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

@doc raw"""

An implementation of `ModelLike` that supports all functions and sets defined
in MOI. It is parameterized by the coefficient type.

# Examples
```jl
model = Model{Float64}()
x = add_variable(model)
```
"""
Model
