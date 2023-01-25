# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

mutable struct _ScalarObjective{T}
    single_variable::Union{Nothing,MOI.VariableIndex}
    scalar_affine::Union{Nothing,MOI.ScalarAffineFunction{T}}
    scalar_quadratic::Union{Nothing,MOI.ScalarQuadraticFunction{T}}
end

function _ScalarObjective(f::MOI.VariableIndex)
    return _ScalarObjective{Float64}(copy(f), nothing, nothing)
end

function _ScalarObjective(f::MOI.ScalarAffineFunction{T}) where {T}
    return _ScalarObjective{T}(nothing, copy(f), nothing)
end

function _ScalarObjective(f::MOI.ScalarQuadraticFunction{T}) where {T}
    return _ScalarObjective{T}(nothing, nothing, copy(f))
end

"""
    ObjectiveContainer{T}

A helper struct to simplify the handling of objective functions in
Utilities.Model.
"""
mutable struct ObjectiveContainer{T} <: MOI.ModelLike
    is_sense_set::Bool
    sense::MOI.OptimizationSense
    objectives::Dict{Int,_ScalarObjective{T}}
    function ObjectiveContainer{T}() where {T}
        return new{T}(
            false,
            MOI.FEASIBILITY_SENSE,
            Dict{Int,_ScalarObjective{T}}(),
        )
    end
end

function MOI.empty!(o::ObjectiveContainer{T}) where {T}
    o.is_sense_set = false
    o.sense = MOI.FEASIBILITY_SENSE
    empty!(o.objectives)
    return
end

function MOI.is_empty(o::ObjectiveContainer)
    return !o.is_sense_set && isempty(o.objectives)
end

###
### ObjectiveSense
###

MOI.supports(::ObjectiveContainer, ::MOI.ObjectiveSense) = true

MOI.get(o::ObjectiveContainer, ::MOI.ObjectiveSense) = o.sense

function MOI.set(o::ObjectiveContainer, ::MOI.ObjectiveSense, value)
    if value == MOI.FEASIBILITY_SENSE
        MOI.empty!(o)
    end
    o.is_sense_set = true
    o.sense = value
    return
end

###
### ObjectiveFunctionType
###

function MOI.get(
    o::_ScalarObjective{T},
    ::MOI.ObjectiveFunctionType,
) where {T}
    if o.scalar_affine !== nothing
        return MOI.ScalarAffineFunction{T}
    elseif o.single_variable !== nothing
        return MOI.VariableIndex
    else
        @assert o.scalar_quadratic !== nothing
        return MOI.ScalarQuadraticFunction{T}
    end
end

function MOI.get(
    o::ObjectiveContainer{T},
    attr::MOI.ObjectiveFunctionType,
) where {T}
    objective = get(o.objectives, attr.objective_index, nothing)
    if objective === nothing
        # Default if not set
        return MOI.ScalarAffineFunction{T}
    end
    return MOI.get(objective, attr)
end

###
### ObjectiveFunction
###

function MOI.supports(
    ::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{
        <:Union{
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{T},
            MOI.ScalarQuadraticFunction{T},
        },
    },
) where {T}
    return true
end

function MOI.get(
    o::_ScalarObjective{T},
    attr::MOI.ObjectiveFunction{F},
) where {T,F}
    if o.single_variable !== nothing
        return convert(F, o.single_variable)
    elseif o.scalar_quadratic !== nothing
        return convert(F, o.scalar_quadratic)
    end
    @assert o.scalar_affine !== nothing
    return convert(F, o.scalar_affine)
end

function MOI.get(
    o::ObjectiveContainer{T},
    attr::MOI.ObjectiveFunction{F},
) where {T,F}
    objective = get(o.objectives, attr.objective_index, nothing)
    if objective === nothing
        return zero(MOI.ScalarAffineFunction{T})
    end
    return MOI.get(objective, attr)
end

function MOI.set(
    o::ObjectiveContainer,
    attr::MOI.ObjectiveFunction{F},
    f::F,
) where {F}
    o.objectives[attr.objective_index] = _ScalarObjective(f)
    return
end

function MOI.set(
    o::ObjectiveContainer,
    attr::MOI.ObjectiveFunction,
    ::Nothing,
)
    delete!(o.objectives, attr.objective_index)
    return
end

###
### MOI.ListOfModelAttributesSet
###

function MOI.get(o::ObjectiveContainer, ::MOI.ListOfModelAttributesSet)
    ret = MOI.AbstractModelAttribute[]
    if o.is_sense_set
        push!(ret, MOI.ObjectiveSense())
    end
    for objective_index in keys(o.objectives)
        F = MOI.get(o, MOI.ObjectiveFunctionType(objective_index))
        push!(ret, MOI.ObjectiveFunction{F}(objective_index))
    end
    return ret
end

###
### MOI.modify
###

function MOI.modify(
    o::_ScalarObjective,
    ::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if o.single_variable !== nothing
        o.single_variable = modify_function!(o.single_variable, change)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = modify_function!(o.scalar_quadratic, change)
    else
        @assert o.scalar_affine !== nothing
        o.scalar_affine = modify_function!(o.scalar_affine, change)
    end
    return
end

function MOI.modify(
    o::ObjectiveContainer,
    attr::MOI.ObjectiveFunction{F},
    change::MOI.AbstractFunctionModification,
) where {F<:MOI.AbstractScalarFunction}
    objective = get(o.objectives, attr.objective_index, nothing)
    if objective === nothing
        # If not set, set a default objective.
        MOI.set(o, attr, zero(F))
    end
    MOI.modify(objective, attr, change)
    return
end

###
### MOI.delete
###

function MOI.delete(o::_ScalarObjective, x::MOI.VariableIndex)
    if o.single_variable !== nothing
        # Handled in the ObjectiveContainer method
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = remove_variable(o.scalar_quadratic, x)
    else
        @assert o.scalar_affine !== nothing
        o.scalar_affine = remove_variable(o.scalar_affine, x)
    end
    return
end

function MOI.delete(o::ObjectiveContainer, x::MOI.VariableIndex)
    to_delete = Int[]
    for (objective_index, objective) in o.objectives
        if objective.single_variable === x
            push!(to_delete, objective_index)
        else
            MOI.delete(objective, x)
        end
    end
    for objective_index in to_delete
        delete!(o.objectives, objective_index)
    end
    return
end

function MOI.delete(o::_ScalarObjective, x::Vector{MOI.VariableIndex})
    keep = v -> !(v in x)
    if o.single_variable !== nothing
        # Handled in the ObjectiveContainer method
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = filter_variables(keep, o.scalar_quadratic)
    else
        @assert o.scalar_affine !== nothing
        o.scalar_affine = filter_variables(keep, o.scalar_affine)
    end
    return
end

function MOI.delete(o::ObjectiveContainer, x::Vector{MOI.VariableIndex})
    to_delete = Int[]
    for (objective_index, objective) in o.objectives
        if objective.single_variable in x
            push!(to_delete, objective_index)
        else
            MOI.delete(objective, x)
        end
    end
    for objective_index in to_delete
        delete!(o.objectives, objective_index)
    end
    return
end
