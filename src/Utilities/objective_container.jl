# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ObjectiveContainer{T}

A helper struct to simplify the handling of objective functions in
Utilities.Model.
"""
mutable struct ObjectiveContainer{T} <: MOI.ModelLike
    is_sense_set::Bool
    sense::MOI.OptimizationSense
    is_function_set::Bool
    single_variable::Union{Nothing,MOI.VariableIndex}
    scalar_affine::Union{Nothing,MOI.ScalarAffineFunction{T}}
    scalar_quadratic::Union{Nothing,MOI.ScalarQuadraticFunction{T}}
    vector_variables::Union{Nothing,MOI.VectorOfVariables}
    vector_affine::Union{Nothing,MOI.VectorAffineFunction{T}}
    vector_quadratic::Union{Nothing,MOI.VectorQuadraticFunction{T}}
    function ObjectiveContainer{T}() where {T}
        o = new{T}()
        MOI.empty!(o)
        return o
    end
end

function MOI.empty!(o::ObjectiveContainer{T}) where {T}
    o.is_sense_set = false
    o.sense = MOI.FEASIBILITY_SENSE
    o.is_function_set = false
    o.single_variable = nothing
    o.scalar_affine = nothing
    o.scalar_quadratic = nothing
    o.vector_variables = nothing
    o.vector_affine = nothing
    o.vector_quadratic = nothing
    return
end

function MOI.is_empty(o::ObjectiveContainer)
    return !o.is_sense_set && !o.is_function_set
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
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunctionType,
) where {T}
    if o.scalar_affine !== nothing
        return MOI.ScalarAffineFunction{T}
    elseif o.single_variable !== nothing
        return MOI.VariableIndex
    elseif o.scalar_quadratic !== nothing
        return MOI.ScalarQuadraticFunction{T}
    elseif o.vector_variables !== nothing
        return MOI.VectorOfVariables
    elseif o.vector_affine !== nothing
        return MOI.VectorAffineFunction{T}
    elseif o.vector_quadratic !== nothing
        return MOI.VectorQuadraticFunction{T}
    end
    # The default if no objective is set.
    return MOI.ScalarAffineFunction{T}
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
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
            MOI.VectorQuadraticFunction{T},
        },
    },
) where {T}
    return true
end

function MOI.get(
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{F},
) where {T,F}
    if o.scalar_affine !== nothing
        return convert(F, o.scalar_affine)
    elseif o.single_variable !== nothing
        return convert(F, o.single_variable)
    elseif o.scalar_quadratic !== nothing
        return convert(F, o.scalar_quadratic)
    elseif o.vector_variables !== nothing
        return convert(F, o.vector_variables)
    elseif o.vector_affine !== nothing
        return convert(F, o.vector_affine)
    elseif o.vector_quadratic !== nothing
        return convert(F, o.vector_quadratic)
    end
    # The default if no objective is set.
    return convert(F, zero(MOI.ScalarAffineFunction{T}))
end

function _empty_keeping_sense(o::ObjectiveContainer)
    sense, is_sense_set = o.sense, o.is_sense_set
    MOI.empty!(o)
    o.sense, o.is_sense_set = sense, is_sense_set
    return
end

function MOI.set(
    o::ObjectiveContainer,
    ::MOI.ObjectiveFunction{MOI.VariableIndex},
    f::MOI.VariableIndex,
)
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.single_variable = copy(f)
    return
end

function MOI.set(
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.scalar_affine = copy(f)
    return
end

function MOI.set(
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
    f::MOI.ScalarQuadraticFunction{T},
) where {T}
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.scalar_quadratic = copy(f)
    return
end

function MOI.set(
    o::ObjectiveContainer,
    ::MOI.ObjectiveFunction{MOI.VectorOfVariables},
    f::MOI.VectorOfVariables,
)
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.vector_variables = copy(f)
    return
end

function MOI.set(
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{MOI.VectorAffineFunction{T}},
    f::MOI.VectorAffineFunction{T},
) where {T}
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.vector_affine = copy(f)
    return
end

function MOI.set(
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction{MOI.VectorQuadraticFunction{T}},
    f::MOI.VectorQuadraticFunction{T},
) where {T}
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.vector_quadratic = copy(f)
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
    if o.is_function_set
        F = MOI.get(o, MOI.ObjectiveFunctionType())
        push!(ret, MOI.ObjectiveFunction{F}())
    end
    return ret
end

###
### MOI.modify
###

function MOI.modify(
    o::ObjectiveContainer,
    ::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if o.single_variable !== nothing
        o.single_variable = modify_function!(o.single_variable, change)
    elseif o.scalar_affine !== nothing
        o.scalar_affine = modify_function!(o.scalar_affine, change)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = modify_function!(o.scalar_quadratic, change)
    elseif o.vector_variables !== nothing
        o.vector_variables = modify_function!(o.vector_variables, change)
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic = modify_function!(o.vector_quadratic, change)
    elseif o.vector_affine !== nothing
        o.vector_affine = modify_function!(o.vector_affine, change)
    else
        # If no objective is set, modify a ScalarAffineFunction by default.
        f = zero(MOI.ScalarAffineFunction{T})
        o.scalar_affine = modify_function!(f, change)
        o.is_function_set = true
    end
    return
end

###
### MOI.delete
###

function MOI.delete(o::ObjectiveContainer, x::MOI.VariableIndex)
    if o.single_variable !== nothing
        if x == o.single_variable
            _empty_keeping_sense(o)
        end
    elseif o.scalar_affine !== nothing
        o.scalar_affine = remove_variable(o.scalar_affine, x)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = remove_variable(o.scalar_quadratic, x)
    elseif o.vector_variables !== nothing
        o.vector_variables = remove_variable(o.vector_variables, x)
        if isempty(o.vector_variables.variables)
            _empty_keeping_sense(o)
        end
    elseif o.vector_affine !== nothing
        o.vector_affine = remove_variable(o.vector_affine, x)
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic = remove_variable(o.vector_quadratic, x)
    end
    return
end

function MOI.delete(o::ObjectiveContainer, x::Vector{MOI.VariableIndex})
    keep = v -> !(v in x)
    if o.single_variable !== nothing
        if o.single_variable in x
            _empty_keeping_sense(o)
        end
    elseif o.scalar_affine !== nothing
        o.scalar_affine = filter_variables(keep, o.scalar_affine)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = filter_variables(keep, o.scalar_quadratic)
    elseif o.vector_variables !== nothing
        o.vector_variables = filter_variables(keep, o.vector_variables)
        if isempty(o.vector_variables.variables)
            _empty_keeping_sense(o)
        end
    elseif o.vector_affine !== nothing
        o.vector_affine = filter_variables(keep, o.vector_affine)
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic = filter_variables(keep, o.vector_quadratic)
    end
    return
end
