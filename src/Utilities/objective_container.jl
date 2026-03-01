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
    scalar_nonlinear::Union{Nothing,MOI.ScalarNonlinearFunction}
    vector_variables::Union{Nothing,MOI.VectorOfVariables}
    vector_affine::Union{Nothing,MOI.VectorAffineFunction{T}}
    vector_quadratic::Union{Nothing,MOI.VectorQuadraticFunction{T}}
    vector_nonlinear::Union{Nothing,MOI.VectorNonlinearFunction}
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
    o.scalar_nonlinear = nothing
    o.vector_variables = nothing
    o.vector_affine = nothing
    o.vector_quadratic = nothing
    o.vector_nonlinear = nothing
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
    elseif o.scalar_nonlinear !== nothing
        return MOI.ScalarNonlinearFunction
    elseif o.vector_variables !== nothing
        return MOI.VectorOfVariables
    elseif o.vector_affine !== nothing
        return MOI.VectorAffineFunction{T}
    elseif o.vector_quadratic !== nothing
        return MOI.VectorQuadraticFunction{T}
    elseif o.vector_nonlinear !== nothing
        return MOI.VectorNonlinearFunction
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
            MOI.ScalarNonlinearFunction,
            MOI.VectorOfVariables,
            MOI.VectorAffineFunction{T},
            MOI.VectorQuadraticFunction{T},
            MOI.VectorNonlinearFunction,
        },
    },
) where {T}
    return true
end

struct UnsafeObjectiveFunction{F<:MOI.AbstractFunction} <:
       MOI.AbstractModelAttribute end

function MOI.get(model::MOI.ModelLike, ::UnsafeObjectiveFunction{F}) where {F}
    return MOI.get(model, MOI.ObjectiveFunction{F}())
end

function MOI.get(
    o::ObjectiveContainer{T},
    attr::MOI.ObjectiveFunction{F},
) where {
    T,
    F<:Union{
        MOI.VariableIndex,
        MOI.ScalarAffineFunction,
        MOI.ScalarQuadraticFunction,
        MOI.ScalarNonlinearFunction,
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction,
        MOI.VectorQuadraticFunction,
        MOI.VectorNonlinearFunction,
    },
}
    if o.scalar_affine !== nothing
        return convert(F, something(o.scalar_affine))
    elseif o.single_variable !== nothing
        return convert(F, something(o.single_variable))
    elseif o.scalar_quadratic !== nothing
        return convert(F, something(o.scalar_quadratic))
    elseif o.scalar_nonlinear !== nothing
        return convert(F, something(o.scalar_nonlinear))
    elseif o.vector_variables !== nothing
        return convert(F, something(o.vector_variables))
    elseif o.vector_affine !== nothing
        return convert(F, something(o.vector_affine))
    elseif o.vector_quadratic !== nothing
        return convert(F, something(o.vector_quadratic))
    elseif o.vector_nonlinear !== nothing
        return convert(F, something(o.vector_nonlinear))
    end
    # The default if no objective is set.
    return convert(F, zero(MOI.ScalarAffineFunction{T}))
end

function MOI.get(o::ObjectiveContainer, ::MOI.ObjectiveFunction{F}) where {F}
    return copy(MOI.get(o, UnsafeObjectiveFunction{F}()))
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
    ::MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction},
    f::MOI.ScalarNonlinearFunction,
)
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.scalar_nonlinear = copy(f)
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

function MOI.set(
    o::ObjectiveContainer,
    ::MOI.ObjectiveFunction{MOI.VectorNonlinearFunction},
    f::MOI.VectorNonlinearFunction,
)
    _empty_keeping_sense(o)
    o.is_function_set = true
    o.vector_nonlinear = copy(f)
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
    o::ObjectiveContainer{T},
    ::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
) where {T}
    if o.single_variable !== nothing
        throw(
            MOI.ModifyObjectiveNotAllowed(
                change,
                "Cannot modify objective when there is a " *
                "`VariableIndex` objective.",
            ),
        )
    elseif o.scalar_affine !== nothing
        o.scalar_affine = modify_function!(something(o.scalar_affine), change)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic =
            modify_function!(something(o.scalar_quadratic), change)
    elseif o.scalar_nonlinear !== nothing
        throw(
            MOI.ModifyObjectiveNotAllowed(
                change,
                "Cannot modify objective when there is a " *
                "`ScalarNonlinearFunction` objective.",
            ),
        )
    elseif o.vector_variables !== nothing
        throw(
            MOI.ModifyObjectiveNotAllowed(
                change,
                "Cannot modify objective when there is a " *
                "`VectorOfVariables` objective.",
            ),
        )
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic =
            modify_function!(something(o.vector_quadratic), change)
    elseif o.vector_affine !== nothing
        o.vector_affine = modify_function!(something(o.vector_affine), change)
    elseif o.vector_nonlinear !== nothing
        throw(
            MOI.ModifyObjectiveNotAllowed(
                change,
                "Cannot modify objective when there is a " *
                "`VectorNonlinearFunction` objective.",
            ),
        )
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
        if x == something(o.single_variable)
            _empty_keeping_sense(o)
        end
    elseif o.scalar_affine !== nothing
        o.scalar_affine = remove_variable(something(o.scalar_affine), x)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = remove_variable(something(o.scalar_quadratic), x)
    elseif o.scalar_nonlinear !== nothing
        throw(
            MOI.DeleteNotAllowed(
                x,
                "Cannot delete variable when there is a " *
                "`ScalarNonlinearFunction` objective",
            ),
        )
    elseif o.vector_variables !== nothing
        o.vector_variables = remove_variable(something(o.vector_variables), x)
        if isempty(something(o.vector_variables).variables)
            _empty_keeping_sense(o)
        end
    elseif o.vector_affine !== nothing
        o.vector_affine = remove_variable(something(o.vector_affine), x)
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic = remove_variable(something(o.vector_quadratic), x)
    elseif o.vector_nonlinear !== nothing
        throw(
            MOI.DeleteNotAllowed(
                x,
                "Cannot delete variable when there is a " *
                "`VectorNonlinearFunction` objective",
            ),
        )
    end
    return
end

function MOI.delete(o::ObjectiveContainer, x::Vector{MOI.VariableIndex})
    keep = v -> !(v in x)
    if o.single_variable !== nothing
        if something(o.single_variable) in x
            _empty_keeping_sense(o)
        end
    elseif o.scalar_affine !== nothing
        o.scalar_affine = filter_variables(keep, something(o.scalar_affine))
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic =
            filter_variables(keep, something(o.scalar_quadratic))
    elseif o.scalar_nonlinear !== nothing
        throw(
            MOI.DeleteNotAllowed(
                first(x),
                "Cannot delete variable when there is a " *
                "`ScalarNonlinearFunction` objective",
            ),
        )
    elseif o.vector_variables !== nothing
        o.vector_variables =
            filter_variables(keep, something(o.vector_variables))
        if isempty(something(o.vector_variables).variables)
            _empty_keeping_sense(o)
        end
    elseif o.vector_affine !== nothing
        o.vector_affine = filter_variables(keep, something(o.vector_affine))
    elseif o.vector_quadratic !== nothing
        o.vector_quadratic =
            filter_variables(keep, something(o.vector_quadratic))
    elseif o.vector_nonlinear !== nothing
        throw(
            MOI.DeleteNotAllowed(
                first(x),
                "Cannot delete variable when there is a " *
                "`VectorNonlinearFunction` objective",
            ),
        )
    end
    return
end
