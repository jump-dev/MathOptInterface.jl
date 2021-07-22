"""
    ObjectiveFunctionContainer{T}

A helper struct to simplify the handling of objective functions in
Utilities.Model.
"""
mutable struct ObjectiveFunctionContainer{T} <: MOI.ModelLike
    is_sense_set::Bool
    sense::MOI.OptimizationSense
    is_function_set::Bool
    single_variable::Union{Nothing,MOI.SingleVariable}
    scalar_affine::Union{Nothing,MOI.ScalarAffineFunction{T}}
    scalar_quadratic::Union{Nothing,MOI.ScalarQuadraticFunction{T}}
    function ObjectiveFunctionContainer{T}() where {T}
        o = new{T}()
        MOI.empty!(o)
        return o
    end
end

function MOI.empty!(o::ObjectiveFunctionContainer{T}) where {T}
    o.is_sense_set = false
    o.sense = MOI.FEASIBILITY_SENSE
    o.is_function_set = false
    o.single_variable = nothing
    o.scalar_affine =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
    o.scalar_quadratic = nothing
    return
end

function MOI.is_empty(o::ObjectiveFunctionContainer)
    return !o.is_sense_set && !o.is_function_set
end

###
### ObjectiveSense
###

MOI.supports(::ObjectiveFunctionContainer, ::MOI.ObjectiveSense) = true

MOI.get(o::ObjectiveFunctionContainer, ::MOI.ObjectiveSense) = o.sense

function MOI.set(o::ObjectiveFunctionContainer, ::MOI.ObjectiveSense, value)
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
    o::ObjectiveFunctionContainer{T},
    ::MOI.ObjectiveFunctionType,
) where {T}
    if o.single_variable !== nothing
        return MOI.SingleVariable
    elseif o.scalar_quadratic !== nothing
        return MOI.ScalarQuadraticFunction{T}
    end
    @assert o.scalar_affine !== nothing
    return MOI.ScalarAffineFunction{T}
end

###
### ObjectiveFunction
###

function MOI.supports(
    ::ObjectiveFunctionContainer{T},
    ::MOI.ObjectiveFunction{
        <:Union{
            MOI.SingleVariable,
            MOI.ScalarAffineFunction{T},
            MOI.ScalarQuadraticFunction{T},
        },
    },
) where {T}
    return true
end

function MOI.get(
    o::ObjectiveFunctionContainer{T},
    ::MOI.ObjectiveFunction{F},
) where {T,F}
    if o.single_variable !== nothing
        return convert(F, o.single_variable)
    elseif o.scalar_quadratic !== nothing
        return convert(F, o.scalar_quadratic)
    end
    @assert o.scalar_affine !== nothing
    return convert(F, o.scalar_affine)
end

function MOI.set(
    o::ObjectiveFunctionContainer,
    ::MOI.ObjectiveFunction{MOI.SingleVariable},
    f::MOI.SingleVariable,
)
    o.is_function_set = true
    o.single_variable = copy(f)
    o.scalar_affine = nothing
    o.scalar_quadratic = nothing
    return
end

function MOI.set(
    o::ObjectiveFunctionContainer{T},
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    o.is_function_set = true
    o.single_variable = nothing
    o.scalar_affine = copy(f)
    o.scalar_quadratic = nothing
    return
end

function MOI.set(
    o::ObjectiveFunctionContainer{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
    f::MOI.ScalarQuadraticFunction{T},
) where {T}
    o.is_function_set = true
    o.single_variable = nothing
    o.scalar_affine = nothing
    o.scalar_quadratic = copy(f)
    return
end

###
### MOI.ListOfModelAttributesSet
###

function MOI.get(o::ObjectiveFunctionContainer, ::MOI.ListOfModelAttributesSet)
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
    o::ObjectiveFunctionContainer,
    ::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if o.single_variable !== nothing
        o.single_variable = modify_function(o.single_variable, change)
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = modify_function(o.scalar_quadratic, change)
    else
        @assert o.scalar_affine !== nothing
        o.is_function_set = true
        o.scalar_affine = modify_function(o.scalar_affine, change)
    end
    return
end

###
### MOI.delete
###

function MOI.delete(o::ObjectiveFunctionContainer, x::MOI.VariableIndex)
    if o.single_variable !== nothing
        sense = o.sense
        MOI.empty!(o)
        if o.is_sense_set
            MOI.set(o, MOI.ObjectiveSense(), sense)
        end
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = remove_variable(o.scalar_quadratic, x)
    else
        @assert o.scalar_affine !== nothing
        o.scalar_affine = remove_variable(o.scalar_affine, x)
    end
    return
end

function MOI.delete(o::ObjectiveFunctionContainer, x::Vector{MOI.VariableIndex})
    keep = v -> !(v in x)
    if o.single_variable !== nothing
        if o.single_variable.variable in x
            sense = o.sense
            MOI.empty!(o)
            if o.is_sense_set
                MOI.set(o, MOI.ObjectiveSense(), sense)
            end
        end
    elseif o.scalar_quadratic !== nothing
        o.scalar_quadratic = filter_variables(keep, o.scalar_quadratic)
    else
        @assert o.scalar_affine !== nothing
        o.scalar_affine = filter_variables(keep, o.scalar_affine)
    end
    return
end
