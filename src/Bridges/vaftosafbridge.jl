const VectorToScalarBridgeSets = Union{MOI.Zeros,
                                       MOI.Nonpositives,
                                       MOI.Nonnegatives}
get_scalar_set(T, ::Type{MOI.Zeros}) = MOI.EqualTo{T}
get_scalar_set(T, ::Type{MOI.Nonpositives}) = MOI.LessThan{T}
get_scalar_set(T, ::Type{MOI.Nonnegatives}) = MOI.GreaterThan{T}

"""
    VectorToScalarBridge{S, T}

The `VectorToScalarBridge` splits an `VectorAffineFunction` into a series of
`ScalarAffineFunction`s.
"""
struct VectorToScalarBridge{T, S<:VectorToScalarBridgeSets} <: AbstractBridge
    constraints::Vector{CI{MOI.ScalarAffineFunction{T}, S}}
end
function VectorToScalarBridge(model, func::MOI.VectorAffineFunction{T}, set::S) where {T, S}
    set_type = get_scalar_set(T, S)
    sets = [set_type(-constant) for constant in func.constants]
    functions = [MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}[], zero(T))
        for term in func.terms]
    @assert dimension(set) == length(sets) == length(functions)
    for term in func.terms
        scalar_function = functions[term.output_index]
        push!(scalar_function.terms, term.scalar_term)
    end
    constraint_indices = MOI.addconstraints!(model, functions, sets)
    VectorToScalarBridge(constraint_indices)
end

function MOI.supportsconstraint(::Type{VectorToScalarBridge{T, S}},
                                ::Type{MOI.VectorAffineFunction{T}},
                                ::Type{S}) where {T, S<:VectorToScalarBridgeSets}
    return true
end

function addedconstrainttypes(::Type{VectorToScalarBridge{T, S}},
                              ::Type{<:MOI.VectorAffineFunction{T}},
                              ::Type{S}) where {T, S<:VectorToScalarBridgeSets}
    return [(MOI.ScalarAffineFunction{T}, get_scalar_set(T, S))]
end

function concrete_bridge_type(::Type{<:VectorToScalarBridge},
                              ::Type{<:MOI.VectorAffineFunction{T}},
                              ::Type{S}) where {T, S<:VectorToScalarBridgeSets}
    return VectorToScalarBridge{T, S}
end

# Attributes, Bridge acting as an model
function MOI.get(bridge::VectorToScalarBridge{T, S},
       ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return length(bridge.constraints)
end
function MOI.get(bridge::VectorToScalarBridge{T, S},
       ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return bridge.constraints
end

# Indices
function MOI.delete!(model::MOI.ModelLike, bridge::VectorToScalarBridge)
    MOI.delete!.(Ref(model), bridge.constraints)
end

# Attributes, Bridge acting as a constraint

function MOI.canget(model::MOI.ModelLike, attribute::Union{MOI.ConstraintPrimal,
                                                           MOI.ConstraintDual},
                    ::Type{VectorToScalarBridge{T, S}}) where {T, S}
    return MOI.canget(model, attribute,
                      CI{MOI.ScalarAffineFunction{T}, get_scalar_set(T, S)})
end
function MOI.get(model::MOI.ModelLike, attribute::Union{MOI.ConstraintPrimal,
                                                        MOI.ConstraintDual},
                 bridge::VectorToScalarBridge)
    return [MOI.get(model, attribute, ci) for ci in bridge.constraints]
end

# Constraints
# MOI.canmodify(model::MOI.ModelLike, ::Type{<:VectorAffineBridge}, ::Type{<:MOI.AbstractFunctionModification}) = true
# function MOI.modify!(model::MOI.ModelLike, c::VectorAffineBridge, change::MOI.AbstractFunctionModification)
    # MOI.modify!(model, c.lower, change)
    # MOI.modify!(model, c.upper, change)
# end
#
# MOI.canset(model::MOI.ModelLike, ::MOI.ConstraintFunction, ::Type{<:VectorAffineBridge}) = true
# function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::VectorAffineBridge, func::MOI.ScalarAffineFunction)
#     MOI.set!(model, MOI.ConstraintFunction(), c.lower, func)
#     MOI.set!(model, MOI.ConstraintFunction(), c.upper, func)
# end
#
# MOI.canset(model::MOI.ModelLike, ::MOI.ConstraintSet, ::Type{<:VectorAffineBridge}) = true
# function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintSet, c::VectorAffineBridge, change::MOI.Interval)
#     MOI.set!(model, MOI.ConstraintSet(), c.lower, MOI.GreaterThan(change.lower))
#     MOI.set!(model, MOI.ConstraintSet(), c.upper, MOI.LessThan(change.upper))
# end
