const VectorToScalarBridgeSets = Union{MOI.Zeros,
                                       MOI.Nonpositives,
                                       MOI.Nonnegatives}
get_scalar_set(T, ::Type{MOI.Zeros}) = MOI.EqualTo{T}
get_scalar_set(T, ::Type{MOI.Nonpositives}) = MOI.LessThan{T}
get_scalar_set(T, ::Type{MOI.Nonnegatives}) = MOI.GreaterThan{T}

get_vector_set(::Type{MOI.EqualTo{T}}) where T = MOI.Zeros
get_vector_set(::Type{MOI.LessThan{T}}) where T = MOI.Nonpositives
get_vector_set(::Type{MOI.GreaterThan{T}}) where T = MOI.Nonnegatives

"""
    VectorToScalarBridge{T, S}

The `VectorToScalarBridge` splits an `VectorAffineFunction` into a series of
`ScalarAffineFunction`s.

Supported sets are:
 - `MOI.Zeros` =>`MOI.EqualTo{T}`
 - `MOI.Nonnegatives` =>`MOI.GreaterThan{T}`
 - `MOI.Nonpositives` =>`MOI.LessThan{T}`
"""
struct VectorToScalarBridge{T, S} <: AbstractBridge
    constraints::Vector{CI{MOI.ScalarAffineFunction{T}, S}}
end
function VectorToScalarBridge{T, ScalarSet}(model,
                                            func::MOI.VectorAffineFunction{T},
                                            set::VectorSet) where
                                            {T, ScalarSet, VectorSet}
    @assert ScalarSet == get_scalar_set(T, VectorSet)
    sets = [ScalarSet(-constant) for constant in func.constants]
    @assert MOI.dimension(set) == length(sets)
    functions = [MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}[], zero(T))
        for i in 1:MOI.dimension(set)]
    for term in func.terms
        scalar_function = functions[term.output_index]
        push!(scalar_function.terms, term.scalar_term)
    end
    constraint_indices = MOI.addconstraints!(model, functions, sets)
    return VectorToScalarBridge(constraint_indices)
end

function MOI.supportsconstraint(::Type{VectorToScalarBridge{T, ScalarSet}},
                                ::Type{MOI.VectorAffineFunction{T}},
                                ::Type{VectorSet}) where {T, ScalarSet, VectorSet}
    return get_scalar_set(T, VectorSet) == ScalarSet
end

function addedconstrainttypes(::Type{VectorToScalarBridge{T, ScalarSet}},
                              ::Type{MOI.VectorAffineFunction{T}},
                              ::Type{VectorSet}) where {T, ScalarSet, VectorSet}
    return [(MOI.ScalarAffineFunction{T}, ScalarSet)]
end

function concrete_bridge_type(::Type{<:VectorToScalarBridge},
                              ::Type{<:MOI.VectorAffineFunction{T}},
                              ::Type{S}) where {T, S<:VectorToScalarBridgeSets}
    scalar_set = get_scalar_set(T, S)
    return VectorToScalarBridge{T, scalar_set}
end


function MOI.canget(bridge::VectorToScalarBridge{T, S},
       ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return true
end
function MOI.get(bridge::VectorToScalarBridge{T, S},
       ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return length(bridge.constraints)
end

function MOI.canget(bridge::VectorToScalarBridge{T, S},
       ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return true
end
function MOI.get(bridge::VectorToScalarBridge{T, S},
       ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}) where {T, S}
    return bridge.constraints
end

function MOI.delete!(model::MOI.ModelLike, bridge::VectorToScalarBridge)
    MOI.delete!.(Ref(model), bridge.constraints)
end

function MOI.canget(model::MOI.ModelLike, attribute::MOI.ConstraintDual,
                    ::Type{VectorToScalarBridge{T, S}}) where {T, S}
    return MOI.canget(model, attribute, CI{MOI.ScalarAffineFunction{T}, S})
end
function MOI.get(model::MOI.ModelLike, attribute::MOI.ConstraintDual,
                 bridge::VectorToScalarBridge)
    return MOI.get.(Ref(model), Ref(attribute), bridge.constraints)
end

function MOI.canget(model::MOI.ModelLike, attribute::MOI.ConstraintPrimal,
                    ::Type{VectorToScalarBridge{T, S}}) where {T, S}
    return MOI.canget(model, attribute, CI{MOI.ScalarAffineFunction{T}, S}) &&
        MOI.canget(model, MOI.ConstraintSet(), CI{MOI.ScalarAffineFunction{T}, S})
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, bridge::VectorToScalarBridge)
    primals = fill(0.0, length(bridge.constraints))
    for (i, index) in enumerate(bridge.constraints)
        primal = MOI.get(model, MOI.ConstraintPrimal(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        set_constant = MOIU.getconstant(set)
        primals[i] = primal - set_constant
    end
    return primals
end

function MOI.modify!(model::MOI.ModelLike, bridge::VectorToScalarBridge{T, S},
                     change::MOI.VectorConstantChange{Float64}) where {T, S}
    @assert length(bridge.constraints) == length(change.new_constant)
    for (index, coef) in zip(bridge.constraints, change.new_constant)
        MOI.set!(model, MOI.ConstraintSet(), index, S(-coef))
    end
end

function MOI.modify!(model::MOI.ModelLike, bridge::VectorToScalarBridge{T, S},
                     change::MOI.MultirowChange{T}) where {T, S}
    @assert length(bridge.constraints) == length(change.new_coefficients)
    variable = change.variable
    for (row, coef) in change.new_coefficients
        index = bridge.constraints[row]
        MOI.modify!(model, index, MOI.ScalarCoefficientChange(variable, coef))
    end
end

MOI.canget(::MOI.ModelLike, ::MOI.ConstraintSet, ::Type{<:VectorToScalarBridge}) = true
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet,
                 bridge::VectorToScalarBridge{T, S}) where {T, S}
    return get_vector_set(S)
end

MOI.canget(::MOI.ModelLike, ::MOI.ConstraintFunction, ::Type{<:VectorToScalarBridge}) = true
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                 bridge::VectorToScalarBridge{T, S}) where {T, S}
    terms = VectorAffineTerm{T}[]
    constants = T[]
    for (row, index) in enumerate(bridge.constraints)
        foo = MOI.get(model, MOI.ConstraintFunction(), index)
        for term in foo.terms
            push!(terms, MOI.VectorAffineTerm(row, term))
        end
        scalar_set = MOI.get(model, MOI.ConstraintSet())::S
        push!(constants, foo.constant + MOIU.getconstant(scalar_set))
    end
    return VectorAffineFunction(terms, constants)
end
