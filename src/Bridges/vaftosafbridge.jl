"""
    VectorAffineBridge{T}

The `VectorAffineBridge` splits a `VectorAffineFunction` into a series of
`ScalarAffineFunction`s.
"""
struct VectorAffineBridge{S,T} <: AbstractBridge
    constraints::Vector{CI{MOI.ScalarAffineFunction{T}, S}}
end
const VectorAffineBridgeSets = Union{MOI.Zeros, MOI.Nonpositives, MOI.Nonnegatives}
function VectorAffineBridge{T}(model, f::MOI.VectorAffineFunction{T}, s::VectorAffineBridgeSets) where T
    set_type = getscalarset(T, typeof(s))
    sets      = [ set_type(-constant) for constant in f.constants ]
    functions = [ MOI.ScalarAffineFunction{T}(MOI.ScalarAffineTerm{T}[], zero(T)) for term in f.terms ]
    @assert dimension(s) == length(sets) == length(functions)
    for term in f.terms
        saf = functions[term.output_index]
        push!(saf.terms, term.scalar_term)
    end
    ci = CI{MOI.ScalarAffineFunction{T}, S}[]
    for (func, set) in zip(functions, sets)
        push!(ci, MOI.addconstraint!(model, func, set))
    end
    VectorAffineBridge(ci)
end
getscalarset(T, ::Type{MOI.Zeros})        = MOI.EqualTo{T}
getscalarset(T, ::Type{MOI.Nonpositives}) = MOI.LessThan{T}
getscalarset(T, ::Type{MOI.Nonnegatives}) = MOI.GreaterThan{T}


MOI.supportsconstraint(::Type{VectorAffineBridge{T,S}}, ::Type{MOI.VectorAffineFunction{T}}, ::Type{S}) where {T,S<:VectorAffineBridgeSets} = true
function addedconstrainttypes(::Type{VectorAffineBridge{T,S}}, ::Type{MOI.VectorAffineFunction{T}}, ::Type{S}) where {T,S<:VectorAffineBridgeSets}
    [(MOI.ScalarAffineFunction{T}, getscalarset(T,S))]
 end

# Attributes, Bridge acting as an model
function MOI.get(b::VectorAffineBridge{T,S}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, S2}) where {T,S,S2}
    if getscalarset(T, S) == S2
        return length(b.constraints)
    else
        return 0
    end
end
function MOI.get(b::VectorAffineBridge{T,S}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S2}) where {T,S,S2}
    b.constraints
end

# Indices
function MOI.delete!(model::MOI.ModelLike, c::VectorAffineBridge)
    for ci in c.constraints
        MOI.delete!(model, ci)
    end
end

# Attributes, Bridge acting as a constraint
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, ::Type{VectorAffineBridge{T,S}}) where {T,S}
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, getscalarset(T, S)})
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::VectorAffineBridge)
    [ MOI.get(model, MOI.ConstraintPrimal(), ci) for ci in c.constraints ]
end
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, ::Type{VectorAffineBridge{T,S}}) where {T,S}
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, getscalarset(T, S))
end
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::VectorAffineBridge)
    [ MOI.get(model, MOI.ConstraintDual(), ci) for ci in c.constraints ]
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
