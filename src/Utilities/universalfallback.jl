"""
    UniversalFallback

The `UniversalFallback` can be applied on a `ModelLike` `model` to create the model `UniversalFallback(model)` supporting *any* attributes.
This allows to have a specialized implementation in `model` for performance critical attributes while still supporting other attributes with a small performance penalty.
Note that `model` is unaware of attributes stored by `UniversalFallback` so this is not appropriate if `model` is an optimizer (for this reason, `optimize!` have not been implemented). In that case, optimizer bridges should be used instead.
"""
struct UniversalFallback{MT} <: MOI.ModelLike
    model::MT
    optattr::Dict{MOI.AbstractOptimizerAttribute, Any}
    modattr::Dict{MOI.AbstractModelAttribute, Any}
    varattr::Dict{MOI.AbstractVariableAttribute, Dict{VI, Any}}
    conattr::Dict{MOI.AbstractConstraintAttribute, Dict{CI, Any}}
    function UniversalFallback{MT}(model::MOI.ModelLike) where {MT}
        new{typeof(model)}(model,
                           Dict{MOI.AbstractOptimizerAttribute, Any}(),
                           Dict{MOI.AbstractModelAttribute, Any}(),
                           Dict{MOI.AbstractVariableAttribute, Dict{VI, Any}}(),
                           Dict{MOI.AbstractConstraintAttribute, Dict{CI, Any}}())
    end
end
UniversalFallback(model::MOI.ModelLike) = UniversalFallback{typeof(model)}(model)

MOI.isempty(uf::UniversalFallback) = MOI.isempty(uf.model) && isempty(uf.optattr) && isempty(uf.modattr) && isempty(uf.varattr) && isempty(uf.conattr)
function MOI.empty!(uf::UniversalFallback)
    MOI.empty!(uf.model)
    empty!(uf.optattr)
    empty!(uf.modattr)
    empty!(uf.varattr)
    empty!(uf.conattr)
end
MOI.supports(b::UniversalFallback, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = true
MOI.copy!(uf::UniversalFallback, src::MOI.ModelLike; copynames=true) = MOIU.defaultcopy!(uf, src, copynames)

# References
MOI.candelete(uf::UniversalFallback, idx::MOI.Index) = MOI.candelete(uf.model, idx)
MOI.isvalid(uf::UniversalFallback, idx::MOI.Index) = MOI.isvalid(uf.model, idx)
function MOI.delete!(uf::UniversalFallback, ci::CI)
    MOI.delete!(uf.model, ci)
    for d in values(uf.conattr)
        delete!(d, ci)
    end
end
function MOI.delete!(uf::UniversalFallback, vi::VI)
    MOI.delete!(uf.model, vi)
    for d in values(uf.varattr)
        delete!(d, vi)
    end
end

# Attributes
_canget(uf, attr::MOI.AbstractOptimizerAttribute)              = haskey(uf.optattr, attr)
_canget(uf, attr::MOI.AbstractModelAttribute)                  = haskey(uf.modattr, attr)
_canget(uf, attr::MOI.AbstractVariableAttribute, ::Type{VI})   = haskey(uf.varattr, attr) && length(uf.varattr[attr]) == MOI.get(uf.model, MOI.NumberOfVariables())
function _canget(uf, attr::MOI.AbstractConstraintAttribute, ::Type{CI{F, S}}) where {F, S}
    haskey(uf.conattr, attr) && length(uf.conattr[attr]) == MOI.get(uf.model, MOI.NumberOfConstraints{F, S}())
end
MOI.canget(uf::UniversalFallback, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.canget(uf.model, attr) || _canget(uf, attr)
MOI.canget(uf::UniversalFallback, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, IdxT::Type{<:MOI.Index}) = MOI.canget(uf.model, attr, IdxT) || _canget(uf, attr, IdxT)
_get(uf, attr::MOI.AbstractOptimizerAttribute)          = uf.optattr[attr]
_get(uf, attr::MOI.AbstractModelAttribute)              = uf.modattr[attr]
_get(uf, attr::MOI.AbstractVariableAttribute,   vi::VI) = uf.varattr[attr][vi]
_get(uf, attr::MOI.AbstractConstraintAttribute, ci::CI) = uf.conattr[attr][ci]
function MOI.get(uf::UniversalFallback, attr::Union{MOI.AbstractOptimizerAttribute, MOI.AbstractModelAttribute})
    if MOI.canget(uf.model, attr)
        MOI.get(uf.model, attr)
    else
        _get(uf, attr)
    end
end
function MOI.get(uf::UniversalFallback, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, idx::MOI.Index)
    if MOI.canget(uf.model, attr, typeof(idx))
        MOI.get(uf.model, attr, idx)
    else
        _get(uf, attr, idx)
    end
end

function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfOptimizerAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.optattr)
        if MOI.canget(uf, attr)
            push!(list, attr)
        end
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfModelAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.modattr)
        if MOI.canget(uf, attr)
            push!(list, attr)
        end
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfVariableAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.varattr)
        if MOI.canget(uf, attr, MOI.VariableIndex)
            push!(list, attr)
        end
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfConstraintAttributesSet{F, S}) where {F, S}
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.conattr)
        if MOI.canget(uf, attr, MOI.ConstraintIndex{F, S})
            push!(list, attr)
        end
    end
    list
end

# Name
MOI.canget(uf::UniversalFallback, IdxT::Type{<:MOI.Index}, name::String) = MOI.canget(uf.model, IdxT, name)
MOI.get(uf::UniversalFallback, IdxT::Type{<:MOI.Index}, name::String) = MOI.get(uf.model, IdxT, name)

_set!(uf, attr::MOI.AbstractOptimizerAttribute, value) = uf.optattr[attr] = value
_set!(uf, attr::MOI.AbstractModelAttribute, value)     = uf.modattr[attr] = value
function _set!(uf, attr::MOI.AbstractVariableAttribute, vi::VI, value)
    if !haskey(uf.varattr, attr)
        uf.varattr[attr] = Dict{VI, Any}()
    end
    uf.varattr[attr][vi] = value
end
function _set!(uf, attr::MOI.AbstractConstraintAttribute, ci::CI, value)
    if !haskey(uf.conattr, attr)
        uf.conattr[attr] = Dict{CI, Any}()
    end
    uf.conattr[attr][ci] = value
end
MOI.canset(::UniversalFallback, ::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = true
MOI.canset(::UniversalFallback, ::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, ::Type{<:MOI.Index}) = true
function MOI.set!(uf::UniversalFallback, attr::Union{MOI.AbstractOptimizerAttribute, MOI.AbstractModelAttribute}, value)
    if MOI.canset(uf.model, attr)
        MOI.set!(uf.model, attr, value)
    else
        _set!(uf, attr, value)
    end
end
function MOI.set!(uf::UniversalFallback, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, idx::MOI.Index, value)
    if MOI.canset(uf.model, attr, typeof(idx))
        MOI.set!(uf.model, attr, idx, value)
    else
        _set!(uf, attr, idx, value)
    end
end

# Constraints
MOI.supportsconstraint(uf::UniversalFallback, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = true
MOI.canaddconstraint(uf::UniversalFallback, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = MOI.canaddconstraint(uf.model, F, S)
MOI.addconstraint!(uf::UniversalFallback, f::MOI.AbstractFunction, s::MOI.AbstractSet) = MOI.addconstraint!(uf.model, f, s)
MOI.canmodifyconstraint(uf::UniversalFallback, ci::CI, change) = MOI.canmodifyconstraint(uf.model, ci, change)
MOI.modifyconstraint!(uf::UniversalFallback, ci::CI, change) = MOI.modifyconstraint!(uf.model, ci, change)

function MOI.canset(uf::UniversalFallback, ::MOI.ConstraintSet, ::Type{C}) where C <: CI
    MOI.canset(uf.model, MOI.ConstraintSet(), C)
end
function MOI.set!(uf::UniversalFallback, ::MOI.ConstraintSet, ci::CI{F,S}, set::S) where F where S
    MOI.set!(uf.model, MOI.ConstraintSet(), ci, set)
end

function MOI.canset(uf::UniversalFallback, ::MOI.ConstraintFunction, ::Type{C}) where C <: CI
    MOI.canset(uf.model, MOI.ConstraintFunction(), C)
end
function MOI.set!(uf::UniversalFallback, ::MOI.ConstraintFunction, ci::CI{F,S}, func::F) where F where S
    MOI.set!(uf.model, MOI.ConstraintFunction(), ci, func)
end

# Objective
MOI.canmodifyobjective(uf::UniversalFallback, ::Type{M}) where M<:MOI.AbstractFunctionModification = MOI.canmodifyobjective(uf.model, M)
MOI.modifyobjective!(uf::UniversalFallback, change::MOI.AbstractFunctionModification) = MOI.modifyobjective!(uf.model, change)

# Variables
MOI.canaddvariable(uf::UniversalFallback) = MOI.canaddvariable(uf.model)
MOI.addvariable!(uf::UniversalFallback) = MOI.addvariable!(uf.model)
MOI.addvariables!(uf::UniversalFallback, n) = MOI.addvariables!(uf.model, n)
