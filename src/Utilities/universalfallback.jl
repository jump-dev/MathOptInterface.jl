"""
    UniversalFallback

The `UniversalFallback` can be applied on a `ModelLike` `model` to create the
model `UniversalFallback(model)` supporting *any* constaints and attributes.
This allows to have a specialized implementation in `model` for performance
critical constraints and attributes while still supporting other attributes
with a small performance penalty. Note that `model` is unaware of constraints
and attributes stored by `UniversalFallback` so this is not appropriate if
`model` is an optimizer (for this reason, `optimize!` has not been
implemented). In that case, optimizer bridges should be used instead.
"""
mutable struct UniversalFallback{MT} <: MOI.ModelLike
    model::MT
    constraints::Dict{Tuple{DataType, DataType}, Dict} # See https://github.com/JuliaOpt/JuMP.jl/issues/1152
    nextconstraintid::Int64
    connames::Dict{CI, String}
    namescon::Dict{String, CI}
    optattr::Dict{MOI.AbstractOptimizerAttribute, Any}
    modattr::Dict{MOI.AbstractModelAttribute, Any}
    varattr::Dict{MOI.AbstractVariableAttribute, Dict{VI, Any}}
    conattr::Dict{MOI.AbstractConstraintAttribute, Dict{CI, Any}}
    function UniversalFallback{MT}(model::MOI.ModelLike) where {MT}
        new{typeof(model)}(model,
                           Dict{Tuple{DataType, DataType}, Dict}(),
                           0,
                           Dict{CI, String}(),
                           Dict{String, CI}(),
                           Dict{MOI.AbstractOptimizerAttribute, Any}(),
                           Dict{MOI.AbstractModelAttribute, Any}(),
                           Dict{MOI.AbstractVariableAttribute, Dict{VI, Any}}(),
                           Dict{MOI.AbstractConstraintAttribute, Dict{CI, Any}}())
    end
end
UniversalFallback(model::MOI.ModelLike) = UniversalFallback{typeof(model)}(model)

MOI.isempty(uf::UniversalFallback) = MOI.isempty(uf.model) && isempty(uf.constraints) && isempty(uf.optattr) && isempty(uf.modattr) && isempty(uf.varattr) && isempty(uf.conattr)
function MOI.empty!(uf::UniversalFallback)
    MOI.empty!(uf.model)
    empty!(uf.constraints)
    uf.nextconstraintid = 0
    empty!(uf.connames)
    empty!(uf.namescon)
    empty!(uf.optattr)
    empty!(uf.modattr)
    empty!(uf.varattr)
    empty!(uf.conattr)
end
MOI.copy!(uf::UniversalFallback, src::MOI.ModelLike; copynames=true) = MOIU.defaultcopy!(uf, src, copynames)

# References
MOI.isvalid(uf::UniversalFallback, idx::VI) = MOI.isvalid(uf.model, idx)
function MOI.isvalid(uf::UniversalFallback, idx::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.isvalid(uf.model, idx)
    else
        haskey(uf.constraints, (F, S)) && haskey(uf.constraints[(F, S)], idx)
    end
end
function MOI.delete!(uf::UniversalFallback, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.delete!(uf.model, ci)
    else
        if !MOI.isvalid(uf, ci)
            throw(MOI.InvalidIndex(ci))
        end
        MOI.delete!(uf.constraints[(F, S)], ci)
        if haskey(uf.connames, ci)
            delete!(uf.namescon, uf.connames[ci])
            delete!(uf.connames, ci)
        end
    end
    for d in values(uf.conattr)
        delete!(d, ci)
    end
end
function MOI.delete!(uf::UniversalFallback, vi::VI)
    MOI.delete!(uf.model, vi)
    for d in values(uf.varattr)
        delete!(d, vi)
    end
    for (FS, constraints) in uf.constraints
        for (ci, constraint) in constraints
            f, s = constraint
            if f isa MOI.SingleVariable
                if f.variable == vi
                    delete!(constraints, ci)
                end
            else
                constraints[ci] = removevariable(f, s, vi)
            end
        end
    end
end

# Attributes
_get(uf, attr::MOI.AbstractOptimizerAttribute)          = uf.optattr[attr]
_get(uf, attr::MOI.AbstractModelAttribute)              = uf.modattr[attr]
_get(uf, attr::MOI.AbstractVariableAttribute,   vi::VI) = uf.varattr[attr][vi]
_get(uf, attr::MOI.AbstractConstraintAttribute, ci::CI) = uf.conattr[attr][ci]
function MOI.get(uf::UniversalFallback,
                 attr::Union{MOI.AbstractOptimizerAttribute,
                             MOI.AbstractModelAttribute})
    if MOI.supports(uf.model, attr)
        MOI.get(uf.model, attr)
    else
        _get(uf, attr)
    end
end
function MOI.get(uf::UniversalFallback,
                 attr::Union{MOI.AbstractVariableAttribute,
                             MOI.AbstractConstraintAttribute}, idx::MOI.Index)
    if MOI.supports(uf.model, attr, typeof(idx))
        MOI.get(uf.model, attr, idx)
    else
        _get(uf, attr, idx)
    end
end

function MOI.get(uf::UniversalFallback,
                 attr::Union{MOI.NumberOfVariables,
                             MOI.ListOfVariableIndices})
    return MOI.get(uf.model, attr)
end
function MOI.get(uf::UniversalFallback,
                 attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        return MOI.get(uf.model, attr)
    else
        return length(get(uf.constraints, (F, S), Dict{CI{F, S}, Tuple{F, S}}()))
    end
end
function MOI.get(uf::UniversalFallback,
                 listattr::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.get(uf.model, listattr)
    else
        collect(keys(get(uf.constraints, (F, S), Dict{CI{F, S}, Tuple{F, S}}())))
    end
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfConstraints)
    list = MOI.get(uf.model, listattr)
    for (FS, constraints) in uf.constraints
        if !isempty(constraints)
            push!(list, FS)
        end
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfOptimizerAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.optattr)
        push!(list, attr)
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfModelAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.modattr)
        push!(list, attr)
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfVariableAttributesSet)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.varattr)
        push!(list, attr)
    end
    list
end
function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfConstraintAttributesSet{F, S}) where {F, S}
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.conattr)
        push!(list, attr)
    end
    return list
end

# Name
# The names of constraints not supported by `uf.model` need to be handled
function MOI.set!(uf::UniversalFallback, attr::MOI.ConstraintName, ci::CI{F, S}, name::String) where {F, S}
    if check_can_assign_name(uf, CI, ci, name)
        if MOI.supportsconstraint(uf.model, F, S)
            MOI.set!(uf.model, attr, ci, name)
        else
            setname(uf.connames, uf.namescon, ci, name)
        end
    end
end
function MOI.get(uf::UniversalFallback, attr::MOI.ConstraintName, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        return MOI.get(uf.model, attr, ci)
    else
        return get(uf.connames, ci, EMPTYSTRING)
    end
end

MOI.get(uf::UniversalFallback, ::Type{VI}, name::String) = MOI.get(uf.model, VI, name)
function MOI.get(uf::UniversalFallback, ::Type{CI{F, S}}, name::String) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        return MOI.get(uf.model, CI{F, S}, name)
    else
        ci = get(uf.namescon, name, nothing)
        if ci isa CI{F, S}
            return ci
        else
            return nothing
        end
    end
end
function MOI.get(uf::UniversalFallback, ::Type{CI}, name::String)
    ci = MOI.get(uf.model, CI, name)
    if ci === nothing
        return get(uf.namescon, name, nothing)
    else
        return ci
    end
end

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
MOI.supports(::UniversalFallback, ::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = true
function MOI.set!(uf::UniversalFallback, attr::Union{MOI.AbstractOptimizerAttribute, MOI.AbstractModelAttribute}, value)
    if MOI.supports(uf.model, attr)
        return MOI.set!(uf.model, attr, value)
    else
        return _set!(uf, attr, value)
    end
end
MOI.supports(::UniversalFallback, ::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, ::Type{<:MOI.Index}) = true
function MOI.set!(uf::UniversalFallback, attr::MOI.AbstractVariableAttribute, idx::VI, value)
    if MOI.supports(uf.model, attr, typeof(idx))
        return MOI.set!(uf.model, attr, idx, value)
    else
        return _set!(uf, attr, idx, value)
    end
end
function MOI.set!(uf::UniversalFallback, attr::MOI.AbstractConstraintAttribute, idx::CI{F, S}, value) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S) && MOI.supports(uf.model, attr, CI{F, S})
        return MOI.set!(uf.model, attr, idx, value)
    else
        return _set!(uf, attr, idx, value)
    end
end

# Constraints
MOI.supportsconstraint(uf::UniversalFallback, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = true
function MOI.addconstraint!(uf::UniversalFallback, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    F = typeof(f)
    S = typeof(s)
    if MOI.supportsconstraint(uf.model, F, S)
        return MOI.addconstraint!(uf.model, f, s)
    else
        constraints = get!(uf.constraints, (F, S)) do
            Dict{CI{F, S}, Tuple{F, S}}()
        end::Dict{CI{F, S}, Tuple{F, S}}
        uf.nextconstraintid += 1
        ci = CI{F, S}(uf.nextconstraintid)
        constraints[ci] = (f, s)
        return ci
    end
end
function MOI.modify!(uf::UniversalFallback, ci::CI{F, S}, change::MOI.AbstractFunctionModification) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.modify!(uf.model, ci, change)
    else
        (f, s) = uf.constraints[(F, S)][ci]
        uf.constraints[(F, S)][ci] = (modifyfunction(f, change), s)
    end
end

function MOI.get(uf::UniversalFallback, attr::MOI.ConstraintFunction, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.get(uf.model, attr, ci)
    else
        uf.constraints[(F, S)][ci][1]
    end
end
function MOI.get(uf::UniversalFallback, attr::MOI.ConstraintSet, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.get(uf.model, attr, ci)
    else
        uf.constraints[(F, S)][ci][2]
    end
end
function MOI.set!(uf::UniversalFallback, ::MOI.ConstraintFunction, ci::CI{F,S}, func::F) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.set!(uf.model, MOI.ConstraintFunction(), ci, func)
    else
        (_, s) = uf.constraints[(F, S)][ci]
        uf.constraints[(F, S)][ci] = (func, s)
    end
end
function MOI.set!(uf::UniversalFallback, ::MOI.ConstraintSet, ci::CI{F,S}, set::S) where {F, S}
    if MOI.supportsconstraint(uf.model, F, S)
        MOI.set!(uf.model, MOI.ConstraintSet(), ci, set)
    else
        (f, _) = uf.constraints[(F, S)][ci]
        uf.constraints[(F, S)][ci] = (f, set)
    end
end

# Objective
MOI.modify!(uf::UniversalFallback, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification) = MOI.modify!(uf.model, obj, change)

# Variables
MOI.add_variable(uf::UniversalFallback) = MOI.add_variable(uf.model)
MOI.add_variables(uf::UniversalFallback, n) = MOI.add_variables(uf.model, n)
