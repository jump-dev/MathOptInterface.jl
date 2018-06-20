"""
    AbstractBridgeOptimizer

A bridge optimizer applies a given constraint bridge to a given optimizer.
The attributes of the bridge optimizer are automatically computed to make the bridges transparent, e.g. the variables and constraints created by the bridges are hidden.
"""
abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end

# AbstractBridgeOptimizer interface

"""
    isbridged(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge `F`-in-`S` constraints instead of passing it as is to its internal model.
"""
function isbridged end
# Syntactic sugar
isbridged(b::AbstractBridgeOptimizer, ::Type{CI{F, S}}) where {F, S} = isbridged(b, F, S)

"""
    supportsbridgingconstraint(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` supports bridging `F`-in-`S` constraints.
"""
supportsbridgingconstraint(::AbstractBridgeOptimizer, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

"""
    bridgetype(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `isbridged(b, F, S)`.
"""
function bridgetype end

"""
    bridge(b::AbstractBridgeOptimizer, ci::CI)

Return the `AbstractBridge` used to bridge the constraint with index `ci`.
"""
bridge(b::AbstractBridgeOptimizer, ci::CI) = b.bridges[ci]
# By convention, they should be stored in a `bridges` field using a dictionary-like object.

# Implementation of the MOI interface for AbstractBridgeOptimizer

MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.model)

MOI.isempty(b::AbstractBridgeOptimizer) = isempty(b.bridges) && MOI.isempty(b.model)
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    MOI.empty!(b.bridged)
    empty!(b.bridges)
end
MOI.supports(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.supports(b.model, attr)
MOI.copy!(b::AbstractBridgeOptimizer, src::MOI.ModelLike; copynames=false) = MOIU.defaultcopy!(b, src, copynames)

# References
MOI.candelete(b::AbstractBridgeOptimizer, vi::VI) = MOI.candelete(b.model, vi)
function MOI.candelete(b::AbstractBridgeOptimizer, ci::CI)
    if isbridged(b, typeof(ci))
        MOI.candelete(b.bridged, ci) && MOI.candelete(b, bridge(b, ci))
    else
        MOI.candelete(b.model, ci)
    end
end
MOI.isvalid(b::AbstractBridgeOptimizer, vi::VI) = MOI.isvalid(b.model, vi)
function MOI.isvalid(b::AbstractBridgeOptimizer, ci::CI)
    if isbridged(b, typeof(ci))
        MOI.isvalid(b.bridged, ci)
    else
        MOI.isvalid(b.model, ci)
    end
end
MOI.delete!(b::AbstractBridgeOptimizer, vi::VI) = MOI.delete!(b.model, vi)
function MOI.delete!(b::AbstractBridgeOptimizer, ci::CI)
    if isbridged(b, typeof(ci))
        MOI.delete!(b, bridge(b, ci))
        delete!(b.bridges, ci)
        MOI.delete!(b.bridged, ci)
    else
        MOI.delete!(b.model, ci)
    end
end

# Attributes
function MOI.canget(b::AbstractBridgeOptimizer, attr::Union{MOI.NumberOfConstraints{F, S}, MOI.ListOfConstraintIndices{F, S}}) where {F, S}
    if isbridged(b, F, S)
        MOI.canget(b.bridged, attr)
    else
        MOI.canget(b.model, attr)
    end
end
function MOI.get(b::AbstractBridgeOptimizer, loc::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    if isbridged(b, F, S)
        locr = MOI.get(b.bridged, loc)
    else
        locr = MOI.get(b.model, loc)
    end
    for bridge in values(b.bridges)
        for c in MOI.get(bridge, loc)
            i = something(findfirst(isequal(c), locr), 0)
            if !iszero(i)
                MOI.deleteat!(locr, i)
            end
        end
    end
    locr
end
function _numberof(b::AbstractBridgeOptimizer, model::MOI.ModelLike, attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(model, attr)
    for v in values(b.bridges)
        s -= MOI.get(v, attr)
    end
    s
end
MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfVariables) = _numberof(b, b.model, attr)
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    if isbridged(b, F, S)
        # The constraints contained in `b.bridged` may have been added by bridges
        _numberof(b, b.bridged, attr)
    else
        _numberof(b, b.model, attr)
    end
end
MOI.canget(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints) = MOI.canget(b.model, attr) && MOI.canget(b.bridged, attr)
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    list_of_types = [MOI.get(b.model, attr); MOI.get(b.bridged, attr)]
    # Some constraint types show up in `list_of_types` even when all the constraints
    # of that type have been created by bridges and not by the user.
    # The code in `NumberOfConstraints` takes care of removing these constraints
    # from the counter so we can rely on it to remove these constraint types.
    types_to_remove = findall(iszero.(map(FS -> MOI.get(b, MOI.NumberOfConstraints{FS...}()), list_of_types)))
    deleteat!(list_of_types, types_to_remove)
    list_of_types
end
for f in (:canget, :canset, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.$f(b.model, attr)
    end
end
# Objective function and model name
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}, value) = MOI.set!(b.model, attr, value)
for f in (:canget, :canset)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::Type{<:MOI.Index}) = MOI.$f(b.model, attr, index)
    end
end
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index) = MOI.get(b.model, attr, index)
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}) = MOI.get(b.model, attr, indices)
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index, value) = MOI.set!(b.model, attr, index, value)
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}, values::Vector) = MOI.set!(b.model, attr, indices, values)

const InstanceConstraintAttribute = Union{MOI.ConstraintName, MOI.ConstraintFunction, MOI.ConstraintSet}
const SolverConstraintAttribute = Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart, MOI.ConstraintPrimal, MOI.ConstraintDual, MOI.ConstraintBasisStatus}
function MOI.canget(b::AbstractBridgeOptimizer, attr::InstanceConstraintAttribute, ci::Type{CI{F, S}}) where {F, S}
    if isbridged(b, F, S)
        MOI.canget(b.bridged, attr, ci)
    else
        MOI.canget(b.model, attr, ci)
    end
end
function MOI.canget(b::AbstractBridgeOptimizer, attr::SolverConstraintAttribute, ci::Type{CI{F, S}}) where {F, S}
    if isbridged(b, F, S)
        MOI.canget(b, attr, bridgetype(b, F, S))
    else
        MOI.canget(b.model, attr, ci)
    end
end
function MOI.get(b::AbstractBridgeOptimizer, attr::InstanceConstraintAttribute, ci::CI)
    if isbridged(b, typeof(ci))
        MOI.get(b.bridged, attr, ci)
    else
        MOI.get(b.model, attr, ci)
    end
end
function MOI.get(b::AbstractBridgeOptimizer, attr::SolverConstraintAttribute, ci::CI)
    if isbridged(b, typeof(ci))
        MOI.get(b, attr, bridge(b, ci))
    else
        MOI.get(b.model, attr, ci)
    end
end

# Name
MOI.canget(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.Index}, name::String) = MOI.canget(b.model, IdxT, name)
MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.Index}, name::String) = MOI.get(b.model, IdxT, name)

# Constraints
function MOI.supportsconstraint(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if isbridged(b, F, S)
        supportsbridgingconstraint(b, F, S) && MOI.supportsconstraint(b.bridged, F, S)
    else
        MOI.supportsconstraint(b.model, F, S)
    end
end
function MOI.canaddconstraint(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if isbridged(b, F, S)
        supportsbridgingconstraint(b, F, S) && MOI.canaddconstraint(b.bridged, F, S) && all(C -> MOI.canaddconstraint(b, C...), addedconstrainttypes(bridgetype(b, F, S), F, S))
    else
        MOI.canaddconstraint(b.model, F, S)
    end
end
function MOI.addconstraint!(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    if isbridged(b, typeof(f), typeof(s))
        ci = MOI.addconstraint!(b.bridged, f, s)
        @assert !haskey(b.bridges, ci)
        b.bridges[ci] = bridgetype(b, typeof(f), typeof(s))(b, f, s)
        ci
    else
        MOI.addconstraint!(b.model, f, s)
    end
end
function MOI.canmodify(b::AbstractBridgeOptimizer, ::Type{CI{F, S}}, ::Type{Chg}) where {F, S, Chg<:MOI.AbstractFunctionModification}
    if isbridged(b, CI{F, S})
        MOI.canmodify(b.bridged, CI{F, S}, Chg) && MOI.canmodify(b, MOIB.bridgetype(b, F, S), Chg)
    else
        MOI.canmodify(b.model, CI{F, S}, Chg)
    end
end
function MOI.modify!(b::AbstractBridgeOptimizer, ci::CI, change)
    if isbridged(b, typeof(ci))
        MOI.modify!(b, bridge(b, ci), change)
        MOI.modify!(b.bridged, ci, change)
    else
        MOI.modify!(b.model, ci, change)
    end
end

function MOI.canset(b::AbstractBridgeOptimizer, attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet}, ::Type{CI{F, S}}) where {F, S}
    if isbridged(b, CI{F, S})
        MOI.canset(b.bridged, attr, CI{F, S}) && MOI.canset(b, attr, MOIB.bridgetype(b, F, S))
    else
        MOI.canset(b.model, attr, CI{F, S})
    end
end

function MOI.set!(b::AbstractBridgeOptimizer, ::MOI.ConstraintSet, constraint_index::CI{F,S}, set::S) where {F,S}
    if isbridged(b, typeof(constraint_index))
        MOI.set!(b, MOI.ConstraintSet(), bridge(b, constraint_index), set)
        MOI.set!(b.bridged, MOI.ConstraintSet(), constraint_index, set)
    else
        MOI.set!(b.model, MOI.ConstraintSet(), constraint_index, set)
    end
end

function MOI.set!(b::AbstractBridgeOptimizer, ::MOI.ConstraintFunction, constraint_index::CI{F,S}, func::F) where {F,S}
    if isbridged(b, typeof(constraint_index))
        MOI.set!(b, MOI.ConstraintFunction(), bridge(b, constraint_index), func)
        MOI.set!(b.bridged, MOI.ConstraintFunction(), constraint_index, func)
    else
        MOI.set!(b.model, MOI.ConstraintFunction(), constraint_index, func)
    end
end

# Objective
MOI.canmodify(b::AbstractBridgeOptimizer, obj::MOI.ObjectiveFunction, ::Type{M}) where M<:MOI.AbstractFunctionModification = MOI.canmodify(b.model, obj, M)
MOI.modify!(b::AbstractBridgeOptimizer, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification) = MOI.modify!(b.model, obj, change)

# Variables
MOI.canaddvariable(b::AbstractBridgeOptimizer) = MOI.canaddvariable(b.model)
MOI.addvariable!(b::AbstractBridgeOptimizer) = MOI.addvariable!(b.model)
MOI.addvariables!(b::AbstractBridgeOptimizer, n) = MOI.addvariables!(b.model, n)
