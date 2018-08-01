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
    bridge_type(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `isbridged(b, F, S)`.
"""
function bridge_type end

function concrete_bridge_type(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    return concrete_bridge_type(bridge_type(b, F, S), F, S)
end

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
        if !MOI.isvalid(b, ci)
            throw(MOI.InvalidIndex(ci))
        end
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
for f in (:canget, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.$f(b.model, attr)
    end
end
# Objective function and model name
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}, value) = MOI.set!(b.model, attr, value)
MOI.canget(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::Type{<:MOI.Index}) = MOI.canget(b.model, attr, index)
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index) = MOI.get(b.model, attr, index)
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}) = MOI.get(b.model, attr, indices)
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::Union{MOI.AbstractVariableAttribute,
                                  MOI.AbstractConstraintAttribute},
                      IndexType::Type{<:MOI.Index})
    return MOI.supports(b.model, attr, IndexType)
end
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
        MOI.canget(b, attr, concrete_bridge_type(b, F, S))
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
function MOI.addconstraint!(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    if isbridged(b, typeof(f), typeof(s))
        ci = MOI.addconstraint!(b.bridged, f, s)
        @assert !haskey(b.bridges, ci)
        b.bridges[ci] = concrete_bridge_type(b, typeof(f), typeof(s))(b, f, s)
        ci
    else
        MOI.addconstraint!(b.model, f, s)
    end
end
function MOI.modify!(b::AbstractBridgeOptimizer, ci::CI, change::MOI.AbstractFunctionModification)
    if isbridged(b, typeof(ci))
        MOI.modify!(b, bridge(b, ci), change)
        MOI.modify!(b.bridged, ci, change)
    else
        MOI.modify!(b.model, ci, change)
    end
end

function MOI.supports(b::AbstractBridgeOptimizer, attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet}, ::Type{CI{F, S}}) where {F, S}
    if isbridged(b, CI{F, S})
        MOI.supports(b.bridged, attr, CI{F, S}) && MOI.supports(b, attr, concrete_bridge_type(b, F, S))
    else
        MOI.supports(b.model, attr, CI{F, S})
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
MOI.modify!(b::AbstractBridgeOptimizer, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification) = MOI.modify!(b.model, obj, change)

# Variables
MOI.addvariable!(b::AbstractBridgeOptimizer) = MOI.addvariable!(b.model)
MOI.addvariables!(b::AbstractBridgeOptimizer, n) = MOI.addvariables!(b.model, n)
