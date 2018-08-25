"""
    AbstractBridgeOptimizer

A bridge optimizer applies given constraint bridges to a given optimizer thus
extending the types of supported constraints. The attributes of the inner
optimizer are automatically transformed to make the bridges transparent, e.g.
the variables and constraints created by the bridges are hidden.

By convention, the inner optimizer should be stored in a `model` field and
the dictionary mapping constraint indices to bridges should be stored in a
`bridges` field. If a bridge optimizer deviates from these conventions, it
should implement the functions `MOI.optimize!` and `bridge` respectively.
"""
abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end

# AbstractBridgeOptimizer interface

"""
    isbridged(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
              S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge `F`-in-`S` constraints
instead of passing it as is to its internal model.
"""
function isbridged end
# Syntactic sugar
function isbridged(b::AbstractBridgeOptimizer, ::Type{CI{F, S}}) where {F, S}
    return isbridged(b, F, S)
end
# We don't bridge variables.
isbridged(b::AbstractBridgeOptimizer, ::Type{VI}) = false

"""
    supportsbridgingconstraint(b::AbstractBridgeOptimizer,
                               F::Type{<:MOI.AbstractFunction},
                               S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` supports bridging `F`-in-`S` constraints.
"""
function supportsbridgingconstraint(::AbstractBridgeOptimizer,
                                    ::Type{<:MOI.AbstractFunction},
                                    ::Type{<:MOI.AbstractSet})
    return false
end

"""
    bridge_type(b::AbstractBridgeOptimizer,
                F::Type{<:MOI.AbstractFunction},
                S::Type{<:MOI.AbstractSet})

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `isbridged(b, F, S)`.
"""
function bridge_type end

function concrete_bridge_type(b::AbstractBridgeOptimizer,
                              F::Type{<:MOI.AbstractFunction},
                              S::Type{<:MOI.AbstractSet})
    return concrete_bridge_type(bridge_type(b, F, S), F, S)
end

"""
    bridge(b::AbstractBridgeOptimizer, ci::CI)

Return the `AbstractBridge` used to bridge the constraint with index `ci`.
"""
bridge(b::AbstractBridgeOptimizer, ci::CI) = b.bridges[ci]
# By convention, they should be stored in a `bridges` field using a
# dictionary-like object.

# Implementation of the MOI interface for AbstractBridgeOptimizer

MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.model)
# By convention, the model should be stored in a `model` field

function MOI.isempty(b::AbstractBridgeOptimizer)
    return isempty(b.bridges) && MOI.isempty(b.model)
end
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    MOI.empty!(b.bridged)
    empty!(b.bridges)
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::Union{MOI.AbstractModelAttribute,
                                  MOI.AbstractOptimizerAttribute})
    return MOI.supports(b.model, attr)
end
function MOI.copy_to(b::AbstractBridgeOptimizer, src::MOI.ModelLike;
                   copynames = true)
    return MOIU.default_copy_to(b, src, copynames)
end

# References
MOI.is_valid(b::AbstractBridgeOptimizer, vi::VI) = MOI.is_valid(b.model, vi)
function MOI.is_valid(b::AbstractBridgeOptimizer, ci::CI)
    if isbridged(b, typeof(ci))
        return MOI.is_valid(b.bridged, ci)
    else
        return MOI.is_valid(b.model, ci)
    end
end
MOI.delete!(b::AbstractBridgeOptimizer, vi::VI) = MOI.delete!(b.model, vi)
function MOI.delete!(b::AbstractBridgeOptimizer, ci::CI)
    if isbridged(b, typeof(ci))
        if !MOI.is_valid(b, ci)
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
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    if isbridged(b, F, S)
        list = MOI.get(b.bridged, attr)
    else
        list = MOI.get(b.model, attr)
    end
    for bridge in values(b.bridges)
        for c in MOI.get(bridge, attr)
            i = something(findfirst(isequal(c), list), 0)
            if !iszero(i)
                MOI.deleteat!(list, i)
            end
        end
    end
    return list
end
function _numberof(b::AbstractBridgeOptimizer, model::MOI.ModelLike,
                   attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(model, attr)
    for v in values(b.bridges)
        s -= MOI.get(v, attr)
    end
    return s
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfVariables)
    return _numberof(b, b.model, attr)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    if isbridged(b, F, S)
        # The constraints contained in `b.bridged` may have been added by
        # bridges
        return _numberof(b, b.bridged, attr)
    else
        return _numberof(b, b.model, attr)
    end
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    list_of_types = [MOI.get(b.model, attr); MOI.get(b.bridged, attr)]
    # Some constraint types show up in `list_of_types` even when all the
    # constraints of that type have been created by bridges and not by the user.
    # The code in `NumberOfConstraints` takes care of removing these constraints
    # from the counter so we can rely on it to remove these constraint types.
    types_to_remove = findall(iszero.(
        map(FS -> MOI.get(b, MOI.NumberOfConstraints{FS...}()), list_of_types)))
    deleteat!(list_of_types, types_to_remove)
    return list_of_types
end

# Model an optimizer attributes
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::Union{MOI.AbstractModelAttribute,
                             MOI.AbstractOptimizerAttribute})
    return MOI.get(b.model, attr)
end
function MOI.set(b::AbstractBridgeOptimizer,
                  attr::Union{MOI.AbstractModelAttribute,
                              MOI.AbstractOptimizerAttribute},
                  value)
    return MOI.set(b.model, attr, value)
end

# Variable attributes
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 index::VI)
    return MOI.get(b.model, attr, index)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractVariableAttribute,
                 indices::Vector{VI})
    return MOI.get(b.model, attr, indices)
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::MOI.AbstractVariableAttribute,
                      IndexType::Type{<:MOI.Index})
    return MOI.supports(b.model, attr, IndexType)
end
function MOI.set(b::AbstractBridgeOptimizer,
                  attr::MOI.AbstractVariableAttribute,
                  index::MOI.Index, value)
    return MOI.set(b.model, attr, index, value)
end
function MOI.set(b::AbstractBridgeOptimizer,
                  attr::MOI.AbstractVariableAttribute,
                  indices::Vector{<:MOI.Index}, values::Vector)
    return MOI.set(b.model, attr, indices, values)
end

# Constraint attributes
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractConstraintAttribute,
                 ci::CI)
    if isbridged(b, typeof(ci))
        if MOIU.is_result_attribute(attr)
            MOI.get(b, attr, bridge(b, ci))
        else
            MOI.get(b.bridged, attr, ci)
        end
    else
        MOI.get(b.model, attr, ci)
    end
end
## Setting names
function MOI.supports(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                      Index::Type{<:CI})
    if isbridged(b, Index)
        return MOI.supports(b.bridged, attr, Index)
    else
        return MOI.supports(b.model, attr, Index)
    end
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                  constraint_index::CI, name::String)
    if isbridged(b, typeof(constraint_index))
        MOI.set(b.bridged, attr, constraint_index, name)
    else
        MOI.set(b.model, attr, constraint_index, name)
    end
end
## Setting functions and sets
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::Union{MOI.ConstraintFunction, MOI.ConstraintSet},
                      ::Type{CI{F, S}}) where {F, S}
    if isbridged(b, CI{F, S})
        return MOI.supports(b.bridged, attr, CI{F, S}) &&
            MOI.supports(b, attr, concrete_bridge_type(b, F, S))
    else
        return MOI.supports(b.model, attr, CI{F, S})
    end
end
function MOI.set(b::AbstractBridgeOptimizer, ::MOI.ConstraintSet,
                  constraint_index::CI{F, S}, set::S) where {F, S}
    if isbridged(b, typeof(constraint_index))
        MOI.set(b, MOI.ConstraintSet(), bridge(b, constraint_index), set)
        MOI.set(b.bridged, MOI.ConstraintSet(), constraint_index, set)
    else
        MOI.set(b.model, MOI.ConstraintSet(), constraint_index, set)
    end
end
function MOI.set(b::AbstractBridgeOptimizer, ::MOI.ConstraintFunction,
                  constraint_index::CI{F, S}, func::F) where {F, S}
    if isbridged(b, typeof(constraint_index))
        MOI.set(b, MOI.ConstraintFunction(), bridge(b, constraint_index), func)
        MOI.set(b.bridged, MOI.ConstraintFunction(), constraint_index, func)
    else
        MOI.set(b.model, MOI.ConstraintFunction(), constraint_index, func)
    end
end

# Name
function MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.Index},
                 name::String)
    if isbridged(b, IdxT)
        return MOI.get(b.bridged, IdxT, name)
    else
        return MOI.get(b.model, IdxT, name)
    end
end

# We have no information as to whether the constraint is in the bridge or the
# model. Therefore, we try the model first, and then the bridge if that fails.
function MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{CI},
                 name::String)
    ci = MOI.get(b.model, IdxT, name)
    if ci === nothing
        return MOI.get(b.bridged, IdxT, name)
    else
        return ci
    end
end

# Constraints
function MOI.supports_constraint(b::AbstractBridgeOptimizer,
                                F::Type{<:MOI.AbstractFunction},
                                S::Type{<:MOI.AbstractSet})
    if isbridged(b, F, S)
        return supportsbridgingconstraint(b, F, S) &&
            MOI.supports_constraint(b.bridged, F, S)
    else
        return MOI.supports_constraint(b.model, F, S)
    end
end
function MOI.add_constraint(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction,
                            s::MOI.AbstractSet)
    if isbridged(b, typeof(f), typeof(s))
        ci = MOI.add_constraint(b.bridged, f, s)
        @assert !haskey(b.bridges, ci)
        b.bridges[ci] = concrete_bridge_type(b, typeof(f), typeof(s))(b, f, s)
        return ci
    else
        return MOI.add_constraint(b.model, f, s)
    end
end
function MOI.modify(b::AbstractBridgeOptimizer, ci::CI,
                     change::MOI.AbstractFunctionModification)
    if isbridged(b, typeof(ci))
        MOI.modify(b, bridge(b, ci), change)
        MOI.modify(b.bridged, ci, change)
    else
        MOI.modify(b.model, ci, change)
    end
end

# Objective
function MOI.modify(b::AbstractBridgeOptimizer, obj::MOI.ObjectiveFunction,
                     change::MOI.AbstractFunctionModification)
    MOI.modify(b.model, obj, change)
end

# Variables
MOI.add_variable(b::AbstractBridgeOptimizer) = MOI.add_variable(b.model)
MOI.add_variables(b::AbstractBridgeOptimizer, n) = MOI.add_variables(b.model, n)
