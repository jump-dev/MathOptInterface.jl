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
    is_bridged(b::AbstractBridgeOptimizer, F::Type{<:MOI.AbstractFunction},
              S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge `F`-in-`S` constraints
instead of passing it as is to its internal model.
"""
function is_bridged end
# Syntactic sugar
function is_bridged(b::AbstractBridgeOptimizer, ::Type{CI{F, S}}) where {F, S}
    return is_bridged(b, F, S)
end
# We don't bridge variables.
is_bridged(b::AbstractBridgeOptimizer, ::Type{VI}) = false

"""
    supports_bridging_constraint(b::AbstractBridgeOptimizer,
                               F::Type{<:MOI.AbstractFunction},
                               S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` supports bridging `F`-in-`S` constraints.
"""
function supports_bridging_constraint(::AbstractBridgeOptimizer,
                                      ::Type{<:MOI.AbstractFunction},
                                      ::Type{<:MOI.AbstractSet})
    return false
end

"""
    bridge_type(b::AbstractBridgeOptimizer,
                F::Type{<:MOI.AbstractFunction},
                S::Type{<:MOI.AbstractSet})

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `is_bridged(b, F, S)`.
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
bridge(b::AbstractBridgeOptimizer, ci::CI) = b.bridges[ci.value]
# By convention, they should be stored in a `bridges` field using a
# dictionary-like object.
function bridge(b::AbstractBridgeOptimizer,
                ci::CI{MOI.SingleVariable, S}) where S
    return b.single_variable_constraints[(ci.value, S)]
end

# Implementation of the MOI interface for AbstractBridgeOptimizer

MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.model)
# By convention, the model should be stored in a `model` field

function MOI.is_empty(b::AbstractBridgeOptimizer)
    return isempty(b.bridges) && isempty(b.constraint_types) &&
           MOI.is_empty(b.model) && isempty(b.single_variable_constraints)
end
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    empty!(b.bridges)
    empty!(b.constraint_types)
    empty!(b.single_variable_constraints)
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::Union{MOI.AbstractModelAttribute,
                                  MOI.AbstractOptimizerAttribute})
    return MOI.supports(b.model, attr)
end

function MOI.copy_to(mock::AbstractBridgeOptimizer, src::MOI.ModelLike; kws...)
    MOIU.automatic_copy_to(mock, src; kws...)
end
function MOIU.supports_default_copy_to(b::AbstractBridgeOptimizer,
                                       copy_names::Bool)
    return MOIU.supports_default_copy_to(b.model, copy_names)
end

# References
function _constraint_index(b::AbstractBridgeOptimizer, i::Integer)
    F, S = b.constraint_types[i]
    return MOI.ConstraintIndex{F, S}(i)
end
MOI.is_valid(b::AbstractBridgeOptimizer, vi::VI) = MOI.is_valid(b.model, vi)
function MOI.is_valid(b::AbstractBridgeOptimizer, ci::CI{F, S}) where {F, S}
    if is_bridged(b, typeof(ci))
        return 1 ≤ ci.value ≤ length(b.bridges) && b.bridges[ci.value] !== nothing &&
            (F, S) == b.constraint_types[ci.value]
    else
        return MOI.is_valid(b.model, ci)
    end
end
function MOI.delete(b::AbstractBridgeOptimizer, vi::VI)
    for i in eachindex(b.bridges)
        if b.bridges[i] !== nothing && b.constraint_types[i][1] == MOI.SingleVariable
            ci = _constraint_index(b, i)
            f = MOI.get(b, MOI.ConstraintFunction(), ci)::MOI.SingleVariable
            if f.variable == vi
                MOI.delete(b, ci)
            end
        end
    end
    MOI.delete(b.model, vi)
end
function MOI.delete(b::AbstractBridgeOptimizer, ci::CI)
    if is_bridged(b, typeof(ci))
        if !MOI.is_valid(b, ci)
            throw(MOI.InvalidIndex(ci))
        end
        MOI.delete(b, bridge(b, ci))
        b.bridges[ci.value] = nothing
    else
        MOI.delete(b.model, ci)
    end
end

# Attributes
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    if is_bridged(b, F, S)
        list = MOI.ConstraintIndex{F, S}[MOI.ConstraintIndex{F, S}(i) for i in eachindex(b.bridges) if MOI.is_valid(b, MOI.ConstraintIndex{F, S}(i))]
    else
        list = MOI.get(b.model, attr)
    end
    for bridge in values(b.bridges)
        if bridge !== nothing
            for c in MOI.get(bridge, attr)
                i = something(findfirst(isequal(c), list), 0)
                if !iszero(i)
                    MOI.deleteat!(list, i)
                end
            end
        end
    end
    return list
end
function _numberof(b::AbstractBridgeOptimizer, model::MOI.ModelLike,
                   attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(model, attr)
    for bridge in values(b.bridges)
        if bridge !== nothing
            s -= MOI.get(bridge, attr)
        end
    end
    return s
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfVariables)
    return _numberof(b, b.model, attr)
end
function MOI.get(b::AbstractBridgeOptimizer,
                 attr::MOI.NumberOfConstraints{F, S}) where {F, S}
    if is_bridged(b, F, S)
        s = count(i -> MOI.is_valid(b, MOI.ConstraintIndex{F, S}(i)), eachindex(b.bridges))
    else
        s = MOI.get(b.model, attr)
    end
    # The constraints counted in `s` may have been added by bridges
    for v in values(b.bridges)
        if v !== nothing
            s -= MOI.get(v, attr)
        end
    end
    return s
end
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    list_of_types = MOI.get(b.model, attr)
    list_of_bridged_types = Set{Tuple{DataType, DataType}}()
    for i in eachindex(b.bridges)
        if b.bridges[i] !== nothing
            push!(list_of_bridged_types, b.constraint_types[i])
        end
    end
    list_of_types = [list_of_types; collect(list_of_bridged_types)]
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
                 attr::MOI.AbstractConstraintAttribute, ci::CI)
    if is_bridged(b, typeof(ci))
        return MOI.get(b, attr, bridge(b, ci))
    else
        return MOI.get(b.model, attr, ci)
    end
end
function MOI.supports(b::AbstractBridgeOptimizer,
                      attr::MOI.AbstractConstraintAttribute,
                      IndexType::Type{CI{F, S}}) where {F, S}
    if is_bridged(b, IndexType)
        return MOI.supports(b, attr, concrete_bridge_type(b, F, S))
    else
        return MOI.supports(b.model, attr, IndexType)
    end
end

function MOI.set(b::AbstractBridgeOptimizer,
                 attr::MOI.AbstractConstraintAttribute,
                 index::CI, value)
    if is_bridged(b, typeof(index))
        return MOI.set(b, attr, bridge(b, index), value)
    else
        return MOI.set(b.model, attr, index, value)
    end
end
## Getting and Setting names
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                 constraint_index::CI)
    if is_bridged(b, typeof(constraint_index))
        return get(b.con_to_name, constraint_index, MOIU.EMPTYSTRING)
    else
        return MOI.get(b.model, attr, constraint_index)
    end
end

function MOI.supports(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                      Index::Type{CI{F,S}}) where {F,S}
    if is_bridged(b, Index)
        return true
    else
        return MOI.supports(b.model, attr, Index)
    end
end
function MOI.set(b::AbstractBridgeOptimizer, attr::MOI.ConstraintName,
                  constraint_index::CI, name::String)
    if is_bridged(b, typeof(constraint_index))
        b.con_to_name[constraint_index] = name
        b.name_to_con = nothing # Invalidate the name map.
    else
        MOI.set(b.model, attr, constraint_index, name)
    end
end

# Name (similar to `UniversalFallback`)
function MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{MOI.VariableIndex},
                 name::String)
    return MOI.get(b.model, IdxT, name)
end

function MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.ConstraintIndex},
                 name::String)
    if b.name_to_con === nothing
        b.name_to_con = MOIU.build_name_to_con_map(b.con_to_name)
    end

    if is_bridged(b, IdxT)
        ci = get(b.name_to_con, name, nothing)
        if ci == CI{Nothing, Nothing}(-1)
            error("Multiple constraints have the name $name.")
        elseif ci isa IdxT
            return ci
        else
            return nothing
        end
    else
        ci = MOI.get(b.model, IdxT, name)
        if ci !== nothing && haskey(b.name_to_con, name)
            error("Multiple constraints have the name $name.")
        end
        return ci
    end
end

# We have no information as to whether the constraint is in the bridge or the
# model. Therefore, we try the model first, and then the bridge if that fails.
function MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{CI},
                 name::String)
    if b.name_to_con === nothing
        b.name_to_con = MOIU.build_name_to_con_map(b.con_to_name)
    end

    ci = MOI.get(b.model, IdxT, name)
    if ci === nothing
        b_ci = get(b.name_to_con, name, nothing)
        if b_ci == CI{Nothing, Nothing}(-1)
            error("Multiple constraints have the name $name.")
        else
            return b_ci
        end
    else
        if haskey(b.name_to_con, name)
            error("Multiple constraints have the name $name.")
        end

        return ci
    end
end

# Constraints
function MOI.supports_constraint(b::AbstractBridgeOptimizer,
                                F::Type{<:MOI.AbstractFunction},
                                S::Type{<:MOI.AbstractSet})
    if is_bridged(b, F, S)
        return supports_bridging_constraint(b, F, S)
    else
        return MOI.supports_constraint(b.model, F, S)
    end
end
function store_bridge(b::AbstractBridgeOptimizer, func::MOI.SingleVariable,
                      set::MOI.AbstractSet, bridge)
    b.single_variable_constraints[(func.variable.value, typeof(set))] = bridge
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(func.variable.value)
end
function store_bridge(b::AbstractBridgeOptimizer, func::MOI.AbstractFunction,
                      set::MOI.AbstractSet, bridge)
    push!(b.bridges, bridge)
    push!(b.constraint_types, (typeof(func), typeof(func)))
    return MOI.ConstraintIndex{typeof(func), typeof(func)}(length(b.bridges))
end
function MOI.add_constraint(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction,
                            s::MOI.AbstractSet)
    if is_bridged(b, typeof(f), typeof(s))
        # We compute `BridgeType` first as `concrete_bridge_type` calls
        # `bridge_type` which might throw an `UnsupportedConstraint` error in
        # which case, we do not want any modification to have been done
        BridgeType = concrete_bridge_type(b, typeof(f), typeof(s))
        # `add_constraint` might throw an `UnsupportedConstraint` but no
        # modification has been done in the previous line
        return store_bridge(b, f, s, bridge_constraint(BridgeType, b, f, s))
    else
        return MOI.add_constraint(b.model, f, s)
    end
end
function MOI.add_constraints(b::AbstractBridgeOptimizer, f::Vector{F},
                             s::Vector{S}) where { F <: MOI.AbstractFunction,
                             S <: MOI.AbstractSet}
    if is_bridged(b, F, S)
        return MOI.add_constraint.(b, f, s)
    else
        return MOI.add_constraints(b.model, f, s)
    end
end
function MOI.modify(b::AbstractBridgeOptimizer, ci::CI,
                     change::MOI.AbstractFunctionModification)
    if is_bridged(b, typeof(ci))
        MOI.modify(b, bridge(b, ci), change)
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

# TODO add transform
