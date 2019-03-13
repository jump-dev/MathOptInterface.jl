@enum CachingOptimizerState NO_OPTIMIZER EMPTY_OPTIMIZER ATTACHED_OPTIMIZER
@enum CachingOptimizerMode MANUAL AUTOMATIC

# TODO: Benchmark to check if CachingOptimizer should be parameterized on the ModelLike type.

"""
    CachingOptimizer

`CachingOptimizer` is an intermediate layer that stores a cache of the model
and links it with an optimizer. It supports incremental model
construction and modification even when the optimizer doesn't.

A `CachingOptimizer` may be in one of three possible states (`CachingOptimizerState`):

- `NO_OPTIMIZER`: The CachingOptimizer does not have any optimizer.
- `EMPTY_OPTIMIZER`: The CachingOptimizer an empty optimizer. The optimizer is not synchronized with the cached model.
- `ATTACHED_OPTIMIZER`: The CachingOptimizer a optimizer, and it is synchronized with the cached model.

A `CachingOptimizer` has two modes of operation (`CachingOptimizerMode`):

- `MANUAL`: The only methods that change the state of the `CachingOptimizer` are [`reset_optimizer`](@ref), [`drop_optimizer`](@ref), and [`attach_optimizer`](@ref). Attempting to perform an operation in the incorrect state results in an error.
- `AUTOMATIC`: The `CachingOptimizer` changes its state when necessary. For example, `optimize!` will automatically call `attach_optimizer` (an optimizer must have been previously set). Attempting to add a constraint or perform a modification not supported by the optimizer results in a drop to `EMPTY_OPTIMIZER` mode.
"""
mutable struct CachingOptimizer{OptimizerType, ModelType<:MOI.ModelLike} <: MOI.AbstractOptimizer
    optimizer::Union{Nothing, OptimizerType}
    model_cache::ModelType
    state::CachingOptimizerState
    mode::CachingOptimizerMode
    model_to_optimizer_map::IndexMap
    optimizer_to_model_map::IndexMap
    # CachingOptimizer externally uses the same variable and constraint indices
    # as the model_cache. model_to_optimizer_map maps from the model_cache indices to the
    # optimizer indices.
end

function CachingOptimizer(model_cache::MOI.ModelLike, mode::CachingOptimizerMode)
    CachingOptimizer{MOI.AbstractOptimizer, typeof(model_cache)}(nothing, model_cache, NO_OPTIMIZER, mode, IndexMap(), IndexMap())
end

"""
    CachingOptimizer(model_cache::MOI.ModelLike, optimizer::AbstractOptimizer)

Creates an `CachingOptimizer` in `AUTOMATIC` mode, with the optimizer `optimizer`.
The model_cache manager returned behaves like an `AbstractOptimizer` as long as no
`CachingOptimizer`-specific functions (e.g. `reset_optimizer`) are called on it.
The type of the optimizer returned is `CachingOptimizer{typeof(optimizer),
typeof(model_cache)}` so it does not support the function
`reset_optimizer(::CachingOptimizer, new_optimizer)` if the type of
`new_optimizer` is different from the type of `optimizer`.
"""
function CachingOptimizer(model_cache::MOI.ModelLike, optimizer::MOI.AbstractOptimizer)
    @assert MOI.is_empty(model_cache)
    @assert MOI.is_empty(optimizer)
    CachingOptimizer{typeof(optimizer), typeof(model_cache)}(optimizer, model_cache, ATTACHED_OPTIMIZER, AUTOMATIC, IndexMap(), IndexMap())
end

## Methods for managing the state of CachingOptimizer.

"""
    state(m::CachingOptimizer)::CachingOptimizerState

Returns the state of the CachingOptimizer `m`. See [`CachingOptimizer`](@ref).
"""
state(m::CachingOptimizer) = m.state

"""
    mode(m::CachingOptimizer)::CachingOptimizerMode

Returns the operating mode of the CachingOptimizer `m`. See [`CachingOptimizer`](@ref).
"""
mode(m::CachingOptimizer) = m.mode

"""
    reset_optimizer(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)

Sets or resets `m` to have the given empty optimizer. Can be called
from any state. The `CachingOptimizer` will be in state `EMPTY_OPTIMIZER` after the call.
"""
function reset_optimizer(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)
    @assert MOI.is_empty(optimizer)
    m.optimizer = optimizer
    m.state = EMPTY_OPTIMIZER
    for attr in MOI.get(m.model_cache, MOI.ListOfOptimizerAttributesSet())
        value = MOI.get(m.model_cache, attr)
        optimizer_value = attribute_value_map(m.model_to_optimizer_map, value)
        MOI.set(m.optimizer, attr, optimizer_value)
    end
    return
end

"""
    reset_optimizer(m::CachingOptimizer)

Detaches and empties the current optimizer. Can be called from `ATTACHED_OPTIMIZER`
or `EMPTY_OPTIMIZER` state. The `CachingOptimizer` will be in state `EMPTY_OPTIMIZER`
after the call.
"""
function reset_optimizer(m::CachingOptimizer)
    m.state == EMPTY_OPTIMIZER && return
    @assert m.state == ATTACHED_OPTIMIZER
    MOI.empty!(m.optimizer)
    m.state = EMPTY_OPTIMIZER
    return
end

"""
    drop_optimizer(m::CachingOptimizer)

Drops the optimizer, if one is present. Can be called from any state.
The `CachingOptimizer` will be in state `NO_OPTIMIZER` after the call.
"""
function drop_optimizer(m::CachingOptimizer)
    m.optimizer = nothing
    m.state = NO_OPTIMIZER
    return
end

"""
    attach_optimizer(model::CachingOptimizer)

Attaches the optimizer to `model`, copying all model data into it. Can be called
only from the `EMPTY_OPTIMIZER` state. If the copy succeeds, the
`CachingOptimizer` will be in state `ATTACHED_OPTIMIZER` after the call,
otherwise an error is thrown; see [`copy_to`](@ref) for more details on which
errors can be thrown.
"""
function attach_optimizer(model::CachingOptimizer)
    @assert model.state == EMPTY_OPTIMIZER
    # We do not need to copy names because name-related operations are handled by `m.model_cache`
    indexmap = MOI.copy_to(model.optimizer, model.model_cache, copy_names=false)
    model.state = ATTACHED_OPTIMIZER
    # MOI does not define the type of index_map, so we have to copy it into a
    # concrete container. Also load the reverse map.
    model.model_to_optimizer_map = IndexMap()
    model.optimizer_to_model_map = IndexMap()
    for k in keys(indexmap)
        model.model_to_optimizer_map[k] = indexmap[k]
        model.optimizer_to_model_map[indexmap[k]] = k
    end
end

function MOI.copy_to(m::CachingOptimizer, src::MOI.ModelLike; kws...)
    return automatic_copy_to(m, src; kws...)
end
function supports_default_copy_to(model::CachingOptimizer, copy_names::Bool)
    return supports_default_copy_to(model.model_cache, copy_names)
end

function MOI.empty!(m::CachingOptimizer)
    MOI.empty!(m.model_cache)
    if m.state == ATTACHED_OPTIMIZER
        MOI.empty!(m.optimizer)
    end
    if m.state == EMPTY_OPTIMIZER && m.mode == AUTOMATIC
        m.state = ATTACHED_OPTIMIZER
    end
    m.model_to_optimizer_map = IndexMap()
    m.optimizer_to_model_map = IndexMap()
end
MOI.is_empty(m::CachingOptimizer) = MOI.is_empty(m.model_cache)

# Optimizing and adding/modifying constraints and variables.

function MOI.optimize!(m::CachingOptimizer)
    if m.mode == AUTOMATIC && m.state == EMPTY_OPTIMIZER
        attach_optimizer(m)
    end
    # TODO: better error message if no optimizer is set
    @assert m.state == ATTACHED_OPTIMIZER
    MOI.optimize!(m.optimizer)
end

function MOI.add_variable(m::CachingOptimizer)
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                vindex_optimizer = MOI.add_variable(m.optimizer)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            vindex_optimizer = MOI.add_variable(m.optimizer)
        end
    end
    vindex = MOI.add_variable(m.model_cache)
    if m.state == ATTACHED_OPTIMIZER
        m.model_to_optimizer_map[vindex] = vindex_optimizer
        m.optimizer_to_model_map[vindex_optimizer] = vindex
    end
    return vindex
end

function MOI.add_variables(m::CachingOptimizer, n)
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                vindices_optimizer = MOI.add_variables(m.optimizer, n)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            vindices_optimizer = MOI.add_variables(m.optimizer, n)
        end
    end
    vindices = MOI.add_variables(m.model_cache, n)
    if m.state == ATTACHED_OPTIMIZER
        for (vindex, vindex_optimizer) in zip(vindices, vindices_optimizer)
            m.model_to_optimizer_map[vindex] = vindex_optimizer
            m.optimizer_to_model_map[vindex_optimizer] = vindex
        end
    end
    return vindices
end

function MOI.supports_constraint(m::CachingOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    MOI.supports_constraint(m.model_cache, F, S) && (m.state == NO_OPTIMIZER || MOI.supports_constraint(m.optimizer, F, S))
end

function MOI.add_constraint(m::CachingOptimizer, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                cindex_optimizer = MOI.add_constraint(m.optimizer, mapvariables(m.model_to_optimizer_map, func), set)
            catch err
                if err isa MOI.NotAllowedError
                    # It could be MOI.AddConstraintNotAllowed{F', S'} with F' != F
                    # or S' != S if, e.g., the `F`-in-`S` constraint is bridged
                    # to other constraints in `m.optimizer`
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            cindex_optimizer = MOI.add_constraint(m.optimizer, mapvariables(m.model_to_optimizer_map, func), set)
        end
    end
    cindex = MOI.add_constraint(m.model_cache, func, set)
    if m.state == ATTACHED_OPTIMIZER
        m.model_to_optimizer_map[cindex] = cindex_optimizer
        m.optimizer_to_model_map[cindex_optimizer] = cindex
    end
    return cindex
end

function MOI.modify(m::CachingOptimizer, cindex::CI, change::MOI.AbstractFunctionModification)
    if m.state == ATTACHED_OPTIMIZER
        cindex_optimizer = m.model_to_optimizer_map[cindex]
        change_optimizer = mapvariables(m.model_to_optimizer_map, change)
        if m.mode == AUTOMATIC
            try
                MOI.modify(m.optimizer, cindex_optimizer, change_optimizer)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.modify(m.optimizer, cindex_optimizer, change_optimizer)
        end
    end
    MOI.modify(m.model_cache, cindex, change)
end

# This function avoids duplicating code in the MOI.set methods for
# ConstraintSet and ConstraintFunction methods, but allows us to strongly type
# the third and fourth arguments of the set methods so that we only support
# setting the same type of set or function.
function replace_constraint_function_or_set(m::CachingOptimizer, attr, cindex, replacement)
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                MOI.set(m.optimizer, attr, m.model_to_optimizer_map[cindex], replacement)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.set(m.optimizer, attr, m.model_to_optimizer_map[cindex], replacement)
        end
    end
    MOI.set(m.model_cache, attr, cindex, replacement)
end

function MOI.set(m::CachingOptimizer, ::MOI.ConstraintSet, cindex::CI{F,S}, set::S) where {F,S}
    replace_constraint_function_or_set(m, MOI.ConstraintSet(), cindex, set)
end

function MOI.set(m::CachingOptimizer, ::MOI.ConstraintFunction, cindex::CI{F,S}, func::F) where {F,S}
    replace_constraint_function_or_set(m, MOI.ConstraintFunction(), cindex, func)
end

function MOI.modify(m::CachingOptimizer, obj::MOI.ObjectiveFunction, change::MOI.AbstractFunctionModification)
    if m.state == ATTACHED_OPTIMIZER
        change_optimizer = mapvariables(m.model_to_optimizer_map, change)
        if m.mode == AUTOMATIC
            try
                MOI.modify(m.optimizer, obj, change_optimizer)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.modify(m.optimizer, obj, change_optimizer)
        end
    end
    MOI.modify(m.model_cache, obj, change)
end

MOI.is_valid(m::CachingOptimizer, index::MOI.Index) = MOI.is_valid(m.model_cache, index)

function MOI.delete(m::CachingOptimizer, index::MOI.Index)
    if m.state == ATTACHED_OPTIMIZER
        if !MOI.is_valid(m, index)
            # The index thrown by m.model_cache would be xored
            throw(MOI.InvalidIndex(index))
        end
        index_optimizer = m.model_to_optimizer_map[index]
        if m.mode == AUTOMATIC
            try
                MOI.delete(m.optimizer, index_optimizer)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.delete(m.optimizer, index_optimizer)
        end
    end
    # The state may have changed in AUTOMATIC mode since reset_optimizer is
    # called in case the deletion is not supported
    if m.state == ATTACHED_OPTIMIZER
        delete!(m.optimizer_to_model_map, m.model_to_optimizer_map[index])
        delete!(m.model_to_optimizer_map, index)
    end
    MOI.delete(m.model_cache, index)
end


# TODO: add_constraints, transform

## CachingOptimizer get and set attributes

# Attributes are mapped through attribute_value_map (defined in copy.jl) before
# they are sent to the optimizer and when they are returned from the optimizer.
# This map currently only translates indices on MOI.AbstractFunction objects
# between the optimizer indices and the (user-facing) model_cache indices. As a result,
# all MOI.AbstractFunctions must implement mapvariables. Other attributes that
# store indices need to be handled with care.

function MOI.set(m::CachingOptimizer, attr::MOI.AbstractModelAttribute, value)
    if m.state == ATTACHED_OPTIMIZER
        optimizer_value = attribute_value_map(m.model_to_optimizer_map, value)
        if m.mode == AUTOMATIC
            try
                MOI.set(m.optimizer, attr, optimizer_value)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.set(m.optimizer, attr, optimizer_value)
        end
    end
    MOI.set(m.model_cache, attr, value)
end

function MOI.set(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, index::MOI.Index, value)
    if m.state == ATTACHED_OPTIMIZER
        optimizer_index = m.model_to_optimizer_map[index]
        optimizer_value = attribute_value_map(m.model_to_optimizer_map, value)
        if m.mode == AUTOMATIC
            try
                MOI.set(m.optimizer, attr, optimizer_index, optimizer_value)
            catch err
                if err isa MOI.NotAllowedError
                    reset_optimizer(m)
                else
                    rethrow(err)
                end
            end
        else
            MOI.set(m.optimizer, attr, optimizer_index, optimizer_value)
        end
    end
    MOI.set(m.model_cache, attr, index, value)
end

function MOI.supports(m::CachingOptimizer,
                      attr::Union{MOI.AbstractVariableAttribute,
                                  MOI.AbstractConstraintAttribute},
                      IndexType::Type{<:MOI.Index})
    return MOI.supports(m.model_cache, attr, IndexType) &&
        (m.state == NO_OPTIMIZER || MOI.supports(m.optimizer, attr, IndexType))
end

function MOI.supports(m::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    return MOI.supports(m.model_cache, attr) &&
        (m.state == NO_OPTIMIZER || MOI.supports(m.optimizer, attr))
end

function MOI.get(model::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    if MOI.is_set_by_optimize(attr)
        if state(model) == NO_OPTIMIZER
            if attr == MOI.TerminationStatus()
                return MOI.OPTIMIZE_NOT_CALLED
            elseif attr == MOI.PrimalStatus()
                return MOI.NO_SOLUTION
            elseif attr == MOI.DualStatus()
                return MOI.NO_SOLUTION
            else
                error("Cannot query $(attr) from caching optimizer because no" *
                      " optimizer is attached.")
            end
        end
        return attribute_value_map(model.optimizer_to_model_map,
                                   MOI.get(model.optimizer, attr))
    else
        return MOI.get(model.model_cache, attr)
    end
end
function MOI.get(model::CachingOptimizer,
                 attr::Union{MOI.AbstractVariableAttribute,
                             MOI.AbstractConstraintAttribute},
                 index::MOI.Index)
    if MOI.is_set_by_optimize(attr)
        if state(model) == NO_OPTIMIZER
            error("Cannot query $(attr) from caching optimizer because no " *
                  "optimizer is attached.")
        end
        return attribute_value_map(model.optimizer_to_model_map,
                                   MOI.get(model.optimizer, attr,
                                           model.model_to_optimizer_map[index]))
    else
        return MOI.get(model.model_cache, attr, index)
    end
end
function MOI.get(model::CachingOptimizer,
                 attr::Union{MOI.AbstractVariableAttribute,
                             MOI.AbstractConstraintAttribute},
                 indices::Vector{<:MOI.Index})
    if MOI.is_set_by_optimize(attr)
        if state(model) == NO_OPTIMIZER
            error("Cannot query $(attr) from caching optimizer because no " *
                  "optimizer is attached.")
        end
        return attribute_value_map(model.optimizer_to_model_map,
                                   MOI.get(model.optimizer, attr,
                                           map(index -> model.model_to_optimizer_map[index],
                                               indices)))
  else
      return MOI.get(model.model_cache, attr, indices)
  end
end

#####
##### Names
#####

# Names are not copied, i.e. we use the option `copy_names=false` in
# `attachoptimizer`, so the caching optimizer can support names even if the
# optimizer does not.
function MOI.supports(model::CachingOptimizer,
                      attr::Union{MOI.VariableName,
                                  MOI.ConstraintName},
                      IndexType::Type{<:MOI.Index})
    return MOI.supports(model.model_cache, attr, IndexType)
end
function MOI.set(model::CachingOptimizer,
                 attr::Union{MOI.VariableName, MOI.ConstraintName},
                 index::MOI.Index, value)
    MOI.set(model.model_cache, attr, index, value)
end

function MOI.supports(m::CachingOptimizer,
                      attr::MOI.Name)
    return MOI.supports(m.model_cache, attr)
end
function MOI.set(model::CachingOptimizer, attr::MOI.Name, value)
    MOI.set(model.model_cache, attr, value)
end

function MOI.get(m::CachingOptimizer, IdxT::Type{<:MOI.Index}, name::String)
    return MOI.get(m.model_cache, IdxT, name)
end

function MOI.set(model::CachingOptimizer, attr::MOI.AbstractOptimizerAttribute,
                 value)
    optimizer_value = attribute_value_map(model.model_to_optimizer_map, value)
    if model.optimizer !== nothing
        MOI.set(model.optimizer, attr, optimizer_value)
    end
    MOI.set(model.model_cache, attr, value)
end

function MOI.get(model::CachingOptimizer, attr::MOI.AbstractOptimizerAttribute)
    if state(model) == NO_OPTIMIZER
        # TODO: Copyable attributes (e.g., Silent, TimeLimit) are also be stored
        # in the cache so we could return the value stored in the cache instead.
        # However, for non-copyable attributes( e.g. `SolverName`) the error is
        # appropriate.
        error("Cannot query $(attr) from caching optimizer because no " *
              "optimizer is attached.")
    end
    return attribute_value_map(model.optimizer_to_model_map,
                               MOI.get(model.optimizer, attr))
end

# Force users to specify whether the attribute should be queried from the
# model_cache or the optimizer. Maybe we could consider a small whitelist of
# attributes to handle automatically.

# These are expert methods to get or set attributes directly in the model_cache
# or optimizer.

struct AttributeFromModelCache{T <: MOI.AnyAttribute}
    attr::T
end

struct AttributeFromOptimizer{T <: MOI.AnyAttribute}
    attr::T
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromModelCache{T}) where {T <: MOI.AbstractModelAttribute}
    return MOI.get(m.model_cache, attr.attr)
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idx) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.get(m.model_cache, attr.attr, idx)
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr))
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx::MOI.Index) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == ATTACHED_OPTIMIZER
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr, m.model_to_optimizer_map[idx]))
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx::Vector{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == ATTACHED_OPTIMIZER
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr, getindex.(m.model_to_optimizer_map,idx)))
end

function MOI.set(m::CachingOptimizer, attr::AttributeFromModelCache{T}, v) where {T <: MOI.AbstractModelAttribute}
    return MOI.set(m.model_cache, attr.attr, v)
end

function MOI.set(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.set(m.model_cache, attr.attr, idx, v)
end

function MOI.set(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, v) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.set(m.optimizer, attr.attr, attribute_value_map(m.model_to_optimizer_map,v))
end

# Map vector of indices into vector of indices or one index into one index
map_indices_to_optimizer(m::CachingOptimizer, idx::MOI.Index) = m.model_to_optimizer_map[idx]
map_indices_to_optimizer(m::CachingOptimizer, indices::Vector{<:MOI.Index}) = getindex.(Ref(m.model_to_optimizer_map), indices)
function MOI.set(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.set(m.optimizer, attr.attr, map_indices_to_optimizer(m, idx), attribute_value_map(m.model_to_optimizer_map,v))
end

function MOI.supports(m::CachingOptimizer, attr::AttributeFromModelCache{T}) where {T <: MOI.AbstractModelAttribute}
    return MOI.supports(m.model_cache, attr.attr)
end

function MOI.supports(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.supports(m.model_cache, attr.attr, idxtype)
end

function MOI.supports(m::CachingOptimizer, attr::AttributeFromOptimizer{T}) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.supports(m.optimizer, attr.attr)
end

function MOI.supports(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.supports(m.optimizer, attr.attr, idxtype)
end

# TODO: get and set methods to look up/set name strings
