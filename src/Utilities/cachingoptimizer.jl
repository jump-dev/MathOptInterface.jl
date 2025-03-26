# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Tips for writing type stable code in this file.
#
# Because the .optimizer field can by an abstract type (for example, JuMP uses
# AbstractOptimizer), referring to `.optimizer` in a function is not always
# type-stable. To help the compiler, always annotate the return type of a
# function that uses `.optimizer`. For example:
# `MOI.supports(model.optimizer, F, S)::Bool`.

MOI.@_documented_enum(
    """
        CachingOptimizerState

    A [`Utilities.CachingOptimizer`](@ref) may be in one of three possible
    states.
    """,
    CachingOptimizerState,
    "The CachingOptimizer does not have any optimizer.",
    NO_OPTIMIZER,
    """
    The CachingOptimizer has an optimizer. The optimizer is empty and it is not
    synchronized with the cached model.
    """,
    EMPTY_OPTIMIZER,
    """
    The CachingOptimizer has an optimizer, and it is synchronized with the
    cached model.
    """,
    ATTACHED_OPTIMIZER,
)
MOI.@_documented_enum(
    """
        CachingOptimizerMode

    A [`Utilities.CachingOptimizer`](@ref) has two modes of operation.
    """,
    CachingOptimizerMode,
    """
    The only methods that change the state of the `CachingOptimizer`
    are [`Utilities.reset_optimizer`](@ref), [`Utilities.drop_optimizer`](@ref),
    and [`Utilities.attach_optimizer`](@ref).

    Attempting to perform an operation in the incorrect state results in an
    error.
    """,
    MANUAL,
    """
    The [`Utilities.CachingOptimizer`](@ref) changes its state when necessary.
    For example, [`MOI.optimize!`](@ref) will automatically call
    [`Utilities.attach_optimizer`](@ref) (an optimizer must have been previously
    set). Attempting to add a constraint or perform a modification not supported
    by the optimizer results in a drop to the [`EMPTY_OPTIMIZER`](@ref) state.
    """,
    AUTOMATIC,
)

"""
    CachingOptimizer

`CachingOptimizer` is an intermediate layer that stores a cache of the model and
links it with an optimizer. It supports incremental model construction and
modification even when the optimizer doesn't.

## Mode

A [`Utilities.CachingOptimizer`](@ref) has two modes of operation:
[`Utilities.AUTOMATIC`](@ref) and [`Utilities.MANUAL`](@ref). See their
docstrings for details.

Use [`Utilities.mode`](@ref) to query the mode of a
[`Utilities.CachingOptimizer`](@ref).

## State

A [`Utilities.CachingOptimizer`](@ref) may be in one of three possible states:
[`NO_OPTIMIZER`](@ref), [`Utilities.EMPTY_OPTIMIZER`](@ref), and
[`Utilities.ATTACHED_OPTIMIZER`](@ref). See their docstrings for details.

Use [`Utilities.state`](@ref) to query the state of a
[`Utilities.CachingOptimizer`](@ref).

## Constructor with optimizer

```julia
    CachingOptimizer(cache::MOI.ModelLike, optimizer::AbstractOptimizer)
```

Creates a `CachingOptimizer` in [`AUTOMATIC`](@ref) mode, with the optimizer
`optimizer`.

The type of the optimizer returned is
`CachingOptimizer{typeof(optimizer),typeof(cache)}` so it does not support the
function [`Utilities.reset_optimizer(::CachingOptimizer, new_optimizer)`](@ref)
if the type of `new_optimizer` is different from the type of `optimizer`.

## Constructor without optimizer

```julia
    CachingOptimizer(cache::MOI.ModelLike, mode::CachingOptimizerMode)
```

Creates a `CachingOptimizer` in the [`NO_OPTIMIZER`](@ref)
[`Utilities.CachingOptimizerState`](@ref) and the
[`Utilities.CachingOptimizerMode`](@ref) `mode`.

The type of the optimizer returned is
`CachingOptimizer{MOI.AbstractOptimizer,typeof(cache)}` so it _does_ support the
function `reset_optimizer(::CachingOptimizer, new_optimizer)` if the type of
`new_optimizer` is different from the type of `optimizer`.
"""
mutable struct CachingOptimizer{O,M<:MOI.ModelLike} <: MOI.AbstractOptimizer
    optimizer::Union{Nothing,O}
    model_cache::M
    state::CachingOptimizerState
    mode::CachingOptimizerMode
    model_to_optimizer_map::IndexMap
    optimizer_to_model_map::IndexMap
    # CachingOptimizer externally uses the same variable and constraint indices
    # as the model_cache. model_to_optimizer_map maps from the model_cache
    # indices to the optimizer indices.

    function CachingOptimizer(cache::MOI.ModelLike, mode::CachingOptimizerMode)
        return new{MOI.AbstractOptimizer,typeof(cache)}(
            nothing,
            cache,
            NO_OPTIMIZER,
            mode,
            IndexMap(),
            IndexMap(),
        )
    end

    function CachingOptimizer(cache::MOI.ModelLike, optimizer::MOI.ModelLike)
        @assert MOI.is_empty(optimizer)
        model = new{typeof(optimizer),typeof(cache)}(
            optimizer,
            cache,
            EMPTY_OPTIMIZER,
            AUTOMATIC,
            IndexMap(),
            IndexMap(),
        )
        # Optimizer attributes should be copied now. Other attributes are copied
        # later during `copy_to`.
        _copy_optimizer_attributes(model)
        return model
    end
end

function Base.show(io::IO, model::CachingOptimizer)
    offset = Base.get(io, :offset, "")
    println(io, "MOIU.CachingOptimizer")
    println(io, offset, "├ state: $(model.state)")
    println(io, offset, "├ mode: $(model.mode)")
    print_with_acronym(io, "$(offset)├ model_cache: ")
    show(IOContext(io, :offset => offset * "│ "), model.model_cache)
    print_with_acronym(io, "\n$(offset)└ optimizer: ")
    if model.optimizer === nothing
        print(io, "nothing")
    else
        show(IOContext(io, :offset => offset * "  "), model.optimizer)
    end
    return
end

function _copy_optimizer_attributes(m::CachingOptimizer)
    @assert m.state == EMPTY_OPTIMIZER
    for attr in MOI.get(m.model_cache, MOI.ListOfOptimizerAttributesSet())
        # Skip attributes which don't apply to the new optimizer.
        if attr isa MOI.RawOptimizerAttribute
            # Even if the optimizer claims to `supports` `attr`, the value
            # might have a different meaning (for example, two solvers with `logLevel`
            # as a RawOptimizerAttribute). To be on the safe side, just skip all
            # raw parameters.
            continue
        elseif !MOI.is_copyable(attr) || !MOI.supports(m.optimizer, attr)::Bool
            continue
        end
        value = MOI.get(m.model_cache, attr)
        optimizer_value = map_indices(m.model_to_optimizer_map, attr, value)
        MOI.set(m.optimizer, attr, optimizer_value)
    end
    return
end

## Methods for managing the state of CachingOptimizer.

"""
    state(m::CachingOptimizer)::CachingOptimizerState

Returns the state of the CachingOptimizer `m`.

See [`Utilities.CachingOptimizer`](@ref).
"""
state(m::CachingOptimizer) = m.state

"""
    mode(m::CachingOptimizer)::CachingOptimizerMode

Returns the operating mode of the CachingOptimizer `m`.

See [`Utilities.CachingOptimizer`](@ref).
"""
mode(m::CachingOptimizer) = m.mode

"""
    reset_optimizer(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)

Sets or resets `m` to have the given empty optimizer `optimizer`.

Can be called from any state. An assertion error will be thrown if `optimizer`
is not empty.

The `CachingOptimizer` `m` will be in state `EMPTY_OPTIMIZER` after the call.
"""
function reset_optimizer(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)
    @assert MOI.is_empty(optimizer)
    m.optimizer = optimizer
    m.state = EMPTY_OPTIMIZER
    _copy_optimizer_attributes(m)
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
otherwise an error is thrown; see [`MOI.copy_to`](@ref) for more details on which
errors can be thrown.
"""
function attach_optimizer(model::CachingOptimizer)
    @assert model.state == EMPTY_OPTIMIZER
    final_touch(model, nothing)
    indexmap = MOI.copy_to(model.optimizer, model.model_cache)::IndexMap
    model.state = ATTACHED_OPTIMIZER
    model.model_to_optimizer_map = indexmap
    model.optimizer_to_model_map = _reverse_index_map(indexmap)
    return
end

function _reverse_index_map(src::IndexMap)
    dest = IndexMap()
    sizehint!(dest.var_map, length(src.var_map))
    _reverse_dict(dest.var_map, src.var_map)
    _reverse_dict(dest.con_map, src.con_map)
    return dest
end

function _reverse_dict(dest::AbstractDict, src::AbstractDict)
    for (k, v) in src
        dest[v] = k
    end
    return
end

function _reverse_dict(
    dest::DoubleDicts.IndexDoubleDict,
    src::DoubleDicts.IndexDoubleDict,
)
    for (key, value) in src.dict
        dest.dict[key] = MOI.Utilities._reverse_dict(value)
    end
    return
end

function _reverse_dict(src::D) where {D<:Dict}
    return D(values(src) .=> keys(src))
end

function pass_nonvariable_constraints(
    dest::CachingOptimizer,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types,
)
    # This method gets called during default_copy_to, so the dest cannot be
    # attached at that point.
    @assert dest.state != ATTACHED_OPTIMIZER
    pass_nonvariable_constraints(
        dest.model_cache,
        src,
        idxmap,
        constraint_types,
    )
    return
end

function final_touch(m::CachingOptimizer, index_map)
    return final_touch(m.model_cache, index_map)
end

function MOI.copy_to(m::CachingOptimizer, src::MOI.ModelLike)
    if m.state == ATTACHED_OPTIMIZER
        reset_optimizer(m)
    end
    return MOI.copy_to(m.model_cache, src)
end

function MOI.supports_incremental_interface(model::CachingOptimizer)
    return MOI.supports_incremental_interface(model.model_cache)
end

function MOI.empty!(m::CachingOptimizer)
    MOI.empty!(m.model_cache)
    if m.state != NO_OPTIMIZER
        # We call `empty!` even if the state is already `EMPTY_OPTIMIZER`
        # because we may have used to two-argument `optimize!` for a single-shot
        # solver.
        MOI.empty!(m.optimizer)
        m.state = EMPTY_OPTIMIZER
    end
    m.model_to_optimizer_map = IndexMap()
    m.optimizer_to_model_map = IndexMap()
    return
end

MOI.is_empty(m::CachingOptimizer) = MOI.is_empty(m.model_cache)

# Optimizing and adding/modifying constraints and variables.

function MOI.optimize!(m::CachingOptimizer)
    if m.mode == AUTOMATIC && m.state == EMPTY_OPTIMIZER
        final_touch(m.model_cache, nothing)
        # Here is a special case for callbacks: we can't use the two-argument
        # call to `optimize!` because we need the `optimizer_to_model_map` to be
        # set _prior_ to starting the optimization process. Therefore, we need
        # to check if we have an `AbstractCallback` set and attach the optimizer
        # before recalling `MOI.optimize!`.
        for attr in MOI.get(m, MOI.ListOfModelAttributesSet())
            if attr isa MOI.AbstractCallback
                attach_optimizer(m)
                MOI.optimize!(m)
                return
            end
        end
        indexmap, copied = MOI.optimize!(m.optimizer, m.model_cache)
        if copied
            m.state = ATTACHED_OPTIMIZER
        end
        m.model_to_optimizer_map = indexmap
        m.optimizer_to_model_map = _reverse_index_map(indexmap)
    else
        # TODO: better error message if no optimizer is set
        @assert m.state == ATTACHED_OPTIMIZER
        MOI.optimize!(m.optimizer)
    end
    return
end

function MOI.add_variable(m::CachingOptimizer)
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                vindex_optimizer =
                    MOI.add_variable(m.optimizer)::MOI.VariableIndex
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            vindex_optimizer = MOI.add_variable(m.optimizer)::MOI.VariableIndex
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
                vindices_optimizer =
                    MOI.add_variables(m.optimizer, n)::Vector{MOI.VariableIndex}
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            vindices_optimizer =
                MOI.add_variables(m.optimizer, n)::Vector{MOI.VariableIndex}
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

function MOI.supports_add_constrained_variable(
    m::CachingOptimizer,
    S::Type{<:MOI.AbstractScalarSet},
)
    return MOI.supports_add_constrained_variable(m.model_cache, S) && (
        m.state == NO_OPTIMIZER ||
        MOI.supports_add_constrained_variable(m.optimizer, S)::Bool
    )
end

function MOI.add_constrained_variable(
    m::CachingOptimizer,
    set::S,
) where {S<:MOI.AbstractScalarSet}
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                vindex_optimizer, cindex_optimizer =
                    MOI.add_constrained_variable(
                        m.optimizer,
                        set,
                    )::Tuple{
                        MOI.VariableIndex,
                        MOI.ConstraintIndex{MOI.VariableIndex,S},
                    }
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            vindex_optimizer, cindex_optimizer = MOI.add_constrained_variable(
                m.optimizer,
                set,
            )::Tuple{
                MOI.VariableIndex,
                MOI.ConstraintIndex{MOI.VariableIndex,S},
            }
        end
    end
    vindex, cindex = MOI.add_constrained_variable(m.model_cache, set)
    if m.state == ATTACHED_OPTIMIZER
        m.model_to_optimizer_map[vindex] = vindex_optimizer
        m.optimizer_to_model_map[vindex_optimizer] = vindex
        m.model_to_optimizer_map[cindex] = cindex_optimizer
        m.optimizer_to_model_map[cindex_optimizer] = cindex
    end
    return vindex, cindex
end

function _supports_add_constrained_variables(
    m::CachingOptimizer,
    S::Type{<:MOI.AbstractVectorSet},
)
    return MOI.supports_add_constrained_variables(m.model_cache, S) && (
        m.state == NO_OPTIMIZER ||
        MOI.supports_add_constrained_variables(m.optimizer, S)::Bool
    )
end

# Split in two to solve ambiguity
function MOI.supports_add_constrained_variables(
    m::CachingOptimizer,
    ::Type{MOI.Reals},
)
    return _supports_add_constrained_variables(m, MOI.Reals)
end

function MOI.supports_add_constrained_variables(
    m::CachingOptimizer,
    S::Type{<:MOI.AbstractVectorSet},
)
    return _supports_add_constrained_variables(m, S)
end

function MOI.add_constrained_variables(
    m::CachingOptimizer,
    set::S,
) where {S<:MOI.AbstractVectorSet}
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                vindices_optimizer, cindex_optimizer =
                    MOI.add_constrained_variables(
                        m.optimizer,
                        set,
                    )::Tuple{
                        Vector{MOI.VariableIndex},
                        MOI.ConstraintIndex{MOI.VectorOfVariables,S},
                    }
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            vindices_optimizer, cindex_optimizer =
                MOI.add_constrained_variables(
                    m.optimizer,
                    set,
                )::Tuple{
                    Vector{MOI.VariableIndex},
                    MOI.ConstraintIndex{MOI.VectorOfVariables,S},
                }
        end
    end
    vindices, cindex = MOI.add_constrained_variables(m.model_cache, set)
    if m.state == ATTACHED_OPTIMIZER
        for (vindex, vindex_optimizer) in zip(vindices, vindices_optimizer)
            m.model_to_optimizer_map[vindex] = vindex_optimizer
            m.optimizer_to_model_map[vindex_optimizer] = vindex
        end
        m.model_to_optimizer_map[cindex] = cindex_optimizer
        m.optimizer_to_model_map[cindex_optimizer] = cindex
    end
    return vindices, cindex
end

function MOI.supports_constraint(
    m::CachingOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    return MOI.supports_constraint(m.model_cache, F, S) && (
        m.state == NO_OPTIMIZER ||
        MOI.supports_constraint(m.optimizer, F, S)::Bool
    )
end

function MOI.add_constraint(
    m::CachingOptimizer,
    func::F,
    set::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if m.state == ATTACHED_OPTIMIZER
        if m.mode == AUTOMATIC
            try
                cindex_optimizer = MOI.add_constraint(
                    m.optimizer,
                    map_indices(m.model_to_optimizer_map, func),
                    set,
                )::MOI.ConstraintIndex{F,S}
            catch err
                _rethrow_if_not_NotAllowedError(err)
                # It could be MOI.AddConstraintNotAllowed{F', S'} with F' != F
                # or S' != S if, for example, the `F`-in-`S` constraint is bridged
                # to other constraints in `m.optimizer`
                reset_optimizer(m)
            end
        else
            cindex_optimizer = MOI.add_constraint(
                m.optimizer,
                map_indices(m.model_to_optimizer_map, func),
                set,
            )::MOI.ConstraintIndex{F,S}
        end
    end
    cindex = MOI.add_constraint(m.model_cache, func, set)
    if m.state == ATTACHED_OPTIMIZER
        m.model_to_optimizer_map[cindex] = cindex_optimizer
        m.optimizer_to_model_map[cindex_optimizer] = cindex
    end
    return cindex
end

function MOI.modify(
    m::CachingOptimizer,
    cindex::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    if m.state == ATTACHED_OPTIMIZER
        cindex_optimizer = m.model_to_optimizer_map[cindex]
        change_optimizer = map_indices(m.model_to_optimizer_map, change)
        if m.mode == AUTOMATIC
            try
                MOI.modify(m.optimizer, cindex_optimizer, change_optimizer)
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.modify(m.optimizer, cindex_optimizer, change_optimizer)
        end
    end
    MOI.modify(m.model_cache, cindex, change)
    return
end

# This function avoids duplicating code in the MOI.set methods for
# ConstraintSet and ConstraintFunction methods, but allows us to strongly type
# the third and fourth arguments of the set methods so that we only support
# setting the same type of set or function.
function _replace_constraint_function_or_set(
    m::CachingOptimizer,
    attr,
    cindex,
    replacement,
)
    if m.state == ATTACHED_OPTIMIZER
        replacement_optimizer =
            map_indices(m.model_to_optimizer_map, replacement)
        if m.mode == AUTOMATIC
            try
                MOI.set(
                    m.optimizer,
                    attr,
                    m.model_to_optimizer_map[cindex],
                    replacement_optimizer,
                )
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.set(
                m.optimizer,
                attr,
                m.model_to_optimizer_map[cindex],
                replacement_optimizer,
            )
        end
    end
    MOI.set(m.model_cache, attr, cindex, replacement)
    return
end

function MOI.set(
    m::CachingOptimizer,
    ::MOI.ConstraintSet,
    cindex::MOI.ConstraintIndex{F,S},
    set::S,
) where {F,S}
    _replace_constraint_function_or_set(m, MOI.ConstraintSet(), cindex, set)
    return
end

function MOI.set(
    model::CachingOptimizer,
    ::MOI.ConstraintFunction,
    cindex::MOI.ConstraintIndex{F,S},
    func::F,
) where {F,S}
    _replace_constraint_function_or_set(
        model,
        MOI.ConstraintFunction(),
        cindex,
        func,
    )
    return
end

function MOI.set(
    ::CachingOptimizer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex,S},
    ::MOI.VariableIndex,
) where {S}
    return throw(MOI.SettingVariableIndexNotAllowed())
end

function MOI.modify(
    m::CachingOptimizer,
    obj::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if m.state == ATTACHED_OPTIMIZER
        change_optimizer = map_indices(m.model_to_optimizer_map, change)
        if m.mode == AUTOMATIC
            try
                MOI.modify(m.optimizer, obj, change_optimizer)
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.modify(m.optimizer, obj, change_optimizer)
        end
    end
    MOI.modify(m.model_cache, obj, change)
    return
end

function MOI.is_valid(m::CachingOptimizer, index::MOI.Index)
    return MOI.is_valid(m.model_cache, index)
end

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
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
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
    return
end

function MOI.delete(m::CachingOptimizer, indices::Vector{<:MOI.Index})
    if m.state == ATTACHED_OPTIMIZER
        for index in indices
            if !MOI.is_valid(m, index)
                # The index thrown by m.model_cache would be xored
                throw(MOI.InvalidIndex(index))
            end
        end
        indices_optimizer =
            [m.model_to_optimizer_map[index] for index in indices]
        if m.mode == AUTOMATIC
            try
                MOI.delete(m.optimizer, indices_optimizer)
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.delete(m.optimizer, indices_optimizer)
        end
    end
    # The state may have changed in AUTOMATIC mode since reset_optimizer is
    # called in case the deletion is not supported
    if m.state == ATTACHED_OPTIMIZER
        for index in indices
            delete!(m.optimizer_to_model_map, m.model_to_optimizer_map[index])
            delete!(m.model_to_optimizer_map, index)
        end
    end
    MOI.delete(m.model_cache, indices)
    return
end

# TODO: add_constraints, transform

## CachingOptimizer get and set attributes

# Attributes are mapped through `map_indices` (defined in functions.jl) before
# they are sent to the optimizer and when they are returned from the optimizer.
# As a result, values of attributes must implement `map_indices`.

function MOI.set(m::CachingOptimizer, attr::MOI.AbstractModelAttribute, value)
    if m.state == ATTACHED_OPTIMIZER
        optimizer_value = map_indices(m.model_to_optimizer_map, attr, value)
        if m.mode == AUTOMATIC
            try
                MOI.set(m.optimizer, attr, optimizer_value)
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.set(m.optimizer, attr, optimizer_value)
        end
    end
    MOI.set(m.model_cache, attr, value)
    return
end

function MOI.set(
    m::CachingOptimizer,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    index::MOI.Index,
    value,
)
    if m.state == ATTACHED_OPTIMIZER
        optimizer_index = m.model_to_optimizer_map[index]
        optimizer_value = map_indices(m.model_to_optimizer_map, attr, value)
        if m.mode == AUTOMATIC
            try
                MOI.set(m.optimizer, attr, optimizer_index, optimizer_value)
            catch err
                _rethrow_if_not_NotAllowedError(err)
                reset_optimizer(m)
            end
        else
            MOI.set(m.optimizer, attr, optimizer_index, optimizer_value)
        end
    end
    MOI.set(m.model_cache, attr, index, value)
    return
end

function MOI.supports(
    m::CachingOptimizer,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    IndexType::Type{<:MOI.Index},
)
    return MOI.supports(m.model_cache, attr, IndexType) && (
        m.state == NO_OPTIMIZER ||
        MOI.supports(m.optimizer, attr, IndexType)::Bool
    )
end

function MOI.supports(
    m::CachingOptimizer,
    attr::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
)
    return MOI.supports(m.model_cache, attr) &&
           (m.state == NO_OPTIMIZER || MOI.supports(m.optimizer, attr)::Bool)
end

_rethrow_if_not_NotAllowedError(err) = rethrow(err)

_rethrow_if_not_NotAllowedError(::MOI.NotAllowedError) = nothing

function _get_model_attribute(model::CachingOptimizer, attr::MOI.ObjectiveValue)
    try
        return MOI.get(model.optimizer, attr)
    catch err
        _rethrow_if_not_NotAllowedError(err)
        return get_fallback(model, attr)
    end
end

function _get_model_attribute(
    model::CachingOptimizer,
    attr::MOI.DualObjectiveValue,
)
    try
        return MOI.get(model.optimizer, attr)
    catch err
        _rethrow_if_not_NotAllowedError(err)
        MOI.check_result_index_bounds(model, attr)
        # We don't know what coefficient type to use, so just use whatever the
        # objective value type is. This is slightly inefficient, but it
        # shouldn't be a bottleneck.
        obj = _get_model_attribute(model, MOI.ObjectiveValue(attr.result_index))
        return get_fallback(model, attr, typeof(obj))
    end
end

function _get_model_attribute(
    model::CachingOptimizer,
    attr::MOI.AbstractModelAttribute,
)
    return map_indices(
        model.optimizer_to_model_map,
        attr,
        MOI.get(model.optimizer, attr)::MOI.attribute_value_type(attr),
    )
end

function _throw_if_get_attribute_not_allowed(
    model::CachingOptimizer,
    attr;
    needs_optimizer_map::Bool,
)
    # If the state(model) == EMPTY_OPTIMIZER, then
    # `model.model_to_optimizer_map[index]` might be empty (because copy_to
    # has not been called yet), or it might be full, if optimize!(dest, src)
    # did not leave a copy in `dest`.
    missing_map = needs_optimizer_map && isempty(model.model_to_optimizer_map)
    if state(model) == NO_OPTIMIZER || missing_map
        msg =
            "Cannot query $(attr) from `Utilities.CachingOptimizer` " *
            "because no optimizer is attached (the state is `$(state(model))`)."
        throw(MOI.GetAttributeNotAllowed(attr, msg))
    end
    return
end

function MOI.get(model::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    if !MOI.is_set_by_optimize(attr)
        return MOI.get(model.model_cache, attr)
    end
    _throw_if_get_attribute_not_allowed(
        model,
        attr;
        needs_optimizer_map = false,
    )
    return _get_model_attribute(model, attr)
end

function MOI.get(
    model::CachingOptimizer,
    attr::MOI.TerminationStatus,
)::MOI.TerminationStatusCode
    if state(model) == NO_OPTIMIZER
        return MOI.OPTIMIZE_NOT_CALLED
    end
    return MOI.get(model.optimizer, attr)
end

function MOI.get(
    model::CachingOptimizer,
    attr::Union{MOI.PrimalStatus,MOI.DualStatus},
)::MOI.ResultStatusCode
    if state(model) == NO_OPTIMIZER
        return MOI.NO_SOLUTION
    end
    return MOI.get(model.optimizer, attr)
end

_has_fallback(::MOI.AnyAttribute, ::Type{<:MOI.Index}) = false

function MOI.get(
    model::CachingOptimizer,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    index::MOI.Index,
)
    if !MOI.is_set_by_optimize(attr)
        return MOI.get(model.model_cache, attr, index)
    end
    _throw_if_get_attribute_not_allowed(model, attr; needs_optimizer_map = true)
    if _has_fallback(attr, typeof(index))
        return _get_fallback(model, attr, index)
    end
    value = MOI.get(
        model.optimizer,
        attr,
        model.model_to_optimizer_map[index],
    )::MOI.attribute_value_type(attr)
    return map_indices(model.optimizer_to_model_map, attr, value)
end

function MOI.get(
    model::CachingOptimizer,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    indices::Vector{I},
) where {I<:MOI.Index}
    if !MOI.is_set_by_optimize(attr)
        return MOI.get(model.model_cache, attr, indices)
    end
    _throw_if_get_attribute_not_allowed(model, attr; needs_optimizer_map = true)
    if _has_fallback(attr, I)
        return _get_fallback(model, attr, indices)
    end
    value = MOI.get(
        model.optimizer,
        attr,
        map(Base.Fix1(getindex, model.model_to_optimizer_map), indices),
    )::Vector{<:MOI.attribute_value_type(attr)}
    return map_indices(model.optimizer_to_model_map, attr, value)
end

###
### MOI.ConstraintPrimal and MOI.ConstraintDual
###

# `ConstraintPrimal` is slightly unique for CachingOptimizer because if the solver
# doesn't support the attribute directly, we can use the fallback to query the
# function from the cache and the variable value from the optimizer.
# The `ConstraintDual` of a `VariableIndex` or `VectorOfVariables` can
# also be computed from the `ConstraintDual` of the other constraints.

_has_fallback(::MOI.ConstraintPrimal, ::Type{<:MOI.ConstraintIndex}) = true

function _has_fallback(
    ::MOI.ConstraintDual,
    ::Type{<:MOI.ConstraintIndex{F}},
) where {F<:Union{MOI.VariableIndex,MOI.VectorOfVariables}}
    return true
end

function _get_fallback(
    model::CachingOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    index::MOI.ConstraintIndex,
)
    try
        return MOI.get(
            model.optimizer,
            attr,
            model.model_to_optimizer_map[index],
        )
    catch err
        _rethrow_if_not_NotAllowedError(err)
        return get_fallback(model, attr, index)
    end
end

function _get_fallback(
    model::CachingOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    indices::Vector{<:MOI.ConstraintIndex},
)
    try
        return MOI.get(
            model.optimizer,
            attr,
            [model.model_to_optimizer_map[i] for i in indices],
        )
    catch err
        _rethrow_if_not_NotAllowedError(err)
        return [get_fallback(model, attr, i) for i in indices]
    end
end

#####
##### Names
#####

function MOI.supports(
    model::CachingOptimizer,
    attr::Union{MOI.VariableName,MOI.ConstraintName},
    IndexType::Type{<:MOI.Index},
)
    return MOI.supports(model.model_cache, attr, IndexType)
end

function MOI.set(
    model::CachingOptimizer,
    attr::Union{MOI.VariableName,MOI.ConstraintName},
    index::MOI.Index,
    value,
)
    MOI.set(model.model_cache, attr, index, value)
    return
end

function MOI.supports(m::CachingOptimizer, attr::MOI.Name)
    return MOI.supports(m.model_cache, attr)
end

function MOI.set(model::CachingOptimizer, attr::MOI.Name, value)
    MOI.set(model.model_cache, attr, value)
    return
end

function MOI.get(m::CachingOptimizer, IdxT::Type{<:MOI.Index}, name::String)
    return MOI.get(m.model_cache, IdxT, name)
end

function MOI.set(
    model::CachingOptimizer,
    attr::MOI.AbstractOptimizerAttribute,
    value,
)
    optimizer_value = map_indices(model.model_to_optimizer_map, attr, value)
    if model.optimizer !== nothing
        MOI.set(model.optimizer, attr, optimizer_value)
    end
    MOI.set(model.model_cache, attr, value)
    return
end

function MOI.get(model::CachingOptimizer, attr::MOI.AbstractOptimizerAttribute)
    # TODO: Copyable attributes (for example, `Silent`, `TimeLimitSec`,
    # `NumberOfThreads`) should also be stored in the cache so we could
    # return the value stored in the cache instead. However, for
    # non-copyable attributes( for example, `SolverName`) the error is appropriate.
    _throw_if_get_attribute_not_allowed(
        model,
        attr;
        needs_optimizer_map = false,
    )
    return map_indices(
        model.optimizer_to_model_map,
        attr,
        MOI.get(model.optimizer, attr)::MOI.attribute_value_type(attr),
    )
end

# Force users to specify whether the attribute should be queried from the
# model_cache or the optimizer. Maybe we could consider a small whitelist of
# attributes to handle automatically.

# These are expert methods to get or set attributes directly in the model_cache
# or optimizer.

struct AttributeFromModelCache{T<:MOI.AnyAttribute}
    attr::T
end

struct AttributeFromOptimizer{T<:MOI.AnyAttribute}
    attr::T
end

function MOI.get(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
) where {T<:MOI.AbstractModelAttribute}
    return MOI.get(m.model_cache, attr.attr)
end

function MOI.get(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
    idx,
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    return MOI.get(m.model_cache, attr.attr, idx)
end

function MOI.get(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
) where {T<:MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    return map_indices(
        m.optimizer_to_model_map,
        attr.attr,
        MOI.get(m.optimizer, attr.attr)::MOI.attribute_value_type(attr.attr),
    )
end

function MOI.get(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
    idx::MOI.Index,
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    @assert m.state == ATTACHED_OPTIMIZER
    return map_indices(
        m.optimizer_to_model_map,
        attr.attr,
        MOI.get(
            m.optimizer,
            attr.attr,
            m.model_to_optimizer_map[idx],
        )::MOI.attribute_value_type(attr.attr),
    )
end

function MOI.get(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
    idx::Vector{<:MOI.Index},
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    @assert m.state == ATTACHED_OPTIMIZER
    return map_indices(
        m.optimizer_to_model_map,
        attr.attr,
        MOI.get(
            m.optimizer,
            attr.attr,
            map_indices_to_optimizer(m, idx),
        )::Vector{<:MOI.attribute_value_type(attr.attr)},
    )
end

function MOI.set(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
    v,
) where {T<:MOI.AbstractModelAttribute}
    MOI.set(m.model_cache, attr.attr, v)
    return
end

function MOI.set(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
    idx,
    v,
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    MOI.set(m.model_cache, attr.attr, idx, v)
    return
end

function MOI.set(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
    v,
) where {T<:MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    MOI.set(
        m.optimizer,
        attr.attr,
        map_indices(m.model_to_optimizer_map, attr.attr, v),
    )
    return
end

# Map vector of indices into vector of indices or one index into one index
function map_indices_to_optimizer(m::CachingOptimizer, idx::MOI.Index)
    return m.model_to_optimizer_map[idx]
end

function map_indices_to_optimizer(
    m::CachingOptimizer,
    indices::Vector{<:MOI.Index},
)
    return getindex.(Ref(m.model_to_optimizer_map), indices)
end

function MOI.set(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
    idx,
    v,
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    @assert m.state == ATTACHED_OPTIMIZER
    MOI.set(
        m.optimizer,
        attr.attr,
        map_indices_to_optimizer(m, idx),
        map_indices(m.model_to_optimizer_map, attr.attr, v),
    )
    return
end

function MOI.supports(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
) where {T<:MOI.AbstractModelAttribute}
    return MOI.supports(m.model_cache, attr.attr)
end

function MOI.supports(
    m::CachingOptimizer,
    attr::AttributeFromModelCache{T},
    idxtype::Type{<:MOI.Index},
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    return MOI.supports(m.model_cache, attr.attr, idxtype)
end

function MOI.supports(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
) where {T<:MOI.AbstractModelAttribute}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.supports(m.optimizer, attr.attr)::Bool
end

function MOI.supports(
    m::CachingOptimizer,
    attr::AttributeFromOptimizer{T},
    idxtype::Type{<:MOI.Index},
) where {
    T<:Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
}
    @assert m.state == ATTACHED_OPTIMIZER
    return MOI.supports(m.optimizer, attr.attr, idxtype)::Bool
end

function MOI.supports(
    caching_opt::CachingOptimizer,
    sub::MOI.AbstractSubmittable,
)
    return caching_opt.optimizer !== nothing &&
           MOI.supports(caching_opt.optimizer, sub)::Bool
end

function MOI.submit(
    caching_opt::CachingOptimizer,
    sub::MOI.AbstractSubmittable,
    args...,
)
    return MOI.submit(
        caching_opt.optimizer,
        sub,
        map_indices.(Ref(caching_opt.model_to_optimizer_map), args)...,
    )
end

# TODO: get and set methods to look up/set name strings

function MOI.compute_conflict!(model::CachingOptimizer)
    MOI.compute_conflict!(model.optimizer)
    return
end
