"""
    Map <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Mapping between bridged variables and the bridge that bridged the variable.
"""
mutable struct Map <: AbstractDict{MOI.VariableIndex,AbstractBridge}
    # Bridged constrained variables
    # `i` ->  `0`: `VariableIndex(-i)` was added with `add_constrained_variable`.
    # `i` -> `-j`: `VariableIndex(-i)` was the first variable of
    #              `add_constrained_variables` with a set of dimension `j`.
    # `i` ->  `j`: `VariableIndex(-i)` was the `j`th  variable of
    #             ` add_constrained_variables`.
    info::Vector{Int64}
    # `i` ->  `-1`: `VariableIndex(-i)` was deleted.
    # `i` ->  `0`: `VariableIndex(-i)` was added with `add_constrained_variable`.
    # `i` ->  `j`: `VariableIndex(-i)` is the `j`th  variable of a constrained
    #               vector of variables, taking deletion into account.
    index_in_vector::Vector{Int64}
    # `i` -> `bridge`: `VariableIndex(-i)` was bridged by `bridge`.
    bridges::Vector{Union{Nothing,AbstractBridge}}
    sets::Vector{Union{Nothing,Type}}
    # If `nothing`, it cannot be computed because some bridges does not support it
    unbridged_function::Union{
        Nothing,
        Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}},
    }
    # Bridge that created this bridge, 0 if it is no bridge.
    parent_index::Vector{Int64}
    # Current bridge, 0 otherwise.
    current_context::Int64
    # Context of constraint bridged by constraint bridges
    constraint_context::Dict{MOI.ConstraintIndex,Int64}
end

function Map()
    return Map(
        Int64[],
        Int64[],
        Union{Nothing,AbstractBridge}[],
        Union{Nothing,Type}[],
        Dict{MOI.VariableIndex,MOI.AbstractScalarFunction}(),
        Int64[],
        0,
        Dict{MOI.ConstraintIndex,Int64}(),
    )
end

# Implementation of `AbstractDict` interface.

Base.isempty(map::Map) = all(bridge -> bridge === nothing, map.bridges)

function Base.empty!(map::Map)
    empty!(map.info)
    empty!(map.index_in_vector)
    empty!(map.bridges)
    empty!(map.sets)
    if map.unbridged_function === nothing
        map.unbridged_function =
            Dict{MOI.VariableIndex,Tuple{Int64,MOI.AbstractScalarFunction}}()
    else
        empty!(map.unbridged_function)
    end
    empty!(map.parent_index)
    map.current_context = 0
    empty!(map.constraint_context)
    return map
end

function bridge_index(map::Map, vi::MOI.VariableIndex)
    index = map.info[-vi.value]
    if index ≤ 0
        return -vi.value
    else
        return -vi.value - index + 1
    end
end

function Base.haskey(map::Map, vi::MOI.VariableIndex)
    return -length(map.bridges) ≤ vi.value ≤ -1 &&
           map.bridges[bridge_index(map, vi)] !== nothing &&
           map.index_in_vector[-vi.value] != -1
end

function Base.getindex(map::Map, vi::MOI.VariableIndex)
    return map.bridges[bridge_index(map, vi)]
end

function Base.delete!(map::Map, vi::MOI.VariableIndex)
    if iszero(map.info[-vi.value])
        # Delete scalar variable
        map.bridges[bridge_index(map, vi)] = nothing
        map.sets[bridge_index(map, vi)] = nothing
    elseif has_keys(map, [vi])
        # Delete whole vector
        delete!(map, [vi])
    else
        # Delete variable in vector and resize vector
        map.info[bridge_index(map, vi)] += 1
        for i in (-vi.value):length(map.index_in_vector)
            if map.index_in_vector[i] == -1
                continue
            elseif bridge_index(map, vi) !=
                   bridge_index(map, MOI.VariableIndex(-i))
                break
            end
            map.index_in_vector[i] -= 1
        end
    end
    map.index_in_vector[-vi.value] = -1
    return map
end

function Base.delete!(map::Map, vis::Vector{MOI.VariableIndex})
    if !has_keys(map, vis)
        throw(
            ArgumentError(
                "`$vis` is not a valid key vector as returned by `add_keys_for_bridge`.",
            ),
        )
    end
    for vi in vis
        map.index_in_vector[-vi.value] = -1
    end
    map.bridges[bridge_index(map, first(vis))] = nothing
    map.sets[bridge_index(map, first(vis))] = nothing
    return map
end

function Base.keys(map::Map)
    return Base.Iterators.Filter(
        vi -> haskey(map, vi),
        MOIU.LazyMap{MOI.VariableIndex}(
            i -> MOI.VariableIndex(-i),
            eachindex(map.bridges),
        ),
    )
end

Base.length(map::Map) = count(bridge -> bridge !== nothing, map.bridges)

function number_of_variables(map::Map)
    num = 0
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            if iszero(map.info[i])
                num += 1
            else
                num += length_of_vector_of_variables(map, MOI.VariableIndex(-i))
            end
        end
        count(bridge -> bridge !== nothing, map.bridges)
    end
    return num
end

function Base.values(map::Map)
    # We don't use `filter` as it would compute the resulting array which
    # is not necessary if the caller just wants to iterater over `values`.
    return Base.Iterators.Filter(bridge -> bridge !== nothing, map.bridges)
end

function Base.iterate(map::Map, state = 1)
    while state ≤ length(map.bridges) && map.bridges[state] === nothing
        state += 1
    end
    if state > length(map.bridges)
        return nothing
    else
        return MOI.VariableIndex(-state) => map.bridges[state], state + 1
    end
end

# Custom interface for information needed by `AbstractBridgeOptimizer`s that is
# not part of the `AbstractDict` interface.

"""
    constrained_set(map::Map, vi::MOI.VariableIndex)

Return the set type in which the bridged variable `vi` was added when it was
bridged.
"""
function constrained_set(map::Map, vi::MOI.VariableIndex)
    return map.sets[bridge_index(map, vi)]
end

"""
    number_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the number of bridged variables in `S`. Note that if `S` is a vector set,
bridging a vector of `n` variables only counts as 1.
"""
function number_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    return count(isequal(S), map.sets)
end

function constraint(map::Map, vi::MOI.VariableIndex)
    S = constrained_set(map, vi)
    F = MOIU.variable_function_type(S)
    return MOI.ConstraintIndex{F,S}(-bridge_index(map, vi))
end

"""
    constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})

Return the list of constraints corresponding to bridged variables in `S`.
"""
function constraints_with_set(map::Map, S::Type{<:MOI.AbstractSet})
    F = MOIU.variable_function_type(S)
    return [
        MOI.ConstraintIndex{F,S}(-i) for
        i in eachindex(map.sets) if map.sets[i] == S
    ]
end

"""
    list_of_constraint_types(map::Map)

Return a list of all the different types `(F, S)` of `F`-in-`S` constraints in
`map`.
"""
function list_of_constraint_types(map::Map)
    list = Set{Tuple{Type,Type}}()
    for i in eachindex(map.bridges)
        if map.bridges[i] !== nothing
            S = map.sets[i]
            if S != MOI.Reals
                F = MOIU.variable_function_type(S)
                push!(list, (F, S))
            end
        end
    end
    return list
end

"""
    has_keys(map::Map, vis::Vector{MOI.VariableIndex})::Bool

Return a `Bool` indicating whether `vis` was returned by
[`add_keys_for_bridge`](@ref) and has not been deleted yet.
"""
function has_keys(map::Map, vis::Vector{MOI.VariableIndex})
    return isempty(vis) || (
        length_of_vector_of_variables(map, first(vis)) == length(vis) &&
        all(
            vi -> bridge_index(map, vi) == bridge_index(map, first(vis)),
            vis,
        ) &&
        all(vi -> haskey(map, vi), vis) &&
        all(i -> vis[i].value < vis[i-1].value, 2:length(vis))
    )
end

"""
    length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)

If `vi` was bridged in a scalar set, it returns 0. Otherwise, it
returns the dimension of the set.
"""
function length_of_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    return -map.info[bridge_index(map, vi)]
end

"""
    index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)::MOIB.IndexInVector

Return the index of `vi` in the vector of variables in which it was bridged.
"""
function index_in_vector_of_variables(map::Map, vi::MOI.VariableIndex)
    return MOIB.IndexInVector(map.index_in_vector[-vi.value])
end

"""
    has_bridges(map::Map)::Bool

Return a `Bool` indicating whether any bridge was added yet. Note that it
returns `false` even if all bridges were deleted while `isempty` would return
`true`. It is computed in `O(1)` while `isempty` needs `O(n)` hence it is used
by [`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref) to shortcut
operations in case variable bridges are not used.
"""
has_bridges(map::Map) = !isempty(map.info)

"""
    add_key_for_bridge(map::Map, bridge_fun::Function,
                       set::MOI.AbstractScalarSet)

Create a new variable index `vi`, store the mapping `vi => bridge` and
associate `vi` to `typeof(set)`. It returns a tuple with `vi` and the
constraint index
`MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(vi.value)`.
"""
function add_key_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::MOI.AbstractScalarSet,
)
    push!(map.parent_index, map.current_context)
    bridge_index = Int64(length(map.parent_index))
    push!(map.info, 0)
    push!(map.index_in_vector, 0)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    map.bridges[bridge_index] = call_in_context(map, bridge_index, bridge_fun)
    index = -bridge_index
    variable = MOI.VariableIndex(index)
    if map.unbridged_function !== nothing
        mappings = unbridged_map(map.bridges[bridge_index], variable)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    map.unbridged_function,
                    mapping.first => (bridge_index, mapping.second),
                )
            end
        end
    end
    return variable, MOI.ConstraintIndex{MOI.SingleVariable,typeof(set)}(index)
end

"""
    add_keys_for_bridge(map::Map, bridge_fun::Function,
                        set::MOI.AbstractVectorSet)

Create vector of variable indices `variables`, stores the mapping
`vi => bridge` for each `vi ∈ variables` and associate `variables` to
`typeof(set)`. It returns a tuple with `variables` and the constraint index
`MOI.ConstraintIndex{MOI.VectorOfVariables, typeof(set)}(first(variables).value)`.
"""
function add_keys_for_bridge(
    map::Map,
    bridge_fun::Function,
    set::MOI.AbstractVectorSet,
)
    if iszero(MOI.dimension(set))
        return MOI.VariableIndex[],
        MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(0)
    end
    push!(map.parent_index, map.current_context)
    bridge_index = Int64(length(map.parent_index))
    push!(map.info, -MOI.dimension(set))
    push!(map.index_in_vector, 1)
    push!(map.bridges, nothing)
    push!(map.sets, typeof(set))
    for i in 2:MOI.dimension(set)
        push!(map.parent_index, 0)
        push!(map.info, i)
        push!(map.index_in_vector, i)
        push!(map.bridges, nothing)
        push!(map.sets, nothing)
    end
    map.bridges[bridge_index] = call_in_context(map, bridge_index, bridge_fun)
    variables = MOI.VariableIndex[
        MOI.VariableIndex(-(bridge_index - 1 + i)) for
        i in 1:MOI.dimension(set)
    ]
    if map.unbridged_function !== nothing
        mappings = unbridged_map(map.bridges[bridge_index], variables)
        if mappings === nothing
            map.unbridged_function = nothing
        else
            for mapping in mappings
                push!(
                    map.unbridged_function,
                    mapping.first => (bridge_index, mapping.second),
                )
            end
        end
    end
    index = first(variables).value
    return variables,
    MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)}(index)
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable})

Return `MOI.SingleVariable(vi)` where `vi` is the bridged variable
corresponding to `ci`.
"""
function function_for(::Map, ci::MOI.ConstraintIndex{MOI.SingleVariable})
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end

"""
    function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})

Return `MOI.VectorOfVariables(vis)` where `vis` is the vector of bridged
variables corresponding to `ci`.
"""
function function_for(map::Map, ci::MOI.ConstraintIndex{MOI.VectorOfVariables})
    variables = MOI.VariableIndex[]
    for i in ci.value:-1:-length(map.bridges)
        vi = MOI.VariableIndex(i)
        if map.index_in_vector[-vi.value] == -1
            continue
        elseif bridge_index(map, vi) == -ci.value
            push!(variables, vi)
        else
            break
        end
    end
    return MOI.VectorOfVariables(variables)
end

"""
    throw_if_cannot_unbridge(map::Map)

Throw an error if some bridged variables do not have any reverse mapping.
"""
function throw_if_cannot_unbridge(map::Map)
    if map.unbridged_function === nothing
        error(
            "Cannot unbridge function because some variables are bridged by",
            " variable bridges that do not support reverse mapping, e.g.,",
            " `ZerosBridge`.",
        )
    end
end

"""
    unbridged_function(map::Map, vi::MOI.VariableIndex)

Return the expression of `vi` in terms of bridged variables.
"""
function unbridged_function(map::Map, vi::MOI.VariableIndex)
    throw_if_cannot_unbridge(map)
    context_func = get(map.unbridged_function, vi, nothing)
    if context_func === nothing
        return nothing
    end
    bridge_index, func = context_func
    # If the bridge bridging `vi` has index `bridge_index` or directly or
    # indirectly created this bridge then we don't unbridge the variable.
    context = map.current_context
    while !iszero(context)
        if bridge_index == context
            return nothing
        end
        context = map.parent_index[context]
    end
    return func
end

"""
    call_in_context(map::Map, bridge_index::Int64, f::Function)

Call function `f` in the context of the variable bridge of index `bridge_index`.
That is, the variable indices bridged by this bridge or the bridges that
created it will not be unbridged in [`unbridged_function`](@ref).
"""
function call_in_context(map::Map, bridge_index::Int64, f::Function)
    if iszero(bridge_index)
        return f()
    end
    previous_context = map.current_context
    map.current_context = bridge_index
    output = nothing
    try
        output = f()
    finally
        map.current_context = previous_context
    end
    return output
end

"""
    call_in_context(map::Map, vi::MOI.VariableIndex, f::Function)

Shortcut for `call_in_context(map, bridge_index, () -> f(bridge))` where
`vi` is bridged by `bridge` with index `bridge_index`.
"""
function call_in_context(map::Map, vi::MOI.VariableIndex, f::Function)
    idx = bridge_index(map, vi)
    return call_in_context(map, idx, () -> f(map.bridges[idx]))
end

"""
    call_in_context(map::Map, ci::MOI.ConstraintIndex, f::Function)

Shortcut for `call_in_context(map, bridge_index, f)` where `bridge_index` is the
variable bridge that created `ci` (directly or indirectly) or 0 otherwise.
"""
function call_in_context(map::Map, ci::MOI.ConstraintIndex, f::Function)
    return call_in_context(map, get(map.constraint_context, ci, Int64(0)), f)
end

"""
    register_context(map::Map, ci::MOI.ConstraintIndex)

Register the current context as the variable bridge that created `ci` (directly
or indirectly) or 0 otherwise.
"""
function register_context(map::Map, ci::MOI.ConstraintIndex)
    if !iszero(map.current_context)
        # By only storing non-zero values, we avoid any dictionary access for
        # constraint not created (directly or indirectly) by variable bridges.
        # This ensures that there is no performance hit of the bridge layer when
        # no variable bridge is used.
        map.constraint_context[ci] = map.current_context
    end
    return
end

"""
    EmptyMap <: AbstractDict{MOI.VariableIndex, AbstractBridge}

Empty version of [`Map`](@ref). It is used by
[`MathOptInterface.Bridges.Constraint.SingleBridgeOptimizer`](@ref) as it does
not bridge any variable.
"""
struct EmptyMap <: AbstractDict{MOI.VariableIndex,AbstractBridge} end

Base.isempty(::EmptyMap) = true

function Base.empty!(::EmptyMap) end

Base.length(::EmptyMap) = 0

Base.keys(::EmptyMap) = MOIU.EmptyVector{MOI.VariableIndex}()

Base.values(::EmptyMap) = MOIU.EmptyVector{AbstractBridge}()

has_bridges(::EmptyMap) = false

number_of_variables(::EmptyMap) = 0

number_with_set(::EmptyMap, ::Type{<:MOI.AbstractSet}) = 0

function constraints_with_set(::EmptyMap, S::Type{<:MOI.AbstractSet})
    return MOI.ConstraintIndex{MOIU.variable_function_type(S),S}[]
end

register_context(::EmptyMap, ::MOI.ConstraintIndex) = nothing

call_in_context(::EmptyMap, ::MOI.ConstraintIndex, f::Function) = f()
