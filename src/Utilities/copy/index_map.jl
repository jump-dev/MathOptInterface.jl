# IndexMap is defined here because there is a boostrapping problem.
#  * IndexMap requires `Utilities.CleverDicts` and `Utilities.DoubleDicts`, so
#    if it were to be defined in MOI proper, it must be included after
#    Utilities.
#  * However, Utilities requires IndexMap, so it must be defined before
#    Utilities.jl is included.
# To work around this issue, we define `IndexMap` here.

struct IndexMap <: AbstractDict{MOI.Index,MOI.Index}
    var_map::CleverDicts.CleverDict{
        MOI.VariableIndex,
        MOI.VariableIndex,
        typeof(CleverDicts.key_to_index),
        typeof(CleverDicts.index_to_key),
    }
    con_map::DoubleDicts.IndexDoubleDict
end

"""
    IndexMap(number_of_variables::Int = 0)

The dictionary-like object returned by [`MathOptInterface.copy_to`](@ref).

If known in advance, pass `number_of_variables` to preallocate the necessary
space for the variables.
"""
function IndexMap(number_of_variables::Int = 0)
    var_map = CleverDicts.CleverDict{MOI.VariableIndex,MOI.VariableIndex}(
        CleverDicts.key_to_index,
        CleverDicts.index_to_key,
        number_of_variables,
    )
    con_map = DoubleDicts.IndexDoubleDict()
    return IndexMap(var_map, con_map)
end

"""
    _index_map_for_variable_indices(variables)

This function does not add variables to the IndexMap.
It simply initializes the IndexMap with a proper data struture.
If the variable indices are contiguous and start from 1, then
an optimized data structure with pre allocated memory is initialized.
Otherwise the data structure will start empty and will try to
keep using performant structure for as long as possible.
"""
function _index_map_for_variable_indices(variables)
    n = length(variables)
    if all(i -> variables[i] == MOI.VariableIndex(i), 1:n)
        return IndexMap(n)
    else
        return IndexMap()
    end
end

function _identity_constraints_map(
    model,
    map::MOIU.DoubleDicts.IndexDoubleDictInner{F,S},
) where {F,S}
    for c in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        map[c] = c
    end
    return
end

"""
    identity_index_map(model::MOI.ModelLike)

Return an [`IndexMap`](@ref) that maps all variable and constraint indices of
`model` to themselves.
"""
function identity_index_map(model::MOI.ModelLike)
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    map = _index_map_for_variable_indices(variables)
    for x in variables
        map[x] = x
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        _identity_constraints_map(model, map.con_map[F, S])
    end
    return map
end

Base.getindex(map::IndexMap, key::MOI.VariableIndex) = map.var_map[key]

function Base.getindex(map::IndexMap, key::MOI.ConstraintIndex{F,S}) where {F,S}
    return map.con_map[key]::MOI.ConstraintIndex{F,S}
end

function Base.setindex!(
    map::IndexMap,
    value::MOI.VariableIndex,
    key::MOI.VariableIndex,
)
    return map.var_map[key] = value
end

function Base.setindex!(
    map::IndexMap,
    value::MOI.ConstraintIndex{F,S},
    key::MOI.ConstraintIndex{F,S},
) where {F,S}
    return map.con_map[key] = value
end

Base.delete!(map::IndexMap, x::MOI.VariableIndex) = delete!(map.var_map, x)

Base.delete!(map::IndexMap, c::MOI.ConstraintIndex) = delete!(map.con_map, c)

Base.haskey(map::IndexMap, c::MOI.ConstraintIndex) = haskey(map.con_map, c)

Base.haskey(map::IndexMap, x::MOI.VariableIndex) = haskey(map.var_map, x)

function Base.keys(map::IndexMap)
    return Iterators.flatten((keys(map.var_map), keys(map.con_map)))
end

Base.length(map::IndexMap) = length(map.var_map) + length(map.con_map)

function Base.iterate(map::IndexMap, args...)
    return iterate(Base.Iterators.flatten((map.var_map, map.con_map)), args...)
end
