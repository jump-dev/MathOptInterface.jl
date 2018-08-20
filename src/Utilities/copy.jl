struct IndexMap
    varmap::Dict{MOI.VariableIndex, MOI.VariableIndex}
    conmap::Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}
end
IndexMap() = IndexMap(Dict{MOI.VariableIndex, MOI.VariableIndex}(),
                 Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}())
Base.getindex(idxmap::IndexMap, vi::MOI.VariableIndex) = idxmap.varmap[vi]
function Base.getindex(idxmap::IndexMap, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    idxmap.conmap[ci]::MOI.ConstraintIndex{F, S}
end

Base.setindex!(idxmap::IndexMap, vi1::MOI.VariableIndex, vi2::MOI.VariableIndex) = Base.setindex!(idxmap.varmap, vi1, vi2)
function Base.setindex!(idxmap::IndexMap, ci1::MOI.ConstraintIndex{F, S}, ci2::MOI.ConstraintIndex{F, S}) where {F, S}
    Base.setindex!(idxmap.conmap, ci1, ci2)
end

Base.delete!(idxmap::IndexMap, vi::MOI.VariableIndex) = delete!(idxmap.varmap, vi)
Base.delete!(idxmap::IndexMap, ci::MOI.ConstraintIndex) = delete!(idxmap.conmap, ci)

Base.keys(idxmap::IndexMap) = Iterators.flatten((keys(idxmap.varmap), keys(idxmap.conmap)))

"""
    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, passattr!::Function=MOI.set!)

Pass the model attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `Name` if `copynames` is `false`.

    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, vis_src::Vector{MOI.VariableIndex}, passattr!::Function=MOI.set!)

Pass the variable attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `VariableName` if `copynames` is `false`.

    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, cis_src::Vector{MOI.ConstraintIndex{F, S}}, passattr!::Function=MOI.set!) where {F, S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `ConstraintName` if `copynames` is `false`.
"""
function passattributes! end

function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, passattr!::Function=MOI.set!)
    # Copy model attributes
    attrs = MOI.get(src, MOI.ListOfModelAttributesSet())
    _passattributes!(dest, src, copynames, idxmap, attrs, tuple(), tuple(), tuple(), passattr!)
end
function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, vis_src::Vector{VI}, passattr!::Function=MOI.set!)
    # Copy variable attributes
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    vis_dest = map(vi -> idxmap[vi], vis_src)
    _passattributes!(dest, src, copynames, idxmap, attrs, (VI,), (vis_src,), (vis_dest,), passattr!)
end
function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, cis_src::Vector{CI{F, S}}, passattr!::Function=MOI.set!) where {F, S}
    # Copy constraint attributes
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
    cis_dest = map(ci -> idxmap[ci], cis_src)
    _passattributes!(dest, src, copynames, idxmap, attrs, (CI{F, S},), (cis_src,), (cis_dest,), passattr!)
end

function _passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, attrs, canargs, getargs, setargs, passattr!::Function=MOI.set!)
    for attr in attrs
        if (copynames || !(attr isa MOI.Name || attr isa MOI.VariableName || attr isa MOI.ConstraintName))
            passattr!(dest, attr, setargs..., attribute_value_map(idxmap, MOI.get(src, attr, getargs...)))
        end
    end
end

"""
    copyconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Copy the constraints of type `F`-in-`S` from the model `src` the model `dest` and fill `idxmap` accordingly. Does not copy `ConstraintName` if `copynames` is `false`.
"""
function copyconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Copy constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        ci_dest = MOI.addconstraint!(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end

    passattributes!(dest, src, copynames, idxmap, cis_src)
end

attribute_value_map(idxmap, f::MOI.AbstractFunction) = mapvariables(idxmap, f)
attribute_value_map(idxmap, attribute_value) = attribute_value
function defaultcopy!(dest::MOI.ModelLike, src::MOI.ModelLike)
    Base.depwarn("defaultcopy!(dest, src) is deprecated, use defaultcopy!(dest, src, true) instead or defaultcopy!(dest, src, false) if you do not want to copy names.", :defaultcopy!)
    defaultcopy!(dest, src, true)
end
function defaultcopy!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool)
    MOI.empty!(dest)

    idxmap = IndexMap()

    # Copy variables
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    for vi in vis_src
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    # Copy variable attributes
    passattributes!(dest, src, copynames, idxmap, vis_src)

    # Copy model attributes
    passattributes!(dest, src, copynames, idxmap)

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        copyconstraints!(dest, src, copynames, idxmap, F, S)
    end

    return idxmap
end

# Allocate-Load Interface: 2-pass copy of a MathOptInterface model
# Some solver wrappers (e.g. SCS, ECOS, SDOI) do not supporting copying an optimization model using `MOI.addconstraints!`, `MOI.addvariables` and `MOI.set!`
# as they first need to figure out some information about a model before being able to pass the problem data to the solver.
#
# During the first pass (called allocate) : the model collects the relevant information about the problem so that
# on the second pass (called load), the constraints can be loaded directly to the solver (in case of SDOI) or written directly into the matrix of constraints (in case of SCS and ECOS).

# To support `MOI.copy!` using this 2-pass mechanism, implement the allocate-load interface defined below and do:
# MOI.copy!(dest::ModelType, src::MOI.ModelLike) = MOIU.allocateload!(dest, src)
# In the implementation of the allocate-load interface, it can be assumed that the different functions will the called in the following order:
# 1) `allocatevariables!`
# 2) `allocate!` and `allocateconstraint!`
# 3) `loadvariables!` and `allocateconstraint!`
# 4) `load!` and `loadconstraint!`
# The interface is not meant to be used to create new constraints with `allocateconstraint!` followed by `loadconstraint!` after a solve, it is only meant for being used in this order to implement `MOI.copy!`.

"""
    needsallocateload(model::MOI.ModelLike)::Bool

Return a `Bool` indicating whether `model` does not support `addvariables!`/`addconstraint!`/`set!` but supports `allocatevariables!`/`allocateconstraint!`/`allocate!`/`loadvariables!`/`loadconstraint!`/`load!`.
That is, the allocate-load interface need to be used to copy an model to `model`.
"""
function needsallocateload end
needsallocateload(::MOI.ModelLike) = false

"""
    allocatevariables!(model::MOI.ModelLike, nvars::Integer)

Creates `nvars` variables and returns a vector of `nvars` variable indices.
"""
function allocatevariables! end

"""
    allocate!(model::ModelLike, attr::ModelLikeAttribute, value)
    allocate!(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    allocate!(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Informs `model` that `load!` will be called with the same arguments after `loadvariables!` is called.
"""
function allocate! end

function allocate!(model::MOI.ModelLike, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector, values::Vector)
    for (index, value) in zip(indices, values)
        allocate!(model, attr, index, value)
    end
end

"""
    allocateconstraint!(model::MOI.ModelLike, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Returns the index for the constraint to be used in `loadconstraint!` that will be called after `loadvariables!` is called.
"""
function allocateconstraint! end

"""
    loadvariables!(model::MOI.ModelLike, nvars::Integer)

Prepares the `model` for `loadobjective!` and `loadconstraint!`.
"""
function loadvariables! end

"""
    load!(model::ModelLike, attr::ModelLikeAttribute, value)
    load!(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    load!(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

This has the same effect that `set!` with the same arguments except that `allocate!` should be called first before `loadvariables!`.
"""
function load! end

function load!(model::MOI.ModelLike, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector, values::Vector)
    for (index, value) in zip(indices, values)
        load!(model, attr, index, value)
    end
end

"""
    loadconstraint!(model::MOI.ModelLike, ci::MOI.ConstraintIndex, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Sets the constraint function and set for the constraint of index `ci`.
"""
function loadconstraint! end

function allocateconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Allocate constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())

    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        ci_dest = allocateconstraint!(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end

    return passattributes!(dest, src, copynames, idxmap, cis_src, allocate!)
end

function loadconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Load constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        loadconstraint!(dest, ci_dest, f_dest, s)
    end

    return passattributes!(dest, src, copynames, idxmap, cis_src, load!)
end

"""
    allocateload!(dest::MOI.ModelLike, src::MOI.ModelLike)

Implements `MOI.copy!(dest, src)` using the allocate-load interface.
"""
function allocateload!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool)
    MOI.empty!(dest)

    idxmap = IndexMap()

    # Allocate variables
    nvars = MOI.get(src, MOI.NumberOfVariables())
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    vis_dest = allocatevariables!(dest, nvars)
    for (var_src, var_dest) in zip(vis_src, vis_dest)
        idxmap.varmap[var_src] = var_dest
    end

    # Allocate variable attributes
    passattributes!(dest, src, copynames, idxmap, vis_src, allocate!)

    # Allocate model attributes
    passattributes!(dest, src, copynames, idxmap, allocate!)

    # Allocate constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        allocateconstraints!(dest, src, copynames, idxmap, F, S)
    end

    # Load variables
    loadvariables!(dest, nvars)

    # Load variable attributes
    passattributes!(dest, src, copynames, idxmap, vis_src, load!)

    # Load model attributes
    passattributes!(dest, src, copynames, idxmap, load!)

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        loadconstraints!(dest, src, copynames, idxmap, F, S)
    end

    return idxmap
end
