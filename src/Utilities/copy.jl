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
    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!)

Pass the model attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `Name` if `copynames` is `false`.

    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, vis_src::Vector{MOI.VariableIndex}, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!)

Pass the variable attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `VariableName` if `copynames` is `false`.

    passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, cis_src::Vector{MOI.ConstraintIndex{F, S}}, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!) where {F, S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `passattr!` to pass the attribute. Does not copy `ConstraintName` if `copynames` is `false`.
"""
function passattributes! end

function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!)
    # Copy model attributes
    @assert MOI.canget(src, MOI.ListOfModelAttributesSet())
    attrs = MOI.get(src, MOI.ListOfModelAttributesSet())
    _passattributes!(dest, src, copynames, idxmap, attrs, tuple(), tuple(), tuple(), canpassattr, passattr!)
end
function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, vis_src::Vector{VI}, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!)
    # Copy variable attributes
    @assert MOI.canget(src, MOI.ListOfVariableAttributesSet())
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    vis_dest = map(vi -> idxmap[vi], vis_src)
    _passattributes!(dest, src, copynames, idxmap, attrs, (VI,), (vis_src,), (vis_dest,), canpassattr, passattr!)
end
function passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, cis_src::Vector{CI{F, S}}, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!) where {F, S}
    # Copy constraint attributes
    @assert MOI.canget(src, MOI.ListOfConstraintAttributesSet{F, S}())
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
    cis_dest = map(ci -> idxmap[ci], cis_src)
    _passattributes!(dest, src, copynames, idxmap, attrs, (CI{F, S},), (cis_src,), (cis_dest,), canpassattr, passattr!)
end

function _passattributes!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, attrs, canargs, getargs, setargs, canpassattr::Function=MOI.canset, passattr!::Function=MOI.set!)
    for attr in attrs
        if (copynames || !(attr isa MOI.Name || attr isa MOI.VariableName || attr isa MOI.ConstraintName)) && MOI.canget(src, attr, canargs...)
            if !canpassattr(dest, attr, canargs...)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            passattr!(dest, attr, setargs..., attribute_value_map(idxmap, MOI.get(src, attr, getargs...)))
        end
    end
    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
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
        if MOI.canaddconstraint(dest, typeof(f_dest), typeof(s))
            ci_dest = MOI.addconstraint!(dest, f_dest, s)
            idxmap.conmap[ci_src] = ci_dest
        else
            return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
        end
    end

    return passattributes!(dest, src, copynames, idxmap, cis_src)
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
    if !MOI.canaddvariable(dest)
        return MOI.CopyResult(MOI.CopyOtherError, "Adding variables is not supported", idxmap)
    end
    for vi in vis_src
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    # Copy variable attributes
    res = passattributes!(dest, src, copynames, idxmap, vis_src)
    res.status == MOI.CopySuccess || return res

    # Copy model attributes
    res = passattributes!(dest, src, copynames, idxmap)
    res.status == MOI.CopySuccess || return res

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = copyconstraints!(dest, src, copynames, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
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

"""
    canallocate(model::ModelLike, attr::ModelLikeAttribute)::Bool
    canallocate(model::ModelLike, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool
    canallocate(model::ModelLike, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool

Return a `Bool` indicating whether it is possible to allocate attribute `attr` applied to the index type `R` in the model `model`.
"""
function canallocate end
canallocate(::MOI.ModelLike, ::MOI.AnyAttribute) = false
canallocate(::MOI.ModelLike, ::MOI.AnyAttribute, ::Type{<:MOI.Index}) = false

"""
    allocateconstraint!(model::MOI.ModelLike, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Returns the index for the constraint to be used in `loadconstraint!` that will be called after `loadvariables!` is called.
"""
function allocateconstraint! end

"""
    canallocateconstraint(model::ModelLike, F::Type{<:AbstractFunction}, S::Type{<:AbstractSet})::Bool

Return a `Bool` indicating whether it is possible to allocate a constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is of type `F`, and ``\\mathcal{S}`` is of type `S`.
"""
canallocateconstraint(model::MOI.ModelLike, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

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

"""
    canload(model::ModelLike, attr::ModelLikeAttribute)::Bool
    canload(model::ModelLike, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool
    canload(model::ModelLike, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool

Return a `Bool` indicating whether it is possible to load attribute `attr` applied to the index type `R` in the model `model`.
"""
function canload end
canload(::MOI.ModelLike, ::MOI.AnyAttribute) = false
canload(::MOI.ModelLike, ::MOI.AnyAttribute, ::Type{<:MOI.Index}) = false

"""
    loadconstraint!(model::MOI.ModelLike, ci::MOI.ConstraintIndex, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Sets the constraint function and set for the constraint of index `ci`.
"""
function loadconstraint! end

"""
    canloadconstraint(model::ModelLike, F::Type{<:AbstractFunction}, S::Type{<:AbstractSet})::Bool

Return a `Bool` indicating whether it is possible to load a constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is of type `F`, and ``\\mathcal{S}`` is of type `S`.
"""
canloadconstraint(model::MOI.ModelLike, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

function allocateconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Allocate constraints
    if !canallocateconstraint(dest, F, S)
        return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
    end
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        ci_dest = allocateconstraint!(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end

    return passattributes!(dest, src, copynames, idxmap, cis_src, canallocate, allocate!)
end

function loadconstraints!(dest::MOI.ModelLike, src::MOI.ModelLike, copynames::Bool, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Load constraints
    if !canloadconstraint(dest, F, S)
        return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
    end
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        loadconstraint!(dest, ci_dest, f_dest, s)
    end

    return passattributes!(dest, src, copynames, idxmap, cis_src, canload, load!)
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
    res = passattributes!(dest, src, copynames, idxmap, vis_src, canallocate, allocate!)
    res.status == MOI.CopySuccess || return res

    # Allocate model attributes
    res = passattributes!(dest, src, copynames, idxmap, canallocate, allocate!)
    res.status == MOI.CopySuccess || return res

    # Allocate constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = allocateconstraints!(dest, src, copynames, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    # Load variables
    loadvariables!(dest, nvars)

    # Load variable attributes
    res = passattributes!(dest, src, copynames, idxmap, vis_src, canload, load!)
    res.status == MOI.CopySuccess || return res

    # Load model attributes
    res = passattributes!(dest, src, copynames, idxmap, canload, load!)
    res.status == MOI.CopySuccess || return res

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = loadconstraints!(dest, src, copynames, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end
