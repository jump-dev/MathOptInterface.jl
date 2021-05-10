# This file contains default implementations for the `MOI.copy_to` function that can be used by a model.

"""
    automatic_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike;
                      copy_names::Bool=true,
                      filter_constraints::Union{Nothing, Function}=nothing)

Use [`MathOptInterface.supports_incremental_interface`](@ref) and
[`Utilities.supports_allocate_load`](@ref) to automatically choose between
[`Utilities.default_copy_to`](@ref) or [`Utilities.allocate_load`](@ref) to
apply the copy operation.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function automatic_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike;
    copy_names::Bool = true,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    if MOI.supports_incremental_interface(dest, copy_names)
        default_copy_to(dest, src, copy_names, filter_constraints)
    elseif supports_allocate_load(dest, copy_names)
        allocate_load(dest, src, copy_names, filter_constraints)
    else
        error(
            "Model $(typeof(dest)) does not support copy",
            copy_names ? " with names" : "",
            ".",
        )
    end
end

@deprecate supports_default_copy_to MOI.supports_incremental_interface

"""
    _index_to_variable(i::Int)

Simply returns `MOI.VariableIndex(i)`. This is necessary to pass a function that
creates the `VariableIndex` from an integer. If we pass `MOI.VariableIndex`
julia understands is as a `DataType` and not a function, this leads to type
instability issues.
"""
_index_to_variable(i) = MOI.VariableIndex(i)
const DenseVariableDict{V} = CleverDicts.CleverDict{
    MOI.VariableIndex,
    V,
    typeof(CleverDicts.key_to_index),
    typeof(CleverDicts.index_to_key),
}
function dense_variable_dict(::Type{V}, n) where {V}
    return CleverDicts.CleverDict{MOI.VariableIndex,V}(
        MOI.index_value,
        _index_to_variable,
        n,
    )
end

struct IndexMap <: AbstractDict{MOI.Index,MOI.Index}
    varmap::DenseVariableDict{MOI.VariableIndex}
    conmap::DoubleDicts.MainIndexDoubleDict
end

function IndexMap(n = 0)
    return IndexMap(
        dense_variable_dict(MOI.VariableIndex, n),
        DoubleDicts.IndexDoubleDict(),
    )
end
function _identity_constraints_map(
    index_map::MOIU.DoubleDicts.IndexWithType{F,S},
    model,
) where {F,S}
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        index_map[ci] = ci
    end
end
function identity_index_map(model::MOI.ModelLike)
    vis = MOI.get(model, MOI.ListOfVariableIndices())
    index_map = IndexMap(length(vis))
    for vi in vis
        index_map[vi] = vi
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        _identity_constraints_map(index_map.conmap[F, S], model)
    end
    return index_map
end

"""
    index_map_for_variable_indices(variables)

This function does not add variables to the IndexMap.
It simply initializes the IndexMap with a proper data struture.
If the variable indices are contiguous and start from 1, then
an optimized data structure with pre allocated memory is initialized.
Otherwise the data structure will start empty and will try to
keep using performant structure for as long as possible.
"""
function index_map_for_variable_indices(variables)
    n = length(variables)
    if all(i -> variables[i] == MOI.VariableIndex(i), 1:n)
        return IndexMap(n)
    else
        return IndexMap()
    end
end

Base.getindex(idxmap::IndexMap, vi::MOI.VariableIndex) = idxmap.varmap[vi]
function Base.getindex(
    idxmap::IndexMap,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    return idxmap.conmap[ci]::MOI.ConstraintIndex{F,S}
end

function Base.setindex!(
    idxmap::IndexMap,
    vi1::MOI.VariableIndex,
    vi2::MOI.VariableIndex,
)
    return Base.setindex!(idxmap.varmap, vi1, vi2)
end
function Base.setindex!(
    idxmap::IndexMap,
    ci1::MOI.ConstraintIndex{F,S},
    ci2::MOI.ConstraintIndex{F,S},
) where {F,S}
    return Base.setindex!(idxmap.conmap, ci1, ci2)
end

function Base.delete!(idxmap::IndexMap, vi::MOI.VariableIndex)
    return delete!(idxmap.varmap, vi)
end
function Base.delete!(idxmap::IndexMap, ci::MOI.ConstraintIndex)
    return delete!(idxmap.conmap, ci)
end

function Base.haskey(idxmap::IndexMap, ci::MOI.ConstraintIndex)
    return haskey(idxmap.conmap, ci)
end
Base.haskey(idxmap::IndexMap, vi::MOI.VariableIndex) = haskey(idxmap.varmap, vi)

function Base.keys(idxmap::IndexMap)
    return Iterators.flatten((keys(idxmap.varmap), keys(idxmap.conmap)))
end

Base.length(idxmap::IndexMap) = length(idxmap.varmap) + length(idxmap.conmap)
function Base.iterate(idxmap::IndexMap, args...)
    return iterate(
        Base.Iterators.flatten((idxmap.varmap, idxmap.conmap)),
        args...,
    )
end

"""
    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, pass_attr::Function=MOI.set)

Pass the model attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `Name` if `copy_names` is `false`.

    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, vis_src::Vector{MOI.VariableIndex}, pass_attr::Function=MOI.set)

Pass the variable attributes from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `VariableName` if `copy_names` is `false`.

    pass_attributes(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool, idxmap::IndexMap, cis_src::Vector{MOI.ConstraintIndex{F, S}}, pass_attr::Function=MOI.set) where {F, S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to the model `dest` using `canpassattr` to check if the attribute can be passed and `pass_attr` to pass the attribute. Does not copy `ConstraintName` if `copy_names` is `false`.
"""
function pass_attributes end

function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    idxmap::IndexMap,
    pass_attr::Function = MOI.set,
)
    # Copy model attributes
    attrs = MOI.get(src, MOI.ListOfModelAttributesSet())
    if !copy_names
        attrs = filter(attr -> !(attr isa MOI.Name), attrs)
    end
    return _pass_attributes(
        dest,
        src,
        idxmap,
        attrs,
        tuple(),
        tuple(),
        tuple(),
        pass_attr,
    )
end
function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    idxmap::IndexMap,
    vis_src::Vector{VI},
    pass_attr::Function = MOI.set,
)
    # Copy variable attributes
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    if !copy_names
        attrs = filter(attr -> !(attr isa MOI.VariableName), attrs)
    end
    if !isempty(attrs) # If `attrs` is empty, we can spare the computation of `vis_dest`
        vis_dest = map(vi -> idxmap[vi], vis_src)
        _pass_attributes(
            dest,
            src,
            idxmap,
            attrs,
            (VI,),
            (vis_src,),
            (vis_dest,),
            pass_attr,
        )
    end
end
function pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    idxmap::IndexMap,
    cis_src::Vector{CI{F,S}},
    pass_attr::Function = MOI.set,
) where {F,S}
    # Copy constraint attributes
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F,S}())
    if !copy_names
        attrs = filter(attr -> !(attr isa MOI.ConstraintName), attrs)
    end
    if !isempty(attrs) # If `attrs` is empty, we can spare the computation of `cis_dest`
        cis_dest = map(ci -> idxmap[ci], cis_src)
        _pass_attributes(
            dest,
            src,
            idxmap,
            attrs,
            (CI{F,S},),
            (cis_src,),
            (cis_dest,),
            pass_attr,
        )
    end
end

function _pass_attributes(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    attrs,
    supports_args,
    get_args,
    set_args,
    pass_attr!::Function = MOI.set,
)
    for attr in attrs
        @assert MOI.is_copyable(attr)
        if attr isa MOI.VariablePrimalStart ||
           attr isa MOI.ConstraintPrimalStart ||
           attr isa MOI.ConstraintDualStart
            # As starting values are simply *hints* for the optimization, not
            # supporting them gives a warning, not an error
            if !MOI.supports(dest, attr, supports_args...)
                @warn(
                    "$attr is not supported by $(typeof(dest)). This " *
                    "information will be discarded."
                )
                continue
            end
        end
        value = MOI.get(src, attr, get_args...)
        if value !== nothing
            mapped_value = map_indices(idxmap, value)
            pass_attr!(dest, attr, set_args..., mapped_value)
        end
    end
end

"""
    copy_vector_of_variables(dest::MOI.ModelLike, src::MOI.ModelLike,
                             idxmap::IndexMap,
                             S::Type{<:MOI.AbstractSet})

Copy the constraints of type `MOI.VectorOfVariables`-in-`S` from the model `src`
to the model `dest` and fill `idxmap` accordingly. The copy is only done when
the variables to be copied are not already keys of `idxmap`. It returns a list
of the constraints copied and not copied.
"""
function copy_vector_of_variables(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    S::Type{<:MOI.AbstractSet},
    copy_constrained_variables::Function = MOI.add_constrained_variables,
)
    added = MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
    not_added = MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
    cis_src =
        MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}())
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.VectorOfVariables}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if all(vi -> !haskey(idxmap, vi), f_src.variables) &&
           allunique(f_src.variables)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vis_dest, ci_dest = copy_constrained_variables(dest, set)
            idxmap[ci_src] = ci_dest
            for (vi_src, vi_dest) in zip(f_src.variables, vis_dest)
                idxmap[vi_src] = vi_dest
            end
            push!(added, ci_src)
        else
            push!(not_added, ci_src)
        end
    end
    return added, not_added
end

"""
    copy_single_variable(dest::MOI.ModelLike, src::MOI.ModelLike,
                          idxmap::IndexMap,
                          S::Type{<:MOI.AbstractSet})

Copy the constraints of type `MOI.SingleVariable`-in-`S` from the model `src` to
the model `dest` and fill `idxmap` accordingly. The copy is only done when the
variables to be copied are not already keys of `idxmap`. It returns a list of
the constraints not copied.
"""
function copy_single_variable(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    S::Type{<:MOI.AbstractSet},
    copy_constrained_variable::Function = MOI.add_constrained_variable,
)
    added = MOI.ConstraintIndex{MOI.SingleVariable,S}[]
    not_added = MOI.ConstraintIndex{MOI.SingleVariable,S}[]
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable,S}())
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.SingleVariable}
    for (ci_src, f_src) in zip(cis_src, fs_src)
        if !haskey(idxmap, f_src.variable)
            set = MOI.get(src, MOI.ConstraintSet(), ci_src)::S
            vi_dest, ci_dest = copy_constrained_variable(dest, set)
            idxmap[ci_src] = ci_dest
            idxmap[f_src.variable] = vi_dest
            push!(added, ci_src)
        else
            push!(not_added, ci_src)
        end
    end
    return added, not_added
end

"""
    copy_constraints(dest::MOI.ModelLike, src::MOI.ModelLike,
                     idxmap::IndexMap,
                     cis_src::Vector{<:MOI.ConstraintIndex},
                     filter_constraints::Union{Nothing, Function}=nothing)

Copy the constraints `cis_src` from the model `src` to the model `dest` and fill
`idxmap` accordingly. Note that the attributes are not copied; call
[`pass_attributes`] to copy the constraint attributes.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function copy_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
    filter_constraints::Union{Nothing,Function} = nothing,
)
    # Retrieve the constraints to copy.
    f_src = MOI.get(src, MOI.ConstraintFunction(), cis_src)

    # Filter this set of constraints if needed before.
    if filter_constraints !== nothing
        filter!(filter_constraints, cis_src)
    end

    # Copy the constraints into the new model and build the map.
    f_dest = map_indices.(Ref(idxmap), f_src)
    s = MOI.get(src, MOI.ConstraintSet(), cis_src)
    cis_dest = MOI.add_constraints(dest, f_dest, s)
    for (ci_src, ci_dest) in zip(cis_src, cis_dest)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function pass_nonvariable_constraints_fallback(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types,
    pass_cons = copy_constraints;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    for (F, S) in constraint_types
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)
    end
end

"""
    pass_nonvariable_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        constraint_types,
        pass_cons = copy_constraints;
        filter_constraints::Union{Nothing,Function} = nothing,
    )

For all tuples `(F, S)` in `constraint_types`, copy all constraints of type
`F`-in-`S` from `src` to `dest` mapping the variables indices with `idxmap`.
If `filter_constraints` is not nothing, only indices `ci` such that
`filter_constraints(ci)` is true are copied.

The default implementation calls `pass_nonvariable_constraints_fallback` which
copies the constraints with `pass_cons` and their attributes with `pass_attr`.
A method can be implemented to use a specialized copy for a given type of
`dest`.
"""
function pass_nonvariable_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types,
    pass_cons = copy_constraints;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    return pass_nonvariable_constraints_fallback(
        dest,
        src,
        idxmap,
        constraint_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
end

function pass_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    idxmap::IndexMap,
    single_variable_types,
    single_variable_indices,
    vector_of_variables_types,
    vector_of_variables_indices,
    pass_cons = copy_constraints,
    pass_attr = MOI.set;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    # copy_constraints can also take a filter_constraints argument; however, filtering
    # is performed within this function (because it also calls MOI.set on the constraints).
    # Don't pass this argument to copy_constraints/pass_cons to avoid a double filtering.
    for (S, cis_src) in zip(single_variable_types, single_variable_indices)
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)

        cis_src =
            MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable,S}())
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end

    for (S, cis_src) in
        zip(vector_of_variables_types, vector_of_variables_indices)
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        # do the rest in `pass_cons` which is type stable
        pass_cons(dest, src, idxmap, cis_src)

        cis_src =
            MOI.get(src, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}())
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end

    nonvariable_constraint_types = [
        (F, S) for
        (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent()) if
        F != MOI.SingleVariable && F != MOI.VectorOfVariables
    ]
    pass_nonvariable_constraints(
        dest,
        src,
        idxmap,
        nonvariable_constraint_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
    for (F, S) in nonvariable_constraint_types
        cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
        if filter_constraints !== nothing
            filter!(filter_constraints, cis_src)
        end
        # do the rest in `pass_cons` which is type stable
        pass_attributes(dest, src, copy_names, idxmap, cis_src, pass_attr)
    end
end

function copy_free_variables(
    dest::MOI.ModelLike,
    idxmap::IndexMap,
    vis_src,
    copy_variables::F,
) where {F<:Function}
    if length(vis_src) != length(keys(idxmap.varmap))
        vars = copy_variables(dest, length(vis_src) - length(idxmap.varmap))
        i = 1
        for vi in vis_src
            if !haskey(idxmap.varmap, vi)
                idxmap.varmap[vi] = vars[i]
                i += 1
            end
        end
        @assert i == length(vars) + 1
        @assert length(vis_src) == length(idxmap.varmap)
    end
end

function default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike)
    Base.depwarn(
        "default_copy_to(dest, src) is deprecated, use default_copy_to(dest, src, true) instead or default_copy_to(dest, src, false) if you do not want to copy names.",
        :default_copy_to,
    )
    return default_copy_to(dest, src, true)
end

function sorted_variable_sets_by_cost(dest::MOI.ModelLike, src::MOI.ModelLike)
    constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    single_or_vector_variables_types = [
        (F, S) for (F, S) in constraint_types if
        F === MOI.SingleVariable || F === MOI.VectorOfVariables
    ]
    sort!(
        single_or_vector_variables_types;
        by = ((F, S),) ->
        # We give priority for sets such that there is a big cost reduction
        # constraining the variable on creation.
            (
                MOI.get(dest, MOI.VariableBridgingCost{S}()) -
                MOI.get(dest, MOI.ConstraintBridgingCost{F,S}()),
                # In case of ties, we give priority to vector sets.
                # See issue #987
                F === MOI.SingleVariable,
            ),
    )
    return single_or_vector_variables_types
end

function try_constrain_variables_on_creation(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap,
    copy_constrained_variables::F1,
    copy_constrained_variable::F2,
) where {F1<:Function,F2<:Function}
    single_or_vector_variables_types = sorted_variable_sets_by_cost(dest, src)
    vector_of_variables_types = Type{<:MOI.AbstractVectorSet}[]
    vector_of_variables_added =
        Vector{<:MOI.ConstraintIndex{MOI.VectorOfVariables}}[]
    vector_of_variables_not_added =
        Vector{<:MOI.ConstraintIndex{MOI.VectorOfVariables}}[]
    single_variable_types = Type{<:MOI.AbstractScalarSet}[]
    single_variable_added = Vector{<:MOI.ConstraintIndex{MOI.SingleVariable}}[]
    single_variable_not_added =
        Vector{<:MOI.ConstraintIndex{MOI.SingleVariable}}[]
    for (F, S) in single_or_vector_variables_types
        if F === MOI.VectorOfVariables
            added, not_added = copy_vector_of_variables(
                dest,
                src,
                idxmap,
                S,
                copy_constrained_variables,
            )
            push!(vector_of_variables_added, added)
            push!(vector_of_variables_not_added, not_added)
            push!(vector_of_variables_types, S)
        elseif F === MOI.SingleVariable
            added, not_added = copy_single_variable(
                dest,
                src,
                idxmap,
                S,
                copy_constrained_variable,
            )
            push!(single_variable_added, added)
            push!(single_variable_not_added, not_added)
            push!(single_variable_types, S)
        end
    end
    return vector_of_variables_types,
    vector_of_variables_added,
    vector_of_variables_not_added,
    single_variable_types,
    single_variable_added,
    single_variable_not_added
end

"""
    default_copy_to(dest::MOI.ModelLike, src::MOI.ModelLike, copy_names::Bool,
                    filter_constraints::Union{Nothing, Function}=nothing)

Implements `MOI.copy_to(dest, src)` by adding the variables and then the
constraints and attributes incrementally. The function
[`MathOptInterface.supports_incremental_interface`](@ref) can be used to check
whether `dest` supports the copying a model incrementally.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function default_copy_to(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    MOI.empty!(dest)

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    idxmap = index_map_for_variable_indices(vis_src)

    # The `NLPBlock` assumes that the order of variables does not change (#849)
    if MOI.NLPBlock() in MOI.get(src, MOI.ListOfModelAttributesSet())
        constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
        single_variable_types =
            [S for (F, S) in constraint_types if F == MOI.SingleVariable]
        vector_of_variables_types =
            [S for (F, S) in constraint_types if F == MOI.VectorOfVariables]
        vector_of_variables_not_added = [
            MOI.get(
                src,
                MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}(),
            ) for S in vector_of_variables_types
        ]
        single_variable_not_added = [
            MOI.get(src, MOI.ListOfConstraintIndices{MOI.SingleVariable,S}()) for S in single_variable_types
        ]
    else
        vector_of_variables_types,
        _,
        vector_of_variables_not_added,
        single_variable_types,
        _,
        single_variable_not_added = try_constrain_variables_on_creation(
            dest,
            src,
            idxmap,
            MOI.add_constrained_variables,
            MOI.add_constrained_variable,
        )
    end

    copy_free_variables(dest, idxmap, vis_src, MOI.add_variables)

    # Copy variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src)

    # Copy model attributes
    pass_attributes(dest, src, copy_names, idxmap)

    # Copy constraints
    pass_constraints(
        dest,
        src,
        copy_names,
        idxmap,
        single_variable_types,
        single_variable_not_added,
        vector_of_variables_types,
        vector_of_variables_not_added,
        filter_constraints = filter_constraints,
    )

    return idxmap
end

# Allocate-Load Interface: 2-pass copy of a MathOptInterface model
# Some solver wrappers (e.g. SCS, ECOS, SDOI) do not supporting copying an optimization model using `MOI.add_constraints`, `MOI.add_variables` and `MOI.set`
# as they first need to figure out some information about a model before being able to pass the problem data to the solver.
#
# During the first pass (called allocate) : the model collects the relevant information about the problem so that
# on the second pass (called load), the constraints can be loaded directly to the solver (in case of SDOI) or written directly into the matrix of constraints (in case of SCS and ECOS).

# To support `MOI.copy_to` using this 2-pass mechanism, implement the allocate-load interface defined below and do:
# MOI.copy_to(dest::ModelType, src::MOI.ModelLike) = MOIU.allocate_load(dest, src)
# In the implementation of the allocate-load interface, it can be assumed that the different functions will the called in the following order:
# 1) `allocate_variables`
# 2) `allocate` and `allocate_constraint`
# 3) `load_variables`
# 4) `load` and `load_constraint`
# The interface is not meant to be used to create new constraints with `allocate_constraint` followed by `load_constraint` after a solve, it is only meant for being used in this order to implement `MOI.copy_to`.

# TODO deprecate this in MOI v0.7
"""
    needs_allocate_load(model::MOI.ModelLike)::Bool
Return a `Bool` indicating whether `model` does not support `add_variables`/`add_constraint`/`set` but supports `allocate_variables`/`allocate_constraint`/`allocate`/`load_variables`/`load_constraint`/`load`.
That is, the allocate-load interface need to be used to copy an model to `model`.
"""
needs_allocate_load(::MOI.ModelLike) = false

"""
    supports_allocate_load(model::MOI.ModelLike, copy_names::Bool)::Bool

Return a `Bool` indicating whether `model` supports
[`allocate_load(model, src, copy_names=copy_names)`](@ref) if all the
attributes set to `src` and constraints added to `src` are supported by `model`.
"""
supports_allocate_load(::MOI.ModelLike, copy_names::Bool) = false

"""
    allocate_variables(model::MOI.ModelLike, nvars::Integer)

Creates `nvars` variables and returns a vector of `nvars` variable indices.
"""
function allocate_variables end

"""
    allocate_constrained_variable(model::MOI.ModelLike,
                                  set::MOI.AbstractScalarSet)

Returns a tuple with the variable index and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variable(
    model::MOI.ModelLike,
    set::MOI.AbstractScalarSet,
)
    variables = allocate_variables(model, 1)
    variable = variables[1]
    func = MOI.SingleVariable(variable)
    constraint = allocate_constraint(model, func, set)
    return variable, constraint
end

"""
    allocate_constrained_variables(model::MOI.ModelLike,
                                   set::MOI.AbstractVectorSet)

Returns a tuple with the variable indices and the index for the constraint to be
used in `load_allocate_constraint`.
"""
function allocate_constrained_variables(
    model::MOI.ModelLike,
    set::MOI.AbstractVectorSet,
)
    variables = allocate_variables(model, MOI.dimension(set))
    func = MOI.VectorOfVariables(variables)
    constraint = allocate_constraint(model, func, set)
    return variables, constraint
end

const ALLOCATE_LOAD_NOT_IMPLEMENTED = ErrorException(
    "The Allocate-Load interface is" * " not implemented by the model",
)

"""
    allocate(model::ModelLike, attr::ModelLikeAttribute, value)
    allocate(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    allocate(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Informs `model` that `load` will be called with the same arguments after `load_variables` is called.
"""
function allocate(model::MOI.ModelLike, args...)
    return MOI.throw_set_error_fallback(
        model,
        args...;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function allocate(
    model::MOI.ModelLike,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    indices::Vector,
    values::Vector,
)
    for (index, value) in zip(indices, values)
        allocate(model, attr, index, value)
    end
end

"""
    allocate_constraint(model::MOI.ModelLike, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Returns the index for the constraint to be used in `load_constraint` that will be called after `load_variables` is called.
"""
function allocate_constraint(
    model::MOI.ModelLike,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.throw_add_constraint_error_fallback(
        model,
        func,
        set;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

"""
    load_variables(model::MOI.ModelLike, nvars::Integer)

Prepares `model` for [`load`](@ref) and [`load_constraint`](@ref).
"""
function load_variables end

"""
    load_constrained_variable(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.SingleVariable},
        set::MOI.AbstractScalarSet)

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variable(
    model::MOI.ModelLike,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{MOI.SingleVariable},
    set::MOI.AbstractScalarSet,
)
    func = MOI.SingleVariable(vi)
    return load_constraint(model, ci, func, set)
end

"""
    load_constrained_variables(
        model::MOI.ModelLike, vi::MOI.VariableIndex,
        ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
        set::MOI.AbstractVectorSet)

Load the constrained variable `vi` to set `set` to `model`.
"""
function load_constrained_variables(
    model::MOI.ModelLike,
    vis::Vector{MOI.VariableIndex},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    set::MOI.AbstractVectorSet,
)
    func = MOI.VectorOfVariables(vis)
    return load_constraint(model, ci, func, set)
end

"""
    load(model::ModelLike, attr::ModelLikeAttribute, value)
    load(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)
    load(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

This has the same effect that `set` with the same arguments except that `allocate` should be called first before `load_variables`.
"""
function load(model::MOI.ModelLike, args...)
    return MOI.throw_set_error_fallback(
        model,
        args...;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function load(
    model::MOI.ModelLike,
    attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    indices::Vector,
    values::Vector,
)
    for (index, value) in zip(indices, values)
        load(model, attr, index, value)
    end
end

"""
    load_single_variable(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable, S}}) where S

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_single_variable(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{MOI.SingleVariable,S}},
) where {S}
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.SingleVariable}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vi_dest = idxmap[f_src.variable]
        ci_dest = idxmap[ci_src]
        load_constrained_variable(dest, vi_dest, ci_dest, set)
    end
end

"""
    load_vector_of_variables(
        dest::MOI.ModelLike, src::MOI.ModelLike, idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables, S}}) where S

Load the constraints in `cis_src` from the model `src` into the model `dest`
using `load_constrained_variable`.
"""
function load_vector_of_variables(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{MOI.ConstraintIndex{MOI.VectorOfVariables,S}},
) where {S}
    fs_src = MOI.get(
        src,
        MOI.ConstraintFunction(),
        cis_src,
    )::Vector{MOI.VectorOfVariables}
    sets = MOI.get(src, MOI.ConstraintSet(), cis_src)::Vector{S}
    for (ci_src, f_src, set) in zip(cis_src, fs_src, sets)
        vis_dest = [idxmap[vi] for vi in f_src.variables]
        ci_dest = idxmap[ci_src]
        load_constrained_variables(dest, vis_dest, ci_dest, set)
    end
end

"""
    load_constraint(model::MOI.ModelLike, ci::MOI.ConstraintIndex, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Sets the constraint function and set for the constraint of index `ci`.
"""
function load_constraint(
    model::MOI.ModelLike,
    ::MOI.ConstraintIndex,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.throw_add_constraint_error_fallback(
        model,
        func,
        set;
        error_if_supported = ALLOCATE_LOAD_NOT_IMPLEMENTED,
    )
end

function allocate_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
)
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        ci_dest = allocate_constraint(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function load_constraints(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    cis_src::Vector{<:MOI.ConstraintIndex},
)
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = map_indices(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        load_constraint(dest, ci_dest, f_dest, s)
    end
end

"""
    allocate_load(dest::MOI.ModelLike, src::MOI.ModelLike,
                  filter_constraints::Union{Nothing, Function}=nothing
                  )

Implements `MOI.copy_to(dest, src)` using the Allocate-Load API. The function
[`supports_allocate_load`](@ref) can be used to check whether `dest` supports
the Allocate-Load API.

If the `filter_constraints` arguments is given, only the constraints for which
this function returns `true` will be copied. This function is given a
constraint index as argument.
"""
function allocate_load(
    dest::MOI.ModelLike,
    src::MOI.ModelLike,
    copy_names::Bool,
    filter_constraints::Union{Nothing,Function} = nothing,
)
    MOI.empty!(dest)

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    idxmap = index_map_for_variable_indices(vis_src)
    constraint_types = MOI.get(src, MOI.ListOfConstraintTypesPresent())
    single_variable_types =
        [S for (F, S) in constraint_types if F === MOI.SingleVariable]
    vector_of_variables_types =
        [S for (F, S) in constraint_types if F === MOI.VectorOfVariables]

    # Allocate variables
    vector_of_variables_types,
    vector_of_variables_allocated,
    vector_of_variables_not_allocated,
    single_variable_type,
    single_variable_allocated,
    single_variable_not_allocated = try_constrain_variables_on_creation(
        dest,
        src,
        idxmap,
        allocate_constrained_variables,
        allocate_constrained_variable,
    )

    copy_free_variables(dest, idxmap, vis_src, allocate_variables)

    # Allocate variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src, allocate)

    # Allocate model attributes
    pass_attributes(dest, src, copy_names, idxmap, allocate)

    # Allocate constraints
    pass_constraints(
        dest,
        src,
        copy_names,
        idxmap,
        single_variable_types,
        single_variable_not_allocated,
        vector_of_variables_types,
        vector_of_variables_not_allocated,
        allocate_constraints,
        allocate,
        filter_constraints = filter_constraints,
    )

    # Load variables
    load_variables(dest, length(vis_src))
    for cis_src in vector_of_variables_allocated
        load_vector_of_variables(dest, src, idxmap, cis_src)
    end
    for cis_src in single_variable_allocated
        load_single_variable(dest, src, idxmap, cis_src)
    end

    # Load variable attributes
    pass_attributes(dest, src, copy_names, idxmap, vis_src, load)

    # Load model attributes
    pass_attributes(dest, src, copy_names, idxmap, load)

    # Load constraints
    pass_constraints(
        dest,
        src,
        copy_names,
        idxmap,
        single_variable_types,
        single_variable_not_allocated,
        vector_of_variables_types,
        vector_of_variables_not_allocated,
        load_constraints,
        load,
        filter_constraints = filter_constraints,
    )

    return idxmap
end
