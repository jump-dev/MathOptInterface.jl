# This file contains default implementations for the `MOI.copy_to` function that
# can be used by a model.

@deprecate automatic_copy_to default_copy_to
@deprecate supports_default_copy_to MOI.supports_incremental_interface

include("copy/index_map.jl")

"""
    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        idxmap::IndexMap,
        pass_attr::Function = MOI.set,
    )

Pass the model attributes from the model `src` to the model `dest` using
`canpassattr` to check if the attribute can be passed and `pass_attr` to pass
the attribute. Does not copy `Name` if `copy_names` is `false`.

    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        idxmap::IndexMap,
        vis_src::Vector{MOI.VariableIndex},
        pass_attr::Function = MOI.set,
    )

Pass the variable attributes from the model `src` to the model `dest` using
`canpassattr` to check if the attribute can be passed and `pass_attr` to pass
the attribute.

Does not copy `VariableName` if `copy_names` is `false`.

    pass_attributes(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        idxmap::IndexMap,
        cis_src::Vector{MOI.ConstraintIndex{F, S}},
        pass_attr::Function = MOI.set,
    ) where {F, S}

Pass the constraint attributes of `F`-in-`S` constraints from the model `src` to
the model `dest` using `canpassattr` to check if the attribute can be passed and
`pass_attr` to pass the attribute.

Does not copy `ConstraintName` if `copy_names` is `false`.
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
    _pass_attributes(
        dest,
        src,
        idxmap,
        attrs,
        tuple(),
        tuple(),
        tuple(),
        pass_attr,
    )
    return
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
    # If `attrs` is empty, we can spare the computation of `vis_dest`
    if !isempty(attrs)
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
    return
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
    if !copy_names || F == MOI.SingleVariable
        attrs = filter(attr -> !(attr isa MOI.ConstraintName), attrs)
    end
    # If `attrs` is empty, we can spare the computation of `cis_dest`
    if !isempty(attrs)
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
    return
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
    return
end

"""
    copy_vector_of_variables(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        S::Type{<:MOI.AbstractSet},
    )

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
    copy_single_variable(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        S::Type{<:MOI.AbstractSet},
        copy_constrained_variable::Function = MOI.add_constrained_variable,
    )

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
    copy_constraints(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        idxmap::IndexMap,
        cis_src::Vector{<:MOI.ConstraintIndex},
        filter_constraints::Union{Nothing,Function} = nothing,
    )

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
        idxmap[ci_src] = ci_dest
    end
    return
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
    return
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
    pass_nonvariable_constraints_fallback(
        dest,
        src,
        idxmap,
        constraint_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
    return
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
    # copy_constraints can also take a filter_constraints argument; however,
    # filtering is performed within this function (because it also calls MOI.set
    # on the constraints).
    #
    # Don't pass this argument to copy_constraints/pass_cons to avoid a double
    # filtering.
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
    return
end

function copy_free_variables(
    dest::MOI.ModelLike,
    idxmap::IndexMap,
    vis_src,
    copy_variables::F,
) where {F<:Function}
    if length(vis_src) != length(idxmap.var_map)
        vars = copy_variables(dest, length(vis_src) - length(idxmap.var_map))
        i = 1
        for vi in vis_src
            if !haskey(idxmap, vi)
                idxmap[vi] = vars[i]
                i += 1
            end
        end
        @assert i == length(vars) + 1
        @assert length(vis_src) == length(idxmap.var_map)
    end
    return
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
    function final_touch(model::MOI.ModelLike, idxmap) end

This is called at the end of [`default_copy_to`](@ref) to inform the model that
the copy is finished. This allows `model` to perform thats that should be done
only once all the model information is gathered.
"""
function final_touch(::MOI.ModelLike, idxmap) end

"""
    default_copy_to(
        dest::MOI.ModelLike,
        src::MOI.ModelLike,
        copy_names::Bool,
        filter_constraints::Union{Nothing,Function} = nothing,
    )

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
    if !MOI.supports_incremental_interface(dest, copy_names)
        error(
            "Model $(typeof(dest)) does not support copy",
            copy_names ? " with names" : "",
            ".",
        )
    end
    MOI.empty!(dest)

    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    idxmap = _index_map_for_variable_indices(vis_src)

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

    final_touch(dest, idxmap)

    return idxmap
end
