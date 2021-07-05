"""
    UniversalFallback

The `UniversalFallback` can be applied on a [`MathOptInterface.ModelLike`](@ref)
`model` to create the model `UniversalFallback(model)` supporting *any*
constraint and attribute. This allows to have a specialized implementation in
`model` for performance critical constraints and attributes while still
supporting other attributes with a small performance penalty. Note that `model`
is unaware of constraints and attributes stored by `UniversalFallback` so this
is not appropriate if `model` is an optimizer (for this reason,
[`MathOptInterface.optimize!`](@ref) has not been implemented). In that case,
optimizer bridges should be used instead.
"""
mutable struct UniversalFallback{MT} <: MOI.ModelLike
    model::MT
    objective::Union{MOI.AbstractScalarFunction,Nothing}
    # See https://github.com/jump-dev/JuMP.jl/issues/1152 and
    # https://github.com/jump-dev/JuMP.jl/issues/2238 for why we use an
    # `OrderedDict`
    single_variable_constraints::OrderedDict{DataType,OrderedDict}
    constraints::OrderedDict{Tuple{DataType,DataType},VectorOfConstraints}
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
    optattr::Dict{MOI.AbstractOptimizerAttribute,Any}
    modattr::Dict{MOI.AbstractModelAttribute,Any}
    varattr::Dict{MOI.AbstractVariableAttribute,Dict{VI,Any}}
    conattr::Dict{MOI.AbstractConstraintAttribute,Dict{MOI.ConstraintIndex,Any}}
    function UniversalFallback{MT}(model::MOI.ModelLike) where {MT}
        return new{typeof(model)}(
            model,
            nothing,
            OrderedDict{Tuple{DataType,DataType},OrderedDict}(),
            OrderedDict{Tuple{DataType,DataType},VectorOfConstraints}(),
            Dict{MOI.ConstraintIndex,String}(),
            nothing,
            Dict{MOI.AbstractOptimizerAttribute,Any}(),
            Dict{MOI.AbstractModelAttribute,Any}(),
            Dict{MOI.AbstractVariableAttribute,Dict{VI,Any}}(),
            Dict{
                MOI.AbstractConstraintAttribute,
                Dict{MOI.ConstraintIndex,Any},
            }(),
        )
    end
end

function UniversalFallback(model::MOI.ModelLike)
    return UniversalFallback{typeof(model)}(model)
end

function Base.show(io::IO, U::UniversalFallback)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    MOIU.print_with_acronym(io, summary(U))
    !(U.objective === nothing) && print(io, "\n$(indent)with objective")
    for (attr, name) in (
        (U.single_variable_constraints, "`SingleVariable` constraint"),
        (U.constraints, "constraint"),
        (U.optattr, "optimizer attribute"),
        (U.modattr, "model attribute"),
        (U.varattr, "variable attribute"),
        (U.conattr, "constraint attribute"),
    )
        n = length(attr)
        if n > 0
            print(io, "\n$(indent)with $n $name$(s(n))")
        end
    end
    print(io, "\n$(indent)fallback for ")
    return show(IOContext(io, :indent => get(io, :indent, 0) + 2), U.model)
end

function MOI.is_empty(uf::UniversalFallback)
    return MOI.is_empty(uf.model) &&
           uf.objective === nothing &&
           isempty(uf.single_variable_constraints) &&
           isempty(uf.constraints) &&
           isempty(uf.modattr) &&
           isempty(uf.varattr) &&
           isempty(uf.conattr)
end

function MOI.empty!(uf::UniversalFallback)
    MOI.empty!(uf.model)
    uf.objective = nothing
    empty!(uf.single_variable_constraints)
    empty!(uf.constraints)
    empty!(uf.con_to_name)
    uf.name_to_con = nothing
    empty!(uf.modattr)
    empty!(uf.varattr)
    empty!(uf.conattr)
    return
end

function pass_nonvariable_constraints(
    dest::UniversalFallback,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types,
    pass_cons;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    supported_types = eltype(constraint_types)[]
    unsupported_types = eltype(constraint_types)[]
    for (F, S) in constraint_types
        if MOI.supports_constraint(dest.model, F, S)
            push!(supported_types, (F, S))
        else
            push!(unsupported_types, (F, S))
        end
    end
    pass_nonvariable_constraints(
        dest.model,
        src,
        idxmap,
        supported_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
    return pass_nonvariable_constraints_fallback(
        dest,
        src,
        idxmap,
        unsupported_types,
        pass_cons;
        filter_constraints = filter_constraints,
    )
end

function MOI.copy_to(uf::UniversalFallback, src::MOI.ModelLike; kws...)
    return MOIU.automatic_copy_to(uf, src; kws...)
end

function MOI.supports_incremental_interface(
    uf::UniversalFallback,
    copy_names::Bool,
)
    return MOI.supports_incremental_interface(uf.model, copy_names)
end

function final_touch(uf::UniversalFallback, index_map)
    return final_touch(uf.model, index_map)
end

# References
function MOI.is_valid(uf::UniversalFallback, vi::MOI.VariableIndex)
    return MOI.is_valid(uf.model, vi)
end

function MOI.is_valid(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
) where {S}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        return MOI.is_valid(uf.model, ci)
    end
    return haskey(uf.single_variable_constraints, S) &&
           haskey(uf.single_variable_constraints[S], ci)
end

function MOI.is_valid(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if !MOI.supports_constraint(uf.model, F, S) &&
       !haskey(uf.constraints, (F, S))
        return false
    end
    return MOI.is_valid(constraints(uf, ci), ci)
end

function _delete(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
) where {S}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        MOI.delete(uf.model, ci)
    else
        MOI.is_valid(uf, ci) || throw(MOI.InvalidIndex(ci))
        delete!(uf.single_variable_constraints[S], ci)
    end
    return
end

function _delete(uf::UniversalFallback, ci::MOI.ConstraintIndex)
    MOI.delete(constraints(uf, ci), ci)
    return
end

function MOI.delete(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    _delete(uf, ci)
    if !MOI.supports_constraint(uf.model, F, S)
        delete!(uf.con_to_name, ci)
        uf.name_to_con = nothing
    end
    for d in values(uf.conattr)
        delete!(d, ci)
    end
    return
end

function _remove_variable(
    uf::UniversalFallback,
    constraints::OrderedDict{<:MOI.ConstraintIndex{MOI.SingleVariable}},
    vi::MOI.VariableIndex,
)
    MOI.delete(uf, [ci for ci in keys(constraints) if ci.value == vi.value])
    return
end

function MOI.delete(uf::UniversalFallback, vi::MOI.VariableIndex)
    vis = [vi]
    for constraints in values(uf.constraints)
        _throw_if_cannot_delete(constraints, vis, vi)
    end
    MOI.delete(uf.model, vi)
    for d in values(uf.varattr)
        delete!(d, vi)
    end
    if uf.objective !== nothing
        uf.objective = remove_variable(uf.objective, vi)
    end
    for constraints in values(uf.single_variable_constraints)
        _remove_variable(uf, constraints, vi)
    end
    for constraints in values(uf.constraints)
        _deleted_constraints(constraints, vi) do ci
            delete!(uf.con_to_name, ci)
            uf.name_to_con = nothing
            for d in values(uf.conattr)
                delete!(d, ci)
            end
        end
    end
    return
end

function MOI.delete(uf::UniversalFallback, vis::Vector{MOI.VariableIndex})
    fast_in_vis = Set(vis)
    for constraints in values(uf.constraints)
        _throw_if_cannot_delete(constraints, vis, fast_in_vis)
    end
    MOI.delete(uf.model, vis)
    for d in values(uf.varattr)
        for vi in vis
            delete!(d, vi)
        end
    end
    if uf.objective !== nothing
        uf.objective = remove_variable(uf.objective, vis)
    end
    for constraints in values(uf.single_variable_constraints)
        for vi in vis
            _remove_variable(uf, constraints, vi)
        end
    end
    for constraints in values(uf.constraints)
        _deleted_constraints(constraints, vis) do ci
            delete!(uf.con_to_name, ci)
            uf.name_to_con = nothing
            for d in values(uf.conattr)
                delete!(d, ci)
            end
        end
    end
    return
end

# Attributes
function _get(uf::UniversalFallback, attr::MOI.AbstractOptimizerAttribute)
    return uf.optattr[attr]
end

_get(uf::UniversalFallback, attr::MOI.AbstractModelAttribute) = uf.modattr[attr]

function _get(
    uf::UniversalFallback,
    attr::MOI.AbstractVariableAttribute,
    vi::MOI.VariableIndex,
)
    attribute_dict = get(uf.varattr, attr, nothing)
    if attribute_dict === nothing
        # It means the attribute is not set to any variable so in particular, it
        # is not set for `vi`
        return
    end
    return get(attribute_dict, vi, nothing)
end

function _get(
    uf::UniversalFallback,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    attribute_dict = get(uf.conattr, attr, nothing)
    if attribute_dict === nothing
        # It means the attribute is not set to any constraint so in particular,
        # it is not set for `ci`
        return
    end
    return get(attribute_dict, ci, nothing)
end

function _get(
    uf,
    attr::MOI.CanonicalConstraintFunction,
    ci::MOI.ConstraintIndex,
)
    return MOI.get_fallback(uf, attr, ci)
end

function MOI.get(
    uf::UniversalFallback,
    attr::Union{MOI.AbstractOptimizerAttribute,MOI.AbstractModelAttribute},
)
    if !MOI.is_copyable(attr) || MOI.supports(uf.model, attr)
        return MOI.get(uf.model, attr)
    end
    return _get(uf, attr)
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if MOI.supports_constraint(uf.model, F, S) &&
       (!MOI.is_copyable(attr) || MOI.supports(uf.model, attr, typeof(ci)))
        return MOI.get(uf.model, attr, ci)
    end
    return _get(uf, attr, ci)
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.AbstractVariableAttribute,
    vi::MOI.VariableIndex,
)
    if !MOI.is_copyable(attr) || MOI.supports(uf.model, attr, MOI.VariableIndex)
        return MOI.get(uf.model, attr, vi)
    end
    return _get(uf, attr, vi)
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.NumberOfConstraints{MOI.SingleVariable,S},
) where {S}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        return MOI.get(uf.model, attr)
    elseif !haskey(uf.single_variable_constraints, S)
        return 0
    else
        return length(uf.single_variable_constraints[S])
    end
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.NumberOfConstraints{F,S},
) where {F,S}
    return MOI.get(constraints(uf, F, S), attr)
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfConstraintIndices{MOI.SingleVariable,S},
) where {S}
    F = MOI.SingleVariable
    if MOI.supports_constraint(uf.model, F, S)
        return MOI.get(uf.model, listattr)
    end
    indices = get(
        uf.single_variable_constraints,
        S,
        OrderedDict{MOI.ConstraintIndex{F,S},S}(),
    )
    return collect(keys(indices))
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    return MOI.get(constraints(uf, F, S), listattr)
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfConstraintTypesPresent,
)
    list = MOI.get(uf.model, listattr)
    for (S, constraints) in uf.single_variable_constraints
        if !isempty(constraints)
            push!(list, (MOI.SingleVariable, S))
        end
    end
    for (FS, constraints) in uf.constraints
        if !MOI.is_empty(constraints)
            push!(list, FS)
        end
    end
    return list
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfOptimizerAttributesSet,
)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.optattr)
        push!(list, attr)
    end
    return list
end

function MOI.get(uf::UniversalFallback, listattr::MOI.ListOfModelAttributesSet)
    list = MOI.get(uf.model, listattr)
    if uf.objective !== nothing
        push!(list, MOI.ObjectiveFunction{typeof(uf.objective)}())
    end
    for attr in keys(uf.modattr)
        push!(list, attr)
    end
    return list
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfVariableAttributesSet,
)
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.varattr)
        push!(list, attr)
    end
    return list
end

function MOI.get(
    uf::UniversalFallback,
    listattr::MOI.ListOfConstraintAttributesSet{F,S},
) where {F,S}
    list = MOI.get(uf.model, listattr)
    for attr in keys(uf.conattr)
        push!(list, attr)
    end
    return list
end

# Objective
function MOI.set(
    uf::UniversalFallback,
    attr::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
) where {T}
    if sense == MOI.FEASIBILITY_SENSE
        uf.objective = nothing
    end
    MOI.set(uf.model, attr, sense)
    return
end

function MOI.get(uf::UniversalFallback, attr::MOI.ObjectiveFunctionType)
    if uf.objective === nothing
        return MOI.get(uf.model, attr)
    end
    return typeof(uf.objective)
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.ObjectiveFunction{F},
)::F where {F}
    if uf.objective === nothing
        return MOI.get(uf.model, attr)
    end
    return uf.objective
end

function MOI.set(
    uf::UniversalFallback,
    attr::MOI.ObjectiveFunction,
    func::MOI.AbstractScalarFunction,
)
    if MOI.supports(uf.model, attr)
        MOI.set(uf.model, attr, func)
        # Clear any fallback objective
        uf.objective = nothing
    else
        uf.objective = copy(func)
        # Clear any `model` objective
        sense = MOI.get(uf.model, MOI.ObjectiveSense())
        MOI.set(uf.model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        MOI.set(uf.model, MOI.ObjectiveSense(), sense)
    end
    return
end

function MOI.modify(
    uf::UniversalFallback,
    obj::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
) where {F}
    if uf.objective === nothing
        MOI.modify(uf.model, obj, change)
    else
        uf.objective = modify_function(uf.objective, change)
    end
    return
end

# Name
# The names of constraints not supported by `uf.model` need to be handled
function MOI.set(
    uf::UniversalFallback,
    attr::MOI.ConstraintName,
    ci::MOI.ConstraintIndex{F,S},
    name::String,
) where {F,S}
    if MOI.supports_constraint(uf.model, F, S)
        MOI.set(uf.model, attr, ci, name)
    else
        uf.con_to_name[ci] = name
        uf.name_to_con = nothing # Invalidate the name map.
    end
    return
end

function MOI.supports(
    ::UniversalFallback,
    ::MOI.ConstraintName,
    ::Type{<:MOI.ConstraintIndex{MOI.SingleVariable,<:MOI.AbstractScalarSet}},
)
    return throw(MOI.SingleVariableConstraintNameError())
end

function MOI.set(
    ::UniversalFallback,
    ::MOI.ConstraintName,
    ::MOI.ConstraintIndex{MOI.SingleVariable,<:MOI.AbstractScalarSet},
    ::String,
)
    return throw(MOI.SingleVariableConstraintNameError())
end

function MOI.get(
    uf::UniversalFallback,
    attr::MOI.ConstraintName,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if MOI.supports_constraint(uf.model, F, S)
        return MOI.get(uf.model, attr, ci)
    end
    return get(uf.con_to_name, ci, EMPTYSTRING)
end

function MOI.get(uf::UniversalFallback, ::Type{VI}, name::String)
    return MOI.get(uf.model, VI, name)
end

check_type_and_multiple_names(::Type, ::Nothing, ::Nothing, name) = nothing

function check_type_and_multiple_names(
    ::Type{T},
    value::T,
    ::Nothing,
    name,
) where {T}
    return value
end

function check_type_and_multiple_names(::Type, ::Any, ::Nothing, name) where {T}
    return nothing
end

function check_type_and_multiple_names(
    ::Type{T},
    ::Nothing,
    value::T,
    name,
) where {T}
    return value
end

function check_type_and_multiple_names(::Type, ::Nothing, ::Any, name) where {T}
    return nothing
end

function check_type_and_multiple_names(T::Type, ::Any, ::Any, name)
    return throw_multiple_name_error(T, name)
end

function MOI.get(
    uf::UniversalFallback,
    ::Type{MOI.ConstraintIndex{F,S}},
    name::String,
) where {F,S}
    if uf.name_to_con === nothing
        uf.name_to_con = build_name_to_con_map(uf.con_to_name)
    end
    ci = if MOI.supports_constraint(uf.model, F, S)
        MOI.get(uf.model, MOI.ConstraintIndex{F,S}, name)
    else
        # There is no `F`-in-`S` constraint in `b.model`, `ci` is only queried
        # to check for duplicate names.
        MOI.get(uf.model, MOI.ConstraintIndex, name)
    end
    ci_uf = get(uf.name_to_con, name, nothing)
    throw_if_multiple_with_name(ci_uf, name)
    c = check_type_and_multiple_names(MOI.ConstraintIndex{F,S}, ci_uf, ci, name)
    return c
end

function MOI.get(
    uf::UniversalFallback,
    ::Type{MOI.ConstraintIndex},
    name::String,
)
    if uf.name_to_con === nothing
        uf.name_to_con = build_name_to_con_map(uf.con_to_name)
    end
    ci_uf = get(uf.name_to_con, name, nothing)
    throw_if_multiple_with_name(ci_uf, name)
    return check_type_and_multiple_names(
        MOI.ConstraintIndex,
        ci_uf,
        MOI.get(uf.model, MOI.ConstraintIndex, name),
        name,
    )
end

function _set(
    uf::UniversalFallback,
    attr::MOI.AbstractOptimizerAttribute,
    value,
)
    uf.optattr[attr] = value
    return
end

function _set(uf::UniversalFallback, attr::MOI.AbstractModelAttribute, value)
    uf.modattr[attr] = value
    return
end

function _set(
    uf,
    attr::MOI.AbstractVariableAttribute,
    vi::MOI.VariableIndex,
    value,
)
    if !haskey(uf.varattr, attr)
        uf.varattr[attr] = Dict{MOI.VariableIndex,Any}()
    end
    uf.varattr[attr][vi] = value
    return
end

function _set(
    uf,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
    value,
)
    if !haskey(uf.conattr, attr)
        uf.conattr[attr] = Dict{MOI.ConstraintIndex,Any}()
    end
    uf.conattr[attr][ci] = value
    return
end

function MOI.supports(
    ::UniversalFallback,
    ::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
)
    return true
end

function MOI.set(
    uf::UniversalFallback,
    attr::Union{MOI.AbstractOptimizerAttribute,MOI.AbstractModelAttribute},
    value,
)
    if MOI.supports(uf.model, attr)
        MOI.set(uf.model, attr, value)
    else
        _set(uf, attr, value)
    end
    return
end

function MOI.supports(
    ::UniversalFallback,
    ::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute},
    ::Type{<:MOI.Index},
)
    return true
end

function MOI.set(
    uf::UniversalFallback,
    attr::MOI.AbstractVariableAttribute,
    vi::MOI.VariableIndex,
    value,
)
    if MOI.supports(uf.model, attr, MOI.VariableIndex)
        MOI.set(uf.model, attr, vi, value)
    else
        _set(uf, attr, vi, value)
    end
    return
end

function MOI.set(
    uf::UniversalFallback,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex{F,S},
    value,
) where {F,S}
    if MOI.supports_constraint(uf.model, F, S) &&
       MOI.supports(uf.model, attr, MOI.ConstraintIndex{F,S})
        MOI.set(uf.model, attr, ci, value)
    else
        _set(uf, attr, ci, value)
    end
    return
end

# Constraints
function MOI.supports_constraint(
    ::UniversalFallback,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return true
end

function constraints(
    uf::UniversalFallback,
    ::Type{F},
    ::Type{S},
    getter::Function = get,
) where {F,S}
    if MOI.supports_constraint(uf.model, F, S)
        return uf.model
    end
    return getter(uf.constraints, (F, S)) do
        return VectorOfConstraints{F,S}()
    end::VectorOfConstraints{F,S}
end

function constraints(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if !MOI.supports_constraint(uf, F, S)
        throw(MOI.InvalidIndex(ci))
    end
    return constraints(uf, F, S)
end

function MOI.add_constraint(
    uf::UniversalFallback,
    func::MOI.SingleVariable,
    set::S,
) where {S<:MOI.AbstractScalarSet}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        return MOI.add_constraint(uf.model, func, set)
    end
    constraints = get!(
        uf.single_variable_constraints,
        S,
    ) do
        return OrderedDict{
            MOI.ConstraintIndex{MOI.SingleVariable,S},
            S,
        }()
    end::OrderedDict{MOI.ConstraintIndex{MOI.SingleVariable,S},S}
    ci = MOI.ConstraintIndex{MOI.SingleVariable,S}(func.variable.value)
    constraints[ci] = set
    return ci
end

function MOI.add_constraint(
    uf::UniversalFallback,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.add_constraint(
        constraints(uf, typeof(func), typeof(set), get!),
        func,
        set,
    )
end

function MOI.modify(
    uf::UniversalFallback,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(constraints(uf, ci), ci, change)
    return
end

function MOI.get(
    uf::UniversalFallback,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
)
    return MOI.get(constraints(uf, ci), attr, ci)
end

function MOI.set(
    uf::UniversalFallback,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
    func_or_set,
)
    return MOI.set(constraints(uf, ci), attr, ci, func_or_set)
end

function MOI.get(
    uf::UniversalFallback,
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{MOI.SingleVariable},
)
    MOI.throw_if_not_valid(uf, ci)
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end

function MOI.get(
    uf::UniversalFallback,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
) where {S}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        return MOI.get(uf.model, MOI.ConstraintSet(), ci)
    end
    MOI.throw_if_not_valid(uf, ci)
    return uf.single_variable_constraints[S][ci]
end

function MOI.set(
    ::UniversalFallback,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.SingleVariable},
    ::MOI.SingleVariable,
)
    return throw(MOI.SettingSingleVariableFunctionNotAllowed())
end

function MOI.set(
    uf::UniversalFallback,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
    set::S,
) where {S}
    if MOI.supports_constraint(uf.model, MOI.SingleVariable, S)
        MOI.set(uf.model, MOI.ConstraintSet(), ci, set)
    else
        MOI.throw_if_not_valid(uf, ci)
        uf.single_variable_constraints[S][ci] = set
    end
    return
end

# Variables

MOI.add_variable(uf::UniversalFallback) = MOI.add_variable(uf.model)

MOI.add_variables(uf::UniversalFallback, n) = MOI.add_variables(uf.model, n)
