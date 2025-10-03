# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

abstract type AbstractModelLike{T} <: MOI.ModelLike end

"""
    mutable struct GenericModel{T,O,V,C} <: AbstractModelLike{T}

Implements a model supporting coefficients of type `T` and:

 * An objective function stored in `.objective::O`
 * Variables and `VariableIndex` constraints stored in `.variable_bounds::V`
 * `F`-in-`S` constraints (excluding `VariableIndex` constraints) stored in
   `.constraints::C`

All interactions take place via the MOI interface, so the types `O`, `V`, and
`C` must implement the API as needed for their functionality.
"""
mutable struct GenericModel{T,O,V,C} <: AbstractModelLike{T}
    name::String
    objective::O
    variables::V
    constraints::C
    var_to_name::Dict{MOI.VariableIndex,String}
    # If `nothing`, the dictionary hasn't been constructed yet.
    name_to_var::Union{Dict{String,MOI.VariableIndex},Nothing}
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
    # A useful dictionary for extensions to store things. These are
    # _not_ copied between models.
    ext::Dict{Symbol,Any}
    function GenericModel{T,O,V,C}() where {T,O,V,C}
        return new{T,O,V,C}(
            "",
            O(),
            V(),
            C(),
            Dict{MOI.VariableIndex,String}(),
            nothing,
            Dict{MOI.ConstraintIndex,String}(),
            nothing,
            Dict{Symbol,Any}(),
        )
    end
end

abstract type AbstractOptimizer{T} <: MOI.AbstractOptimizer end

"""
    mutable struct GenericOptimizer{T,O,V,C} <: AbstractOptimizer{T}

Implements a model supporting coefficients of type `T` and:

 * An objective function stored in `.objective::O`
 * Variables and `VariableIndex` constraints stored in `.variable_bounds::V`
 * `F`-in-`S` constraints (excluding `VariableIndex` constraints) stored in
   `.constraints::C`

All interactions take place via the MOI interface, so the types `O`, `V`, and
`C` must implement the API as needed for their functionality.
"""
mutable struct GenericOptimizer{T,O,V,C} <: AbstractOptimizer{T}
    name::String
    objective::O
    variables::V
    constraints::C
    var_to_name::Dict{MOI.VariableIndex,String}
    # If `nothing`, the dictionary hasn't been constructed yet.
    name_to_var::Union{Dict{String,MOI.VariableIndex},Nothing}
    con_to_name::Dict{MOI.ConstraintIndex,String}
    name_to_con::Union{Dict{String,MOI.ConstraintIndex},Nothing}
    # A useful dictionary for extensions to store things. These are
    # _not_ copied between models.
    ext::Dict{Symbol,Any}
    function GenericOptimizer{T,O,V,C}() where {T,O,V,C}
        return new{T,O,V,C}(
            "",
            O(),
            V(),
            C(),
            Dict{MOI.VariableIndex,String}(),
            nothing,
            Dict{MOI.ConstraintIndex,String}(),
            nothing,
            Dict{Symbol,Any}(),
        )
    end
end

const AbstractModel{T} = Union{AbstractModelLike{T},AbstractOptimizer{T}}

# `AbstractModel` fields `.variables` and `.constraints` act like a
# `StructOfConstraints` where `.variables` contains the `VariableIndex`-in-`S`
# constraints and `.constraints` contains the other constraints.
function constraints(
    model::AbstractModel,
    ::MOI.ConstraintIndex{MOI.VariableIndex},
)
    return model.variables
end

constraints(model::AbstractModel, ::MOI.ConstraintIndex) = model.constraints

function MOI.is_empty(model::AbstractModel)
    return isempty(model.name) &&
           MOI.is_empty(model.objective) &&
           MOI.is_empty(model.constraints) &&
           MOI.is_empty(model.variables)
end

function MOI.empty!(model::AbstractModel{T}) where {T}
    model.name = ""
    MOI.empty!(model.objective)
    MOI.empty!(model.variables)
    MOI.empty!(model.constraints)
    empty!(model.var_to_name)
    model.name_to_var = nothing
    empty!(model.con_to_name)
    model.name_to_con = nothing
    return
end

function MOI.copy_to(dest::AbstractModel, src::MOI.ModelLike)
    return default_copy_to(dest, src)
end

MOI.supports_incremental_interface(::AbstractModel) = true

function final_touch(model::AbstractModel, index_map)
    return final_touch(model.constraints, index_map)
end

# MOI.ListOfXXXAttributesSet

function MOI.get(::AbstractModel, ::MOI.ListOfOptimizerAttributesSet)
    return MOI.AbstractOptimizerAttribute[]
end

function MOI.get(
    model::AbstractModel,
    attr::MOI.ListOfModelAttributesSet,
)::Vector{MOI.AbstractModelAttribute}
    ret = MOI.AbstractModelAttribute[]
    append!(ret, MOI.get(model.objective, attr))
    if !isempty(model.name)
        push!(ret, MOI.Name())
    end
    return ret
end

function MOI.get(model::AbstractModel, ::MOI.ListOfVariableAttributesSet)
    ret = MOI.AbstractVariableAttribute[]
    if !isempty(model.var_to_name)
        push!(ret, MOI.VariableName())
    end
    return ret
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ListOfConstraintAttributesSet{F,S},
) where {F,S}
    ret = MOI.AbstractConstraintAttribute[]
    if any(Base.Fix2(isa, MOI.ConstraintIndex{F,S}), keys(model.con_to_name))
        push!(ret, MOI.ConstraintName())
    end
    return ret
end

# MOI.Name

MOI.supports(::AbstractModel, ::MOI.Name) = true

function MOI.set(model::AbstractModel, ::MOI.Name, name::String)
    model.name = name
    return
end

MOI.get(model::AbstractModel, ::MOI.Name) = model.name

# MOI.add_variable

"""
    function _add_variable end

This is called by `AbstractModel` to inform the `constraints` field that a
variable has been added. This is similar to
[`MOI.add_variable`](@ref) except that it should return `nothing`.
"""
function _add_variable end

_add_variable(::Nothing) = nothing

_add_variables(::Nothing, ::Int64) = nothing

function MOI.add_variable(model::AbstractModel)
    x = MOI.add_variable(model.variables)
    _add_variable(model.constraints)
    return x
end

function MOI.is_valid(model::AbstractModel, x::MOI.VariableIndex)
    return MOI.is_valid(model.variables, x)
end

# MOI.NumberOfVariables

function MOI.get(model::AbstractModel, attr::MOI.NumberOfVariables)::Int64
    return MOI.get(model.variables, attr)
end

# MOI.ListOfVariableIndices

function MOI.get(model::AbstractModel, attr::MOI.ListOfVariableIndices)
    return MOI.get(model.variables, attr)
end

# MOI.VariableName

function MOI.supports(
    ::AbstractModel,
    ::MOI.VariableName,
    ::Type{MOI.VariableIndex},
)
    return true
end

function MOI.set(
    model::AbstractModel,
    ::MOI.VariableName,
    vi::MOI.VariableIndex,
    name::String,
)
    model.var_to_name[vi] = name
    model.name_to_var = nothing # Invalidate the name map.
    return
end

function MOI.get(
    model::AbstractModel,
    ::MOI.VariableName,
    vi::MOI.VariableIndex,
)
    return get(model.var_to_name, vi, "")
end

# MOI.get(::AbstractModel, ::Type{MOI.VariableIndex}, ::String)

"""
    build_name_to_var_map(con_to_name::Dict{MOI.VariableIndex, String})

Create and return a reverse map from name to variable index, given a map from
variable index to name. The special value `MOI.VariableIndex(0)` is used to
indicate that multiple variables have the same name.
"""
function build_name_to_var_map(var_to_name::Dict{MOI.VariableIndex,String})
    name_to_var = Dict{String,MOI.VariableIndex}()
    for (var, var_name) in var_to_name
        if haskey(name_to_var, var_name)
            # 0 is a special value that means this string does not map to
            # a unique variable name.
            name_to_var[var_name] = MOI.VariableIndex(0)
        else
            name_to_var[var_name] = var
        end
    end
    return name_to_var
end

function throw_multiple_name_error(::Type{MOI.VariableIndex}, name::String)
    return error("Multiple variables have the name $name.")
end

function throw_multiple_name_error(::Type{<:MOI.ConstraintIndex}, name::String)
    return error("Multiple constraints have the name $name.")
end

function throw_if_multiple_with_name(::Nothing, ::String) end

function throw_if_multiple_with_name(index::MOI.Index, name::String)
    if iszero(index.value)
        throw_multiple_name_error(typeof(index), name)
    end
    return
end

function MOI.get(model::AbstractModel, ::Type{MOI.VariableIndex}, name::String)
    if model.name_to_var === nothing
        model.name_to_var = build_name_to_var_map(model.var_to_name)
    end
    result = get(model.name_to_var, name, nothing)
    throw_if_multiple_with_name(result, name)
    return result
end

# Constraints

function MOI.is_valid(model::AbstractModel, ci::MOI.ConstraintIndex)
    return MOI.is_valid(constraints(model, ci), ci)
end

function MOI.supports_constraint(
    model::AbstractModel,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return MOI.supports_constraint(model.constraints, F, S)
end

function MOI.add_constraint(
    model::AbstractModel,
    func::F,
    set::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    # We check supports_constraint here because it is a common practice for
    # AbstractModels to declare that they do not support particular constraints,
    # even though the underlying `.constraints` object does. See, for example,
    # the various models in MOI.FileFormats.
    if !MOI.supports_constraint(model, F, S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    return MOI.add_constraint(model.constraints, func, set)
end

function MOI.supports_constraint(
    model::AbstractModel,
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {S<:MOI.AbstractScalarSet}
    return MOI.supports_constraint(model.variables, MOI.VariableIndex, S)
end

function MOI.add_constraint(
    model::AbstractModel,
    func::MOI.VariableIndex,
    set::S,
) where {S<:MOI.AbstractScalarSet}
    # We check supports_constraint here because it is a common practice for
    # AbstractModels to declare that they do not support particular constraints,
    # even though the underlying `.constraints` object does. See, for example,
    # the various models in MOI.FileFormats.
    if !MOI.supports_constraint(model, MOI.VariableIndex, S)
        throw(MOI.UnsupportedConstraint{MOI.VariableIndex,S}())
    end
    return MOI.add_constraint(model.variables, func, set)
end

# MOI.NumberOfConstraints

function MOI.get(
    model::AbstractModel,
    attr::MOI.NumberOfConstraints{MOI.VariableIndex,S},
)::Int64 where {S<:MOI.AbstractScalarSet}
    if !MOI.supports_constraint(model, MOI.VariableIndex, S)
        return 0
    end
    return MOI.get(model.variables, attr)
end

function MOI.get(model::AbstractModel, attr::MOI.NumberOfConstraints)::Int64
    return MOI.get(model.constraints, attr)
end

# MOI.ListOfConstraintTypesPresent

function MOI.get(
    model::AbstractModel{T},
    attr::MOI.ListOfConstraintTypesPresent,
)::Vector{Tuple{Type,Type}} where {T}
    return vcat(
        MOI.get(model.constraints, attr),
        MOI.get(model.variables, attr),
    )
end

# MOI.ListOfConstraintIndices

function MOI.get(
    model::AbstractModel,
    attr::MOI.ListOfConstraintIndices{MOI.VariableIndex,S},
) where {S<:MOI.AbstractScalarSet}
    if !MOI.supports_constraint(model, MOI.VariableIndex, S)
        return MOI.ConstraintIndex{MOI.VariableIndex,S}[]
    end
    return MOI.get(model.variables, attr)
end

function MOI.get(model::AbstractModel, attr::MOI.ListOfConstraintIndices)
    return MOI.get(model.constraints, attr)
end

# MOI.ConstraintFunction/MOI.ConstraintSet

function MOI.get(
    model::AbstractModel,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
)
    return MOI.get(constraints(model, ci), attr, ci)
end

function MOI.set(
    model::AbstractModel,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
    value,
)
    MOI.set(constraints(model, ci), attr, ci, value)
    return
end

# MOI.ConstraintName

function MOI.supports(
    ::AbstractModel,
    ::MOI.ConstraintName,
    ::Type{<:MOI.ConstraintIndex},
)
    return true
end

function MOI.set(
    model::AbstractModel,
    ::MOI.ConstraintName,
    ci::MOI.ConstraintIndex,
    name::String,
)
    model.con_to_name[ci] = name
    model.name_to_con = nothing # Invalidate the name map.
    return
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ConstraintName,
    ci::MOI.ConstraintIndex,
)
    return get(model.con_to_name, ci, "")
end

function MOI.supports(
    ::AbstractModel,
    ::MOI.ConstraintName,
    ::Type{<:MOI.ConstraintIndex{MOI.VariableIndex,<:MOI.AbstractScalarSet}},
)
    return throw(MOI.VariableIndexConstraintNameError())
end

function MOI.set(
    ::AbstractModel,
    ::MOI.ConstraintName,
    ::MOI.ConstraintIndex{MOI.VariableIndex,<:MOI.AbstractScalarSet},
    ::String,
)
    return throw(MOI.VariableIndexConstraintNameError())
end

# MOI.get(::AbstractModel, ::Type{MOI.ConstraintIndex}, ::String)

"""
    build_name_to_con_map(con_to_name::Dict{MOI.ConstraintIndex, String})

Create and return a reverse map from name to constraint index, given a map from
constraint index to name. The special value
`MOI.ConstraintIndex{Nothing, Nothing}(0)` is used to indicate that multiple
constraints have the same name.
"""
function build_name_to_con_map(con_to_name::Dict{<:MOI.ConstraintIndex,String})
    name_to_con = Dict{String,MOI.ConstraintIndex}()
    for (con, con_name) in con_to_name
        if haskey(name_to_con, con_name)
            name_to_con[con_name] = MOI.ConstraintIndex{Nothing,Nothing}(0)
        else
            name_to_con[con_name] = con
        end
    end
    return name_to_con
end

function MOI.get(
    model::AbstractModel,
    ::Type{ConType},
    name::String,
) where {ConType<:MOI.ConstraintIndex}
    if model.name_to_con === nothing
        model.name_to_con = build_name_to_con_map(model.con_to_name)
    end
    ci = get(model.name_to_con, name, nothing)
    throw_if_multiple_with_name(ci, name)
    return ci isa ConType ? ci : nothing
end

# MOI.ObjectiveFunctionType

function MOI.get(model::AbstractModel, attr::MOI.ObjectiveFunctionType)
    return MOI.get(model.objective, attr)
end

# MOI.ObjectiveSense

function MOI.supports(model::AbstractModel, attr::MOI.ObjectiveSense)
    return MOI.supports(model.objective, attr)
end

function MOI.get(model::AbstractModel, attr::MOI.ObjectiveSense)
    return MOI.get(model.objective, attr)
end

function MOI.set(
    model::AbstractModel,
    attr::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
)
    MOI.set(model.objective, attr, sense)
    return
end

# MOI.ObjectiveFunction

function MOI.supports(model::AbstractModel, attr::MOI.ObjectiveFunction)
    return MOI.supports(model.objective, attr)
end

function MOI.get(model::AbstractModel, attr::MOI.ObjectiveFunction)
    return MOI.get(model.objective, attr)
end

function MOI.set(
    model::AbstractModel,
    attr::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:MOI.AbstractFunction}
    if !MOI.supports(model, attr)
        throw(MOI.UnsupportedAttribute(attr))
    end
    MOI.set(model.objective, attr, f)
    return
end

# MOI.modify

function MOI.modify(
    model::AbstractModel,
    attr::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model.objective, attr, change)
    return
end

function MOI.modify(
    model::AbstractModel,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model.constraints, ci, change)
    return
end

# MOI.delete

function MOI.delete(model::AbstractModel, ci::MOI.ConstraintIndex)
    MOI.delete(constraints(model, ci), ci)
    model.name_to_con = nothing
    delete!(model.con_to_name, ci)
    return
end

function MOI.delete(model::AbstractModel, vi::MOI.VariableIndex)
    _throw_if_cannot_delete(model.constraints, [vi], vi)
    MOI.delete(model.variables, vi)
    delete!(model.var_to_name, vi)
    _deleted_constraints(model.constraints, vi) do ci
        return delete!(model.con_to_name, ci)
    end
    MOI.delete(model.objective, vi)
    model.name_to_var = nothing
    model.name_to_con = nothing
    return
end

function MOI.delete(model::AbstractModel, vis::Vector{MOI.VariableIndex})
    if isempty(vis)
        return
    end
    _throw_if_cannot_delete(model.constraints, vis, Set(vis))
    _deleted_constraints(model.constraints, vis) do ci
        return delete!(model.con_to_name, ci)
    end
    for vi in vis
        MOI.delete(model.variables, vi)
        delete!(model.var_to_name, vi)
    end
    MOI.delete(model.objective, vis)
    model.name_to_var = nothing
    model.name_to_con = nothing
    return
end

# A copy utility

function pass_nonvariable_constraints(
    dest::AbstractModel,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types,
)
    pass_nonvariable_constraints(
        dest.constraints,
        src,
        idxmap,
        constraint_types,
    )
    return
end

# Macro to generate Model

function _struct_of_constraints_type(name, subtypes, parametrized_type)
    if length(subtypes) == 1
        # Only one type, no need for a `StructOfConstraints`.
        return subtypes[1]
    end
    expr = Expr(:curly, name, esc(:T))
    if parametrized_type
        append!(expr.args, subtypes)
    end
    return expr
end

# This macro is for expert/internal use only. Prefer the concrete Model type
# instantiated below.
"""
    macro model(
        model_name,
        scalar_sets,
        typed_scalar_sets,
        vector_sets,
        typed_vector_sets,
        scalar_functions,
        typed_scalar_functions,
        vector_functions,
        typed_vector_functions,
        is_optimizer = false
    )

Creates a type `model_name` implementing the MOI model interface and supporting
all combinations of the provided functions and sets.

Each `typed_` `scalar`/`vector` `sets`/`functions` argument is a tuple of types.
A type is "typed" if it has a coefficient `{T}` as the first type parameter.

## Tuple syntax

To give no set/function, write `()`.
To give one set or function `X`, write `(X,)`.

## `is_optimizer`

If `is_optimizer = true`, the resulting struct is a of [`GenericOptimizer`](@ref),
which is a subtype of [`MOI.AbstractOptimizer`](@ref), otherwise, it is a
[`GenericModel`](@ref), which is a subtype of [`MOI.ModelLike`](@ref).

## VariableIndex

 * The function [`MOI.VariableIndex`](@ref) must not be given in
   `scalar_functions`.
 * The model supports [`MOI.VariableIndex`](@ref)-in-`S` constraints where `S`
   is [`MOI.EqualTo`](@ref), [`MOI.GreaterThan`](@ref), [`MOI.LessThan`](@ref),
   [`MOI.Interval`](@ref), [`MOI.Integer`](@ref), [`MOI.ZeroOne`](@ref),
   [`MOI.Semicontinuous`](@ref) or [`MOI.Semiinteger`](@ref).
 * The sets supported with [`MOI.VariableIndex`](@ref) cannot be controlled from
   the macro; use [`UniversalFallback`](@ref) to support more sets.

## Example

The model describing a linear program would be:
```julia
@model(
    LPModel,                                          # model_name
    (),                                               # untyped scalar sets
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval), #   typed scalar sets
    (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives),  # untyped vector sets
    (),                                               #   typed vector sets
    (),                                               # untyped scalar functions
    (MOI.ScalarAffineFunction,),                      #   typed scalar functions
    (MOI.VectorOfVariables,),                         # untyped vector functions
    (MOI.VectorAffineFunction,),                      #   typed vector functions
    false,                                            # is_optimizer
)
```
"""
macro model(
    model_name,
    scalar_sets,
    typed_scalar_sets,
    vector_sets,
    typed_vector_sets,
    scalar_functions,
    typed_scalar_functions,
    vector_functions,
    typed_vector_functions,
    is_optimizer = false,
)
    scalar_sets = vcat(
        SymbolSet.(scalar_sets.args, false),
        SymbolSet.(typed_scalar_sets.args, true),
    )
    vector_sets = vcat(
        SymbolSet.(vector_sets.args, false),
        SymbolSet.(typed_vector_sets.args, true),
    )
    scalar_funs = vcat(
        SymbolFun.(scalar_functions.args, false),
        SymbolFun.(typed_scalar_functions.args, true),
    )
    vector_funs = vcat(
        SymbolFun.(vector_functions.args, false),
        SymbolFun.(typed_vector_functions.args, true),
    )
    scname = esc(Symbol("$(model_name)ScalarConstraints"))
    vcname = esc(Symbol("$(model_name)VectorConstraints"))
    funs = [scalar_funs; vector_funs]
    set_struct_types = map(eachindex(funs)) do i
        cname, sets = if i <= length(scalar_funs)
            scname, scalar_sets
        else
            vcname, vector_sets
        end
        voc = map(sets) do set
            return :(VectorOfConstraints{$(_typed(funs[i])),$(_typed(set))})
        end
        return _struct_of_constraints_type(cname, voc, true)
    end
    func_name = esc(Symbol("$(model_name)FunctionConstraints"))
    expr = quote end
    if length(scalar_sets) >= 2
        push!(expr.args, struct_of_constraint_code(scname, scalar_sets))
    end
    if length(vector_sets) >= 2
        push!(expr.args, struct_of_constraint_code(vcname, vector_sets))
    end
    if length(funs) != 1
        push!(
            expr.args,
            struct_of_constraint_code(func_name, funs, set_struct_types),
        )
    end
    func_typed = _struct_of_constraints_type(func_name, set_struct_types, false)
    T = esc(:T)
    generic = is_optimizer ? GenericOptimizer : GenericModel
    push!(
        expr.args,
        quote
            const $(esc(model_name)){$T} = $generic{
                $T,
                ObjectiveContainer{$T},
                VariablesContainer{$T},
                $func_typed,
            }
        end,
    )
    return expr
end

const LessThanIndicatorOne{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}

const LessThanIndicatorZero{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{T}}

const GreaterThanIndicatorOne{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}}

const GreaterThanIndicatorZero{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.GreaterThan{T}}

const EqualToIndicatorOne{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{T}}

const EqualToIndicatorZero{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.EqualTo{T}}

@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (
        MOI.EqualTo,
        MOI.GreaterThan,
        MOI.LessThan,
        MOI.Interval,
        MOI.Semicontinuous,
        MOI.Semiinteger,
        MOI.Parameter,
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.Complements,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.NormCone,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.RelativeEntropyCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.PositiveSemidefiniteConeSquare,
        MOI.HermitianPositiveSemidefiniteConeTriangle,
        MOI.Scaled{MOI.PositiveSemidefiniteConeTriangle},
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
        MOI.AllDifferent,
        MOI.CountDistinct,
        MOI.CountBelongs,
        MOI.CountAtLeast,
        MOI.CountGreaterThan,
        MOI.Circuit,
        MOI.Cumulative,
        MOI.Path,
    ),
    (
        MOI.PowerCone,
        MOI.DualPowerCone,
        MOI.SOS1,
        MOI.SOS2,
        LessThanIndicatorOne,
        LessThanIndicatorZero,
        GreaterThanIndicatorOne,
        GreaterThanIndicatorZero,
        EqualToIndicatorOne,
        EqualToIndicatorZero,
        MOI.Table,
        MOI.BinPacking,
        MOI.HyperRectangle,
        MOI.VectorNonlinearOracle,
    ),
    (MOI.ScalarNonlinearFunction,),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables, MOI.VectorNonlinearFunction),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

@doc raw"""
    MOI.Utilities.Model{T}() where {T}

An implementation of `ModelLike` that supports all functions and sets defined
in MOI. It is parameterized by the coefficient type.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}()
MOIU.Model{Float64}
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```
"""
Model

# This export makes the type be printed as:
# ```julia
# julia> Base.show(IOContext(stdout, :compact => true), MathOptInterface.Utilities.Model)
# Model{T} where T
#
# julia> print(MathOptInterface.Utilities.Model)
# MathOptInterface.Utilities.Model{T} where T
#
# julia> MathOptInterface.Utilities.Model
# MathOptInterface.Utilities.Model{T} where T (alias for MathOptInterface.Utilities.GenericModel{T, MathOptInterface.Utilities.ObjectiveContainer{T}, MathOptInterface.Utilities.VariablesContainer{T}, MathOptInterface.Utilities.ModelFunctionConstraints{T}} where T)
# ```
# As MOI is not doing `using .Utilities` and is not exporting `Model`, the user
# still needs to do `MOI.Utilities.Model` unless he does
# `using MathOptInterface.Utilities`.
export Model
