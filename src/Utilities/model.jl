const EMPTYSTRING = ""

# Implementation of MOI for AbstractModel
abstract type AbstractModelLike{T} <: MOI.ModelLike end
abstract type AbstractOptimizer{T} <: MOI.AbstractOptimizer end
const AbstractModel{T} = Union{AbstractModelLike{T},AbstractOptimizer{T}}

# Variables
function MOI.get(model::AbstractModel, attr::MOI.NumberOfVariables)::Int64
    return MOI.get(model.variables, attr)
end

"""
    function _add_variable end

This is called by `AbstractModel` to inform the `constraints` field that a
variable has been added. This is similar to
[`MathOptInterface.add_variable`](@ref) except that it should return `nothing`.
"""
function _add_variable end

function _add_variable(::Nothing) end
function _add_variables(::Nothing, ::Int64) end

function MOI.add_variable(model::AbstractModel)
    x = MOI.add_variable(model.variables)
    _add_variable(model.constraints)
    return x
end

function MOI.add_variables(model::AbstractModel, n::Integer)
    return [MOI.add_variable(model) for i in 1:n]
end

"""
    remove_variable(f::MOI.AbstractFunction, s::MOI.AbstractSet, vi::MOI.VariableIndex)

Return a tuple `(g, t)` representing the constraint `f`-in-`s` with the
variable `vi` removed. That is, the terms containing the variable `vi` in the
function `f` are removed and the dimension of the set `s` is updated if
needed (e.g. when `f` is a `VectorOfVariables` with `vi` being one of the
variables).
"""
remove_variable(f, s, vi::VI) = remove_variable(f, vi), s
function remove_variable(f::MOI.VectorOfVariables, s, vi::VI)
    g = remove_variable(f, vi)
    if length(g.variables) != length(f.variables)
        t = MOI.update_dimension(s, length(g.variables))
    else
        t = s
    end
    return g, t
end

function filter_variables(keep::F, f, s) where {F<:Function}
    return filter_variables(keep, f), s
end
function filter_variables(
    keep::F,
    f::MOI.VectorOfVariables,
    s,
) where {F<:Function}
    g = filter_variables(keep, f)
    if length(g.variables) != length(f.variables)
        t = MOI.update_dimension(s, length(g.variables))
    else
        t = s
    end
    return g, t
end

function _delete_variable(
    model::AbstractModel{T},
    vi::MOI.VariableIndex,
) where {T}
    MOI.delete(model.variables, vi)
    delete!(model.var_to_name, vi)
    model.name_to_var = nothing
    model.name_to_con = nothing
    return
end

function MOI.delete(model::AbstractModel, vi::MOI.VariableIndex)
    _throw_if_cannot_delete(model.constraints, [vi], vi)
    _delete_variable(model, vi)
    _deleted_constraints(model.constraints, vi) do ci
        return delete!(model.con_to_name, ci)
    end
    MOI.delete(model.objective, vi)
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
        _delete_variable(model, vi)
    end
    MOI.delete(model.objective, vis)
    model.name_to_con = nothing
    return
end

function MOI.is_valid(
    model::AbstractModel,
    ci::CI{MOI.SingleVariable,S},
) where {S}
    return MOI.is_valid(model.variables, ci)
end

function MOI.is_valid(model::AbstractModel, ci::MOI.ConstraintIndex)
    return MOI.is_valid(model.constraints, ci)
end

function MOI.is_valid(model::AbstractModel, x::MOI.VariableIndex)
    return MOI.is_valid(model.variables, x)
end

function MOI.get(model::AbstractModel, attr::MOI.ListOfVariableIndices)
    return MOI.get(model.variables, attr)
end

# Names
MOI.supports(::AbstractModel, ::MOI.Name) = true
function MOI.set(model::AbstractModel, ::MOI.Name, name::String)
    return model.name = name
end
MOI.get(model::AbstractModel, ::MOI.Name) = model.name

MOI.supports(::AbstractModel, ::MOI.VariableName, vi::Type{VI}) = true
function MOI.set(model::AbstractModel, ::MOI.VariableName, vi::VI, name::String)
    model.var_to_name[vi] = name
    model.name_to_var = nothing # Invalidate the name map.
    return
end

function MOI.get(model::AbstractModel, ::MOI.VariableName, vi::VI)
    return get(model.var_to_name, vi, EMPTYSTRING)
end

"""
    build_name_to_var_map(con_to_name::Dict{MOI.VariableIndex, String})

Create and return a reverse map from name to variable index, given a map from
variable index to name. The special value `MOI.VariableIndex(0)` is used to
indicate that multiple variables have the same name.
"""
function build_name_to_var_map(var_to_name::Dict{VI,String})
    name_to_var = Dict{String,VI}()
    for (var, var_name) in var_to_name
        if haskey(name_to_var, var_name)
            # 0 is a special value that means this string does not map to
            # a unique variable name.
            name_to_var[var_name] = VI(0)
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
end

function MOI.get(model::AbstractModel, ::Type{VI}, name::String)
    if model.name_to_var === nothing
        # Rebuild the map.
        model.name_to_var = build_name_to_var_map(model.var_to_name)
    end
    result = get(model.name_to_var, name, nothing)
    throw_if_multiple_with_name(result, name)
    return result
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ListOfVariableAttributesSet,
)::Vector{MOI.AbstractVariableAttribute}
    return isempty(model.var_to_name) ? [] : [MOI.VariableName()]
end

MOI.supports(model::AbstractModel, ::MOI.ConstraintName, ::Type{<:CI}) = true

function MOI.set(
    model::AbstractModel,
    ::MOI.ConstraintName,
    ci::CI,
    name::String,
)
    model.con_to_name[ci] = name
    model.name_to_con = nothing # Invalidate the name map.
    return
end

function MOI.supports(
    ::AbstractModel,
    ::MOI.ConstraintName,
    ::Type{<:MOI.ConstraintIndex{MOI.SingleVariable,<:MOI.AbstractScalarSet}},
)
    return throw(MOI.SingleVariableConstraintNameError())
end

function MOI.set(
    ::AbstractModel,
    ::MOI.ConstraintName,
    ::MOI.ConstraintIndex{MOI.SingleVariable,<:MOI.AbstractScalarSet},
    ::String,
)
    return throw(MOI.SingleVariableConstraintNameError())
end

function MOI.get(model::AbstractModel, ::MOI.ConstraintName, ci::CI)
    return get(model.con_to_name, ci, EMPTYSTRING)
end

"""
    build_name_to_con_map(con_to_name::Dict{MOI.ConstraintIndex, String})

Create and return a reverse map from name to constraint index, given a map from
constraint index to name. The special value
`MOI.ConstraintIndex{Nothing, Nothing}(0)` is used to indicate that multiple
constraints have the same name.
"""
function build_name_to_con_map(con_to_name::Dict{CI,String})
    name_to_con = Dict{String,CI}()
    for (con, con_name) in con_to_name
        if haskey(name_to_con, con_name)
            name_to_con[con_name] = CI{Nothing,Nothing}(0)
        else
            name_to_con[con_name] = con
        end
    end
    return name_to_con
end

function MOI.get(model::AbstractModel, ConType::Type{<:CI}, name::String)
    if model.name_to_con === nothing
        # Rebuild the map.
        model.name_to_con = build_name_to_con_map(model.con_to_name)
    end
    ci = get(model.name_to_con, name, nothing)
    throw_if_multiple_with_name(ci, name)
    return ci isa ConType ? ci : nothing
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ListOfConstraintAttributesSet{F,S},
) where {F,S}
    if any(k -> k isa MOI.ConstraintIndex{F,S}, keys(model.con_to_name))
        return MOI.AbstractConstraintAttribute[MOI.ConstraintName()]
    end
    return MOI.AbstractConstraintAttribute[]
end

# Objective

function MOI.get(
    model::AbstractModel,
    attr::Union{
        MOI.ObjectiveSense,
        MOI.ObjectiveFunction,
        MOI.ObjectiveFunctionType,
    },
)
    return MOI.get(model.objective, attr)
end

function MOI.supports(
    model::AbstractModel,
    attr::Union{MOI.ObjectiveSense,MOI.ObjectiveFunction},
)
    return MOI.supports(model.objective, attr)
end

function MOI.set(
    model::AbstractModel,
    attr::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
)
    MOI.set(model.objective, attr, sense)
    return
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

function MOI.modify(
    model::AbstractModel,
    attr::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(model.objective, attr, change)
    return
end

function MOI.get(::AbstractModel, ::MOI.ListOfOptimizerAttributesSet)
    return MOI.AbstractOptimizerAttribute[]
end

function MOI.get(
    model::AbstractModel,
    attr::MOI.ListOfModelAttributesSet,
)::Vector{MOI.AbstractModelAttribute}
    ret = MOI.get(model.objective, attr)
    if !isempty(model.name)
        push!(ret, MOI.Name())
    end
    return ret
end

# Constraints

function MOI.supports_constraint(
    ::AbstractModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return true
end
function MOI.supports_constraint(
    model::AbstractModel,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return MOI.supports_constraint(model.constraints, F, S)
end

function MOI.add_constraint(
    model::AbstractModel{T},
    f::MOI.SingleVariable,
    s::SUPPORTED_VARIABLE_SCALAR_SETS{T},
) where {T}
    return MOI.add_constraint(model.variables, f, s)
end

function MOI.add_constraint(
    model::AbstractModel,
    func::MOI.AbstractFunction,
    set::MOI.AbstractSet,
)
    return MOI.add_constraint(model.constraints, func, set)
end

function MOI.get(
    model::AbstractModel,
    attr::Union{MOI.AbstractFunction,MOI.AbstractSet},
    ci::MOI.ConstraintIndex,
)
    return MOI.get(model.constraints, attr, ci)
end

function _delete_constraint(
    model::AbstractModel,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
) where {S}
    MOI.throw_if_not_valid(model, ci)
    MOI.delete(model.variables, ci)
    return
end

function _delete_constraint(model::AbstractModel, ci::MOI.ConstraintIndex)
    return MOI.delete(model.constraints, ci)
end

function MOI.delete(model::AbstractModel, ci::MOI.ConstraintIndex)
    _delete_constraint(model, ci)
    model.name_to_con = nothing
    delete!(model.con_to_name, ci)
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

function MOI.set(
    ::AbstractModel,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.SingleVariable,<:MOI.AbstractScalarSet},
    ::MOI.SingleVariable,
)
    return throw(MOI.SettingSingleVariableFunctionNotAllowed())
end

function MOI.set(
    model::AbstractModel{T},
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.SingleVariable,S},
    set::S,
) where {T,S<:SUPPORTED_VARIABLE_SCALAR_SETS{T}}
    MOI.throw_if_not_valid(model, ci)
    MOI.set(model.variables, attr, ci, set)
    return
end

function MOI.set(
    model::AbstractModel,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{<:MOI.AbstractFunction,S},
    set::S,
) where {S<:MOI.AbstractSet}
    MOI.set(model.constraints, attr, ci, set)
    return
end

function MOI.set(
    model::AbstractModel,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{F,<:MOI.AbstractSet},
    func::F,
) where {F<:MOI.AbstractFunction}
    MOI.set(model.constraints, attr, ci, func)
    return
end

function MOI.get(
    model::AbstractModel,
    attr::MOI.NumberOfConstraints{MOI.SingleVariable,S},
) where {S}
    return MOI.get(model.variables, attr)
end

function MOI.get(
    model::AbstractModel,
    noc::MOI.NumberOfConstraints{F,S},
)::Int64 where {F,S}
    return MOI.get(model.constraints, noc)
end

function MOI.get(
    model::AbstractModel{T},
    attr::MOI.ListOfConstraintTypesPresent,
) where {T}
    return vcat(
        MOI.get(model.constraints, attr)::Vector{Tuple{Type,Type}},
        MOI.get(model.variables, attr)::Vector{Tuple{Type,Type}},
    )
end

function MOI.get(
    model::AbstractModel,
    attr::MOI.ListOfConstraintIndices{MOI.SingleVariable,S},
) where {S}
    return MOI.get(model.variables, attr)
end

function MOI.get(
    model::AbstractModel,
    loc::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    return MOI.get(model.constraints, loc)
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ConstraintFunction,
    ci::CI{MOI.SingleVariable},
)
    MOI.throw_if_not_valid(model, ci)
    return MOI.SingleVariable(MOI.VariableIndex(ci.value))
end
function MOI.get(
    model::AbstractModel,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
)
    return MOI.get(model.constraints, attr, ci)
end

function MOI.get(
    model::AbstractModel,
    ::MOI.ConstraintSet,
    ci::CI{MOI.SingleVariable,S},
) where {S}
    MOI.throw_if_not_valid(model, ci)
    return set_from_constants(model.variables, S, ci.value)
end

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
    empty!(model.var_to_name)
    model.name_to_var = nothing
    empty!(model.con_to_name)
    model.name_to_con = nothing
    MOI.empty!(model.constraints)
    return
end

function pass_nonvariable_constraints(
    dest::AbstractModel,
    src::MOI.ModelLike,
    idxmap::IndexMap,
    constraint_types;
    filter_constraints::Union{Nothing,Function} = nothing,
)
    return pass_nonvariable_constraints(
        dest.constraints,
        src,
        idxmap,
        constraint_types;
        filter_constraints = filter_constraints,
    )
end

function MOI.copy_to(dest::AbstractModel, src::MOI.ModelLike)
    return default_copy_to(dest, src)
end

MOI.supports_incremental_interface(::AbstractModel) = true
function final_touch(model::AbstractModel, index_map)
    return final_touch(model.constraints, index_map)
end

# Macro to generate Model

function _struct_of_constraints_type(name, subtypes, parametrized_type)
    if length(subtypes) == 1
        # Only one type, no need for a `StructOfConstraints`.
        return subtypes[1]
    else
        T = esc(:T)
        t = :($name{$T})
        if parametrized_type
            append!(t.args, subtypes)
        end
        return t
    end
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

Creates a type `model_name` implementing the MOI model interface and containing
`scalar_sets` scalar sets `typed_scalar_sets` typed scalar sets, `vector_sets`
vector sets, `typed_vector_sets` typed vector sets, `scalar_functions` scalar
functions, `typed_scalar_functions` typed scalar functions, `vector_functions`
vector functions and `typed_vector_functions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

The function [`MathOptInterface.SingleVariable`](@ref) should not be given in
`scalar_functions`. The model supports [`MathOptInterface.SingleVariable`](@ref)-in-`S`
constraints where `S` is [`MathOptInterface.EqualTo`](@ref),
[`MathOptInterface.GreaterThan`](@ref), [`MathOptInterface.LessThan`](@ref),
[`MathOptInterface.Interval`](@ref), [`MathOptInterface.Integer`](@ref),
[`MathOptInterface.ZeroOne`](@ref), [`MathOptInterface.Semicontinuous`](@ref)
or [`MathOptInterface.Semiinteger`](@ref). The sets supported
with the [`MathOptInterface.SingleVariable`](@ref) cannot be controlled from the
macro, use the [`UniversalFallback`](@ref) to support more sets.

This macro creates a model specialized for specific types of constraint,
by defining specialized structures and methods. To create a model that,
in addition to be optimized for specific constraints, also support arbitrary
constraints and attributes, use [`UniversalFallback`](@ref).

If `is_optimizer = true`, the resulting struct is a
of [`GenericOptimizer`](@ref), which is a subtype of
[`MathOptInterface.AbstractOptimizer`](@ref), otherwise, it is a
[`GenericModel`](@ref), which is a subtype of
[`MathOptInterface.ModelLike`](@ref).

### Examples

The model describing an linear program would be:
```julia
@model(LPModel,                                                   # Name of model
      (),                                                         # untyped scalar sets
      (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval), #   typed scalar sets
      (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives),            # untyped vector sets
      (),                                                         #   typed vector sets
      (),                                                         # untyped scalar functions
      (MOI.ScalarAffineFunction,),                                #   typed scalar functions
      (MOI.VectorOfVariables,),                                   # untyped vector functions
      (MOI.VectorAffineFunction,),                                #   typed vector functions
      false
    )
```

Let `MOI` denote `MathOptInterface`, `MOIU` denote `MOI.Utilities`.
The macro would create the following types with
[`struct_of_constraint_code`](@ref):
```julia
struct LPModelScalarConstraints{T, C1, C2, C3, C4} <: MOIU.StructOfConstraints
    moi_equalto::C1
    moi_greaterthan::C2
    moi_lessthan::C3
    moi_interval::C4
end
struct LPModelVectorConstraints{T, C1, C2, C3} <: MOIU.StructOfConstraints
    moi_zeros::C1
    moi_nonnegatives::C2
    moi_nonpositives::C3
end
struct LPModelFunctionConstraints{T} <: MOIU.StructOfConstraints
    moi_scalaraffinefunction::LPModelScalarConstraints{
        T,
        MOIU.VectorOfConstraints{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}},
        MOIU.VectorOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}},
        MOIU.VectorOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}},
        MOIU.VectorOfConstraints{MOI.ScalarAffineFunction{T}, MOI.Interval{T}}
    }
    moi_vectorofvariables::LPModelVectorConstraints{
        T,
        MOIU.VectorOfConstraints{MOI.VectorOfVariables, MOI.Zeros},
        MOIU.VectorOfConstraints{MOI.VectorOfVariables, MOI.Nonnegatives},
        MOIU.VectorOfConstraints{MOI.VectorOfVariables, MOI.Nonpositives}
    }
    moi_vectoraffinefunction::LPModelVectorConstraints{
        T,
        MOIU.VectorOfConstraints{MOI.VectorAffineFunction{T}, MOI.Zeros},
        MOIU.VectorOfConstraints{MOI.VectorAffineFunction{T}, MOI.Nonnegatives},
        MOIU.VectorOfConstraints{MOI.VectorAffineFunction{T}, MOI.Nonpositives}
    }
end
const LPModel{T} = MOIU.GenericModel{T,MOIU.ObjectiveContainer{T},MOIU.VariablesContainer{T},LPModelFunctionConstraints{T}}
```
The type `LPModel` implements the MathOptInterface API except methods specific
to optimizers like `optimize!` or `get` with `VariablePrimal`.
"""
macro model(
    model_name,
    ss,
    sst,
    vs,
    vst,
    sf,
    sft,
    vf,
    vft,
    is_optimizer = false,
)
    scalar_sets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vector_sets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = esc(Symbol(string(model_name) * "ScalarConstraints"))
    vcname = esc(Symbol(string(model_name) * "VectorConstraints"))

    esc_model_name = esc(model_name)
    # TODO if there is only one function or one set, remove the layer

    scalar_funs = [
        SymbolFun.(sf.args, false)
        SymbolFun.(sft.args, true)
    ]
    vector_funs = [
        SymbolFun.(vf.args, false)
        SymbolFun.(vft.args, true)
    ]
    funs = [scalar_funs; vector_funs]
    set_struct_types = map(eachindex(funs)) do i
        if i <= length(scalar_funs)
            cname = scname
            sets = scalar_sets
        else
            cname = vcname
            sets = vector_sets
        end
        voc = map(sets) do set
            return :(VectorOfConstraints{$(_typed(funs[i])),$(_typed(set))})
        end
        return _struct_of_constraints_type(cname, voc, true)
    end
    func_name = esc(Symbol(string(model_name) * "FunctionConstraints"))
    func_typed = _struct_of_constraints_type(func_name, set_struct_types, false)
    T = esc(:T)
    generic = if is_optimizer
        :(GenericOptimizer{
            $T,
            ObjectiveContainer{$T},
            VariablesContainer{$T},
            $func_typed,
        })
    else
        :(GenericModel{
            $T,
            ObjectiveContainer{$T},
            VariablesContainer{$T},
            $func_typed,
        })
    end
    model_code = :(const $esc_model_name{$T} = $generic)
    expr = Expr(:block)
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
    push!(expr.args, model_code)
    return expr
end

for (loop_name, loop_super_type) in [
    (:GenericModel, :AbstractModelLike),
    (:GenericOptimizer, :AbstractOptimizer),
]
    global name = loop_name
    global super_type = loop_super_type
    @eval begin
        """
            mutable struct $name{T,O,V,C} <: $super_type{T}

        Implements a model supporting coefficients of type `T` and:

         * An objective function stored in `.objective::O`
         * Variables and `SingleVariable` constraints stored in `.variable_bounds::V`
         * `F`-in-`S` constraints (excluding `SingleVariable` constraints)
           stored in `.constraints::C`

        All interactions should take place via the MOI interface, so the types
        `O`, `V`, and `C` should implement the API as needed for their
        functionality.
        """
        mutable struct $name{T,O,V,C} <: $super_type{T}
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
            # _not_ copied between models!
            ext::Dict{Symbol,Any}
            function $name{T,O,V,C}() where {T,O,V,C}
                return new{T,O,V,C}(
                    EMPTYSTRING,
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
    end
end

const LessThanIndicatorOne{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}
const LessThanIndicatorZero{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{T}}

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
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.Complements,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
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
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
    ),
    (
        MOI.PowerCone,
        MOI.DualPowerCone,
        MOI.SOS1,
        MOI.SOS2,
        LessThanIndicatorOne,
        LessThanIndicatorZero,
    ),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

@doc raw"""

An implementation of `ModelLike` that supports all functions and sets defined
in MOI. It is parameterized by the coefficient type.

# Examples
```jl
model = Model{Float64}()
x = add_variable(model)
```
"""
Model

# This export makes the type be printed as:
# ```julia
# julia> Base.show(IOContext(stdout, :compact => true), MathOptInterface.Utilities.Model)
# Model{T} where T
# julia> print(MathOptInterface.Utilities.Model)
# MathOptInterface.Utilities.Model{T} where T
# julia> MathOptInterface.Utilities.Model
# MathOptInterface.Utilities.Model{T} where T (alias for MathOptInterface.Utilities.GenericModel{T, MathOptInterface.Utilities.ObjectiveContainer{T}, MathOptInterface.Utilities.VariablesContainer{T}, MathOptInterface.Utilities.ModelFunctionConstraints{T}} where T)
# ```
# As MOI is not doing `using .Utilities` and is not exporting `Model`, the user
# still needs to do `MOI.Utilities.Model` unless he does
# `using MathOptInterface.Utilities`.
export Model
