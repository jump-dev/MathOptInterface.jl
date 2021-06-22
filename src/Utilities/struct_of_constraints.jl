abstract type StructOfConstraints <: MOI.ModelLike end

function _add_variable(model::StructOfConstraints)
    model.num_variables += 1
    return broadcastcall(_add_variable, model)
end
function _add_variables(model::StructOfConstraints, n)
    model.num_variables += n
    return broadcastcall(Base.Fix2(_add_variables, n), model)
end

function final_touch(::Nothing, index_map) end
function final_touch(model::StructOfConstraints, index_map)
    broadcastcall(Base.Fix2(final_touch, index_map), model)
    return
end

function _throw_if_cannot_delete(model::StructOfConstraints, vis, fast_in_vis)
    broadcastcall(model) do constrs
        if constrs !== nothing
            _throw_if_cannot_delete(constrs, vis, fast_in_vis)
        end
        return
    end
    return
end

function _deleted_constraints(
    callback::Function,
    model::StructOfConstraints,
    vi,
)
    broadcastcall(model) do constrs
        if constrs !== nothing
            _deleted_constraints(callback, constrs, vi)
        end
        return
    end
    return
end

function MOI.add_constraint(
    model::StructOfConstraints,
    func::F,
    set::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if !MOI.supports_constraint(model, F, S)
        throw(MOI.UnsupportedConstraint{F,S}())
    end
    return MOI.add_constraint(constraints(model, F, S), func, set)
end

function constraints(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if !MOI.supports_constraint(model, F, S)
        throw(MOI.InvalidIndex(ci))
    end
    return constraints(model, F, S)
end

function MOI.get(
    model::StructOfConstraints,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
)
    return MOI.get(constraints(model, ci), attr, ci)
end

function MOI.delete(model::StructOfConstraints, ci::MOI.ConstraintIndex)
    MOI.delete(constraints(model, ci), ci)
    return
end

function MOI.is_valid(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if !MOI.supports_constraint(model, F, S)
        return false
    end
    return MOI.is_valid(constraints(model, ci), ci)
end

function MOI.modify(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    MOI.modify(constraints(model, ci), ci, change)
    return
end

function MOI.set(
    model::StructOfConstraints,
    attr::Union{MOI.ConstraintFunction,MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
    func_or_set,
)
    MOI.set(constraints(model, ci), attr, ci, func_or_set)
    return
end

function MOI.get(
    model::StructOfConstraints,
    attr::MOI.ListOfConstraintTypesPresent,
)
    return broadcastvcat(model) do constrs
        if constrs === nothing
            return Tuple{DataType,DataType}[]
        end
        return MOI.get(constrs, attr)
    end
end

function MOI.get(
    model::StructOfConstraints,
    attr::MOI.NumberOfConstraints{F,S},
) where {F,S}
    if !MOI.supports_constraint(model, F, S)
        return 0
    end
    return MOI.get(constraints(model, F, S), attr)
end

function MOI.get(
    model::StructOfConstraints,
    attr::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    if !MOI.supports_constraint(model, F, S)
        return MOI.ConstraintIndex{F,S}[]
    end
    return MOI.get(constraints(model, F, S), attr)
end

function MOI.is_empty(model::StructOfConstraints)
    return mapreduce_constraints(&, model, true) do constrs
        return constrs === nothing || MOI.is_empty(constrs)
    end
end

function MOI.empty!(model::StructOfConstraints)
    model.num_variables = 0
    broadcastcall(model) do constrs
        if constrs !== nothing
            MOI.empty!(constrs)
        end
        return
    end
    return
end

# Can be used to access constraints of a model
"""
    broadcastcall(f::Function, model::StructOfConstraints)

Calls `f(contrs)` for every field in `model`.
"""
function broadcastcall end

"""
    broadcastvcat(f::Function, model::StructOfConstraints)

Calls `f(contrs)` for every field in `model` and `vcat`s the results.
"""
function broadcastvcat end

"""
    mapreduce_constraints(
        f::Function,
        op::Function,
        model::StructOfConstraints,
        init,
    )

Call `mapreduce` on every field of `model` given an initial value `init`. Each
element in the map is computed as `f(x)` and the elements are reduced using
`op`.
"""
function mapreduce_constraints end

# Macro code

abstract type SymbolFS end

struct SymbolFun <: SymbolFS
    s::Union{Symbol,Expr}
    typed::Bool
end
SymbolFun(s::Symbol) = SymbolFun(s, false)
function SymbolFun(s::Expr)
    if Meta.isexpr(s, :curly)
        @assert length(s.args) == 2
        @assert s.args[2] == :T
        return SymbolFun(s.args[1], true)
    else
        return SymbolFun(s, false)
    end
end

struct SymbolSet <: SymbolFS
    s::Union{Symbol,Expr}
    typed::Bool
end
SymbolSet(s::Symbol) = SymbolSet(s, false)
function SymbolSet(s::Expr)
    if Meta.isexpr(s, :curly)
        @assert length(s.args) == 2
        @assert s.args[2] == :T
        return SymbolSet(s.args[1], true)
    else
        return SymbolSet(s, false)
    end
end

_typed(s::SymbolFS) = s.typed ? Expr(:curly, esc(s.s), esc(:T)) : esc(s.s)

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
import Unicode

function _field(s::SymbolFS)
    return Symbol(replace(Unicode.lowercase(string(s.s)), "." => "_"))
end

_callfield(f, s::SymbolFS) = :($f(model.$(_field(s))))

_mapreduce_field(s::SymbolFS) = :(cur = op(cur, f(model.$(_field(s)))))

"""
    struct_of_constraint_code(struct_name, types, field_types = nothing)

Given a vector of `n` `SymbolFun` or `SymbolSet` in `types`, defines
a subtype of `StructOfConstraints` of name `name` and which type parameters
`{T, F1, F2, ..., Fn}` if `field_types` is `nothing` and
a `{T}` otherwise.
It contains `n` field where the `i`th field has type `Ci` if `field_types` is
`nothing` and type `field_types[i]` otherwise.
If `types` is vector of `SymbolFun` (resp. `SymbolSet`) then the constraints
of that function (resp. set) type are stored in the corresponding field.

This function is used by the macros [`@model`](@ref),
[`@struct_of_constraints_by_function_types`](@ref) and
[`@struct_of_constraints_by_set_types`](@ref).
"""
function struct_of_constraint_code(struct_name, types, field_types = nothing)
    T = esc(:T)
    typed_struct = :($(struct_name){$T})
    type_parametrized = field_types === nothing
    if type_parametrized
        field_types = [Symbol("C$i") for i in eachindex(types)]
        append!(typed_struct.args, field_types)
    end
    code = quote
        mutable struct $typed_struct <: StructOfConstraints
            num_variables::Int64
        end

        function $MOIU.broadcastcall(f::Function, model::$struct_name)
            $(Expr(:block, _callfield.(Ref(:f), types)...))
            return
        end

        function $MOIU.broadcastvcat(f::Function, model::$struct_name)
            return vcat($(_callfield.(Ref(:f), types)...))
        end

        function $MOIU.mapreduce_constraints(
            f::Function,
            op::Function,
            model::$struct_name,
            cur,
        )
            return $(Expr(:block, _mapreduce_field.(types)...))
        end
    end

    for (t, field_type) in zip(types, field_types)
        field = _field(t)
        push!(code.args[2].args[3].args, :($field::Union{Nothing,$field_type}))
        fun = t isa SymbolFun ? esc(t.s) : :(MOI.AbstractFunction)
        set = t isa SymbolFun ? :(MOI.AbstractSet) : esc(t.s)
        constraints_code = :(
            function $MOIU.constraints(
                model::$typed_struct,
                ::Type{<:$fun},
                ::Type{<:$set},
            )::$(field_type) where {$T}
                if model.$field === nothing
                    model.$field = $(field_type)()
                    $MOI.Utilities._add_variables(
                        model.$field,
                        model.num_variables,
                    )
                end
                return model.$field
            end
        )
        if type_parametrized
            append!(constraints_code.args[1].args, field_types)
        end
        push!(code.args, constraints_code)
    end
    is_func = eltype(types) <: SymbolFun
    SuperF = is_func ? :(Union{$(_typed.(types)...)}) : :(MOI.AbstractFunction)
    SuperS = is_func ? :(MOI.AbstractSet) : :(Union{$(_typed.(types)...)})
    push!(
        code.args,
        :(
            function $MOI.supports_constraint(
                model::$struct_name{$T},
                ::Type{F},
                ::Type{S},
            ) where {$T,F<:$SuperF,S<:$SuperS}
                return $MOI.supports_constraint(constraints(model, F, S), F, S)
            end
        ),
    )
    constructor_code = :(function $typed_struct() where {$T}
        return $typed_struct(0, $([:(nothing) for _ in field_types]...))
    end)
    if type_parametrized
        append!(constructor_code.args[1].args, field_types)
    end
    push!(code.args, constructor_code)
    return code
end

"""
    Utilities.@struct_of_constraints_by_function_types(name, func_types...)

Given a vector of `n` function types `(F1, F2,..., Fn)` in `func_types`, defines
a subtype of `StructOfConstraints` of name `name` and which type parameters
`{T, C1, C2, ..., Cn}`.
It contains `n` field where the `i`th field has type `Ci` and stores the
constraints of function type `Fi`.
"""
macro struct_of_constraints_by_function_types(name, func_types...)
    funcs = SymbolFun.(func_types)
    return struct_of_constraint_code(esc(name), funcs)
end

"""
    Utilities.@struct_of_constraints_by_set_types(name, func_types...)

Given a vector of `n` set types `(S1, S2,..., Sn)` in `func_types`, defines
a subtype of `StructOfConstraints` of name `name` and which type parameters
`{T, C1, C2, ..., Cn}`.
It contains `n` field where the `i`th field has type `Ci` and stores the
constraints of set type `Si`.
"""
macro struct_of_constraints_by_set_types(name, set_types...)
    sets = SymbolSet.(set_types)
    return struct_of_constraint_code(esc(name), sets)
end
