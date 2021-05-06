abstract type StructOfConstraints <: MOI.ModelLike end

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
broadcastcall(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model.

# Examples

To add all constraints of the model to a solver `solver`, one can do
```julia
_addcon(solver, ci, f, s) = MOI.add_constraint(solver, f, s)
function _addcon(solver, constrs::Vector)
    for constr in constrs
        _addcon(solver, constr...)
    end
end
MOIU.broadcastcall(constrs -> _addcon(solver, constrs), model)
```
"""
function broadcastcall end

"""
broadcastvcat(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model and concatenate the results with `vcat` (this is used internally for `ListOfConstraintTypesPresent`).

# Examples

To get the list of all functions:
```julia
_getfun(ci, f, s) = f
_getfun(cindices::Tuple) = _getfun(cindices...)
_getfuns(constrs::Vector) = _getfun.(constrs)
MOIU.broadcastvcat(_getfuns, model)
```
"""
function broadcastvcat end

function mapreduce_constraints end

# Macro code

abstract type SymbolFS end
struct SymbolFun <: SymbolFS
    s::Union{Symbol,Expr}
    typed::Bool
end
struct SymbolSet <: SymbolFS
    s::Union{Symbol,Expr}
    typed::Bool
end

_typedset(s::SymbolSet) = s.typed ? Expr(:curly, esc(s.s), esc(:T)) : esc(s.s)
_typedfun(s::SymbolFun) = s.typed ? Expr(:curly, esc(s.s), esc(:T)) : esc(s.s)

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
import Unicode

function _field(s::SymbolFS)
    return Symbol(replace(Unicode.lowercase(string(s.s)), "." => "_"))
end

_callfield(f, s::SymbolFS) = :($f(model.$(_field(s))))
_broadcastfield(b, s::SymbolFS) = :($b(f, model.$(_field(s))))
_mapreduce_field(s::SymbolFS) = :(cur = op(cur, f(model.$(_field(s)))))
_mapreduce_constraints(s::SymbolFS) = :(cur = op(cur, f(model.$(_field(s)))))

"""
    struct_of_constraint_code(struct_name, types, field_types = nothing)

Given a vector of `n` `SymbolFun` or `SymbolSet` in `types`, creates a
struct of name `struct_name` that is a subtype of
`StructOfConstraint{T, C1, C2, ..., Cn}` if `field_types` is `nothing` and
a subtype of `StructOfConstraint{T}` otherwise.
It contains `n` field where the `i`th field has type `Ci` if `field_types` is
`nothing` and type `field_types[i]` otherwise.
If `types` is vector of `SymbolFun` (resp. `SymbolSet`) then the constraints
of that function (resp. set) type are stored in the corresponding field.
"""
function struct_of_constraint_code(struct_name, types, field_types = nothing)
    T = esc(:T)
    typed_struct = :($(struct_name){$T})
    type_parametrized = field_types === nothing
    if type_parametrized
        field_types = [Symbol("C$i") for i in eachindex(types)]
        append!(typed_struct.args, field_types)
    end
    struct_def = :(mutable struct $typed_struct <: StructOfConstraints end)

    code = quote
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
        push!(struct_def.args[3].args, :($field::Union{Nothing,$field_type}))
        fun = t isa SymbolFun ? esc(t.s) : :(MOI.AbstractFunction)
        set = t isa SymbolFun ? :(MOI.AbstractSet) : esc(t.s)
        constraints_code = :(
            function $MOIU.constraints(
                model::$typed_struct,
                ::Type{<:$fun},
                ::Type{<:$set},
            ) where {$T}
                if model.$field === nothing
                    model.$field = $(field_type)()
                end
                return model.$field
            end
        )
        if type_parametrized
            append!(constraints_code.args[1].args, field_types)
        end
        push!(code.args, constraints_code)
    end
    supports_code = if eltype(types) <: SymbolFun
        quote
            function $MOI.supports_constraint(
                model::$struct_name{$T},
                ::Type{F},
                ::Type{S},
            ) where {
                $T,
                F<:Union{$(_typedfun.(types)...)},
                S<:MOI.AbstractSet,
            }
                return $MOI.supports_constraint(constraints(model, F, S), F, S)
            end
        end
    else
        @assert eltype(types) <: SymbolSet
        quote
            function $MOI.supports_constraint(
                model::$struct_name{$T},
                ::Type{F},
                ::Type{S},
            ) where {
                $T,
                F<:MOI.AbstractFunction,
                S<:Union{$(_typedset.(types)...)},
            }
                return $MOI.supports_constraint(constraints(model, F, S), F, S)
            end
        end
    end
    expr = Expr(:block, struct_def, supports_code, code)
    if !isempty(field_types)
        constructors = [:(nothing) for field_type in field_types]
        # constructors = [:($field_type()) for field_type in field_types]
        # If there is no field type, the default constructor is sufficient and
        # adding this constructor will make a `StackOverflow`.
        constructor_code = :(function $typed_struct() where {$T}
            return $typed_struct($(constructors...))
        end)
        if type_parametrized
            append!(constructor_code.args[1].args, field_types)
        end
        push!(expr.args, constructor_code)
    end
    return expr
end
