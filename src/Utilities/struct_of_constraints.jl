abstract type StructOfConstraints <: MOI.ModelLike end

function _throw_if_cannot_delete(model::StructOfConstraints, vis, fast_in_vis)
    broadcastcall(model) do constrs
        _throw_if_cannot_delete(constrs, vis, fast_in_vis)
    end
end
function _deleted_constraints(callback::Function, model::StructOfConstraints, vi)
    broadcastcall(model) do constrs
        _deleted_constraints(callback, constrs, vi)
    end
end

function MOI.add_constraint(model::StructOfConstraints, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    if MOI.supports_constraint(model, typeof(func), typeof(set))
        return MOI.add_constraint(constraints(model, typeof(func), typeof(set)), func, set)
    else
        throw(MOI.UnsupportedConstraint{typeof(func),typeof(set)}())
    end
end

function constraints(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex{F,S}
) where {F,S}
    if !MOI.supports_constraint(model, F, S)
        throw(MOI.InvalidIndex(ci))
    end
    return constraints(model, F, S)
end
function MOI.get(model::StructOfConstraints, attr::Union{MOI.ConstraintFunction, MOI.ConstraintSet}, ci::MOI.ConstraintIndex)
    return MOI.get(constraints(model, ci), attr, ci)
end

function MOI.delete(model::StructOfConstraints, ci::MOI.ConstraintIndex)
    return MOI.delete(constraints(model, ci), ci)
end

function MOI.is_valid(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex{F,S}
) where {F,S}
    if MOI.supports_constraint(model, F, S)
        return MOI.is_valid(constraints(model, ci), ci)
    else
        return false
    end
end

function MOI.modify(
    model::StructOfConstraints,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    return MOI.modify(constraints(model, ci), ci, change)
end

function MOI.set(
    model::StructOfConstraints,
    attr::Union{MOI.ConstraintFunction, MOI.ConstraintSet},
    ci::MOI.ConstraintIndex,
    func_or_set,
)
    return MOI.set(constraints(model, ci), attr, ci, func_or_set)
end


function MOI.get(model::StructOfConstraints, loc::MOI.ListOfConstraints) where {T}
    return broadcastvcat(model) do v
        MOI.get(v, loc)
    end
end

function MOI.get(model::StructOfConstraints, noc::MOI.NumberOfConstraints{F,S}) where {F,S}
    if MOI.supports_constraint(model, F, S)
        return MOI.get(constraints(model, F, S), noc)
    else
        return 0
    end
end

function MOI.get(model::StructOfConstraints, loc::MOI.ListOfConstraintIndices{F,S}) where {F,S}
    if MOI.supports_constraint(model, F, S)
        return MOI.get(constraints(model, F, S), loc)
    else
        return MOI.ConstraintIndex{F,S}[]
    end
end

function MOI.is_empty(model::StructOfConstraints)
    return mapreduce_constraints(MOI.is_empty, &, model, true)
end
function MOI.empty!(model::StructOfConstraints)
    broadcastcall(MOI.empty!, model)
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

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model and concatenate the results with `vcat` (this is used internally for `ListOfConstraints`).

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

# QuoteNode prevents s from being interpolated and keeps it as a symbol
# Expr(:., MOI, s) would be MOI.s
# Expr(:., MOI, $s) would be Expr(:., MOI, EqualTo)
# Expr(:., MOI, :($s)) would be Expr(:., MOI, :EqualTo)
# Expr(:., MOI, :($(QuoteNode(s)))) is Expr(:., MOI, :(:EqualTo)) <- what we want

# (MOI, :Zeros) -> :(MOI.Zeros)
# (:Zeros) -> :(MOI.Zeros)
_set(s::SymbolSet) = esc(s.s)
_fun(s::SymbolFun) = esc(s.s)
function _typedset(s::SymbolSet)
    if s.typed
        T = esc(:T)
        :($(_set(s)){$T})
    else
        _set(s)
    end
end
function _typedfun(s::SymbolFun)
    if s.typed
        T = esc(:T)
        :($(_fun(s)){$T})
    else
        _fun(s)
    end
end

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
using Unicode

_field(s::SymbolFS) = Symbol(replace(lowercase(string(s.s)), "." => "_"))

_getC(s::SymbolSet) = :(VectorOfConstraints{F,$(_typedset(s))})
_getC(s::SymbolFun) = _typedfun(s)

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
    esc_struct_name = struct_name
    T = esc(:T)
    typed_struct = :($(esc_struct_name){$T})
    type_parametrized = field_types === nothing
    if type_parametrized
        field_types = [Symbol("C$i") for i in eachindex(types)]
        append!(typed_struct.args, field_types)
    end

    struct_def = :(struct $typed_struct <: StructOfConstraints
    end)

    for (t, field_type) in zip(types, field_types)
        field = _field(t)
        push!(struct_def.args[3].args, :($field::$field_type))
    end
    code = quote
        function $MOIU.broadcastcall(f::Function, model::$esc_struct_name)
            return $(Expr(:block, _callfield.(Ref(:f), types)...))
        end
        function $MOIU.broadcastvcat(f::Function, model::$esc_struct_name)
            return vcat($(_callfield.(Ref(:f), types)...))
        end
        function $MOIU.mapreduce_constraints(f::Function, op::Function, model::$esc_struct_name, cur)
            return $(Expr(:block, _mapreduce_field.(types)...))
        end
    end

    for t in types
        if t isa SymbolFun
            fun = _fun(t)
            set = :(MOI.AbstractSet)
        else
            fun = :(MOI.AbstractFunction)
            set = _set(t)
        end
        field = _field(t)
        code = quote
            $code
            function $MOIU.constraints(
                model::$esc_struct_name,
                ::Type{<:$fun},
                ::Type{<:$set},
            ) where S
                return model.$field
            end
        end
    end
    supports_code = if eltype(types) <: SymbolFun
        quote
            function $MOI.supports_constraint(
                model::$esc_struct_name{$T},
                ::Type{F},
                ::Type{S},
            ) where {$T, F<:Union{$(_typedfun.(types)...)}, S<:MOI.AbstractSet}
                return $MOI.supports_constraint(constraints(model, F, S), F, S)
            end
        end
    else
        @assert eltype(types) <: SymbolSet
        quote
            function $MOI.supports_constraint(
                model::$esc_struct_name{$T},
                ::Type{F},
                ::Type{S},
            ) where {$T, F<:MOI.AbstractFunction, S<:Union{$(_typedset.(types)...)}}
                return $MOI.supports_constraint(constraints(model, F, S), F, S)
            end
        end
    end
    expr = Expr(
        :block,
        struct_def,
        supports_code,
        code,
    )
    if !isempty(field_types)
        constructors = [:($field_type()) for field_type in field_types]
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
