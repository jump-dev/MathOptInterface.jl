# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Base.Meta: isexpr

struct _ParsedScalarAffineTerm{T}
    coefficient::T
    variable::Symbol
end

struct _ParsedScalarAffineFunction{T}
    terms::Vector{_ParsedScalarAffineTerm{T}}
    constant::T
end

struct _ParsedVectorAffineTerm{T}
    output_index::Int64
    scalar_term::_ParsedScalarAffineTerm{T}
end

struct _ParsedVectorAffineFunction{T}
    terms::Vector{_ParsedVectorAffineTerm{T}}
    constant::Vector{T}
end

struct _ParsedScalarQuadraticTerm{T}
    coefficient::T
    variable_1::Symbol
    variable_2::Symbol
end

struct _ParsedScalarQuadraticFunction{T}
    quadratic_terms::Vector{_ParsedScalarQuadraticTerm{T}}
    affine_terms::Vector{_ParsedScalarAffineTerm{T}}
    constant::T
end

struct _ParsedVectorQuadraticTerm{T}
    output_index::Int64
    scalar_term::_ParsedScalarQuadraticTerm{T}
end

struct _ParsedVectorQuadraticFunction{T}
    quadratic_terms::Vector{_ParsedVectorQuadraticTerm{T}}
    affine_terms::Vector{_ParsedVectorAffineTerm{T}}
    constant::Vector{T}
end

struct _ParsedVariableIndex
    variable::Symbol
end

struct _ParsedVectorOfVariables
    variables::Vector{Symbol}
end

# Not written with any considerations for performance
function _parse_function(ex, ::Type{T} = Float64) where {T}
    if isa(ex, Symbol)
        return _ParsedVariableIndex(ex)
    elseif isexpr(ex, :vect)
        if all(s -> isa(s, Symbol), ex.args)
            return _ParsedVectorOfVariables(copy(ex.args))
        else
            singlefunctions = _parse_function.(ex.args, T)
            affine_terms = _ParsedVectorAffineTerm{T}[]
            quadratic_terms = _ParsedVectorQuadraticTerm{T}[]
            constant = T[]
            for (outindex, f) in enumerate(singlefunctions)
                outindex = Int64(outindex)
                if isa(f, _ParsedVariableIndex)
                    push!(
                        affine_terms,
                        _ParsedVectorAffineTerm{T}(
                            outindex,
                            _ParsedScalarAffineTerm{T}(one(T), f.variable),
                        ),
                    )
                    push!(constant, zero(T))
                elseif isa(f, _ParsedScalarAffineFunction{T})
                    append!(
                        affine_terms,
                        _ParsedVectorAffineTerm{T}.(outindex, f.terms),
                    )
                    push!(constant, f.constant)
                else
                    @assert isa(f, _ParsedScalarQuadraticFunction{T})
                    append!(
                        affine_terms,
                        _ParsedVectorAffineTerm{T}.(outindex, f.affine_terms),
                    )
                    append!(
                        quadratic_terms,
                        _ParsedVectorQuadraticTerm.(
                            outindex,
                            f.quadratic_terms,
                        ),
                    )
                    push!(constant, f.constant)
                end
            end
            if length(quadratic_terms) == 0
                return _ParsedVectorAffineFunction{T}(affine_terms, constant)
            else
                return _ParsedVectorQuadraticFunction{T}(
                    quadratic_terms,
                    affine_terms,
                    constant,
                )
            end
        end
    else
        # For simplicity, only accept Expr(:call, :+, ...); no recursive
        # expressions
        if isexpr(ex, :call) && ex.args[1] == :*
            ex = Expr(:call, :+, ex)  # Handle 2x as (+)(2x)
        end
        if ex isa Number
            ex = Expr(:call, :+, ex)
        end
        @assert isexpr(ex, :call)
        if ex.args[1] != :+
            error(
                "Unsupported operator in `loadfromstring!`: `$(ex.args[1])`. " *
                "The parser is deliberately limited in the syntax it " *
                "accepts. Write `x - y` as `x + -1 * y`,  and `x^2` as " *
                "`x * x`.",
            )
        end
        affine_terms = _ParsedScalarAffineTerm{T}[]
        quadratic_terms = _ParsedScalarQuadraticTerm{T}[]
        constant = zero(T)
        for subex in ex.args[2:end]
            if isexpr(subex, :call) && subex.args[1] == :*
                if length(subex.args) == 3
                    # constant * variable
                    coef = if isa(subex.args[2], Number)
                        subex.args[2]
                    else
                        Core.eval(Base, subex.args[2])
                    end
                    @assert isa(subex.args[3], Symbol)
                    push!(
                        affine_terms,
                        _ParsedScalarAffineTerm{T}(
                            convert(T, coef),
                            subex.args[3],
                        ),
                    )
                else
                    # constant * variable * variable for quadratic
                    @assert length(subex.args) == 4 "Multiplication with more than three terms not supported"
                    coefficient = if isa(subex.args[2], Number)
                        subex.args[2]
                    else
                        Core.eval(Base, subex.args[2])
                    end
                    @assert isa(subex.args[3], Symbol)
                    @assert isa(subex.args[4], Symbol)
                    if subex.args[3] == subex.args[4]
                        coefficient *= T(2)
                    end
                    push!(
                        quadratic_terms,
                        _ParsedScalarQuadraticTerm{T}(
                            coefficient,
                            subex.args[3],
                            subex.args[4],
                        ),
                    )
                end
            elseif isa(subex, Symbol)
                push!(affine_terms, _ParsedScalarAffineTerm{T}(one(T), subex))
            else
                @assert isa(subex, Number)
                constant += subex
            end
        end
        if length(quadratic_terms) == 0
            return _ParsedScalarAffineFunction{T}(affine_terms, constant)
        else
            return _ParsedScalarQuadraticFunction{T}(
                quadratic_terms,
                affine_terms,
                constant,
            )
        end
    end
end

# see tests for examples
function _separate_label(ex)
    if isexpr(ex, :call) && ex.args[1] == :(:)
        # A line like `variables: x`.
        return ex.args[2], ex.args[3]
    elseif isexpr(ex, :tuple)
        # A line like `variables: x, y`. _Parsed as `((variables:x), y)`
        ex = copy(ex)
        @assert isexpr(ex.args[1], :call) && ex.args[1].args[1] == :(:)
        label = ex.args[1].args[2]
        ex.args[1] = ex.args[1].args[3]
        return label, ex
    elseif isexpr(ex, :call)
        ex = copy(ex)
        if isexpr(ex.args[2], :call) && ex.args[2].args[1] == :(:)
            # A line like `c: x <= 1`
            label = ex.args[2].args[2]
            ex.args[2] = ex.args[2].args[3]
            return label, ex
        else
            # A line like `x <= 1`
            return Symbol(""), ex
        end
    else
        error("Unrecognized expression $ex")
    end
end

function _parsed_to_moi(model, s::Symbol)
    index = MOI.get(model, MOI.VariableIndex, String(s))
    if index === nothing
        error("Invalid variable name $s.")
    end
    return index
end

# Used for Vector{Symbol}, Vector{_ParsedScalarAffineTerm},
# Vector{_ParsedVectorAffineTerm}, Vector{_ParsedScalarQuadraticTerm} and
# Vector{_ParsedVectorQuadraticTerm}.
_parsed_to_moi(model, s::Vector) = _parsed_to_moi.(model, s)

_parsed_to_moi(model, s::Number) = s

for typename in [
    :_ParsedScalarAffineTerm,
    :_ParsedScalarAffineFunction,
    :_ParsedVectorAffineTerm,
    :_ParsedVectorAffineFunction,
    :_ParsedScalarQuadraticTerm,
    :_ParsedScalarQuadraticFunction,
    :_ParsedVectorQuadraticTerm,
    :_ParsedVectorQuadraticFunction,
    :_ParsedVectorOfVariables,
]
    moiname = Meta.parse(replace(string(typename), "_Parsed" => "MOI."))
    fields = fieldnames(eval(typename))
    constructor = Expr(
        :call,
        moiname,
        [
            Expr(
                :call,
                :_parsed_to_moi,
                :model,
                Expr(:., :f, Base.Meta.quot(field)),
            ) for field in fields
        ]...,
    )
    @eval _parsed_to_moi(model, f::$typename) = $constructor
end

function _parsed_to_moi(model, f::_ParsedVariableIndex)
    return _parsed_to_moi(model, f.variable)
end

# Ideally, this should be load_from_string
"""
    loadfromstring!(model, s)

A utility function to aid writing tests.

!!! warning

This function is not intended for widespread use! It is mainly used as a tool to
simplify writing tests in MathOptInterface. Do not use it as an exchange format
for storing or transmitting problem instances. Use the FileFormats submodule
instead.

## Example

```jldoctest; setup=:(using MathOptInterface; const MOI = MathOptInterface)
julia> model = MOI.Utilities.Model{Float64}();

julia> MOI.Utilities.loadfromstring!(model, \"\"\"
       variables: x, y, z
       constrainedvariable: [a, b, c] in Nonnegatives(3)
       minobjective::Float64: 2x + 3y
       con1: x + y <= 1.0
       con2: [x, y] in Nonnegatives(2)
       x >= 0.0
       \"\"\")
```

## Notes

Special labels are:

 - variables
 - minobjective
 - maxobjectives

Everything else denotes a constraint with a name.

Append `::T` to use an element type of `T` when parsing the function.

Do not name `VariableIndex` constraints.

## Exceptions

 * `x - y` does NOT currently parse. Instead, write `x + -1.0 * y`.
 * `x^2` does NOT currently parse. Instead, write `x * x`.
"""
function loadfromstring!(model, s)
    parsedlines = filter(ex -> ex !== nothing, Meta.parse.(split(s, "\n")))
    for line in parsedlines
        label, ex = _separate_label(line)
        T, label = _split_type(label)
        if label == :variables
            if isexpr(ex, :tuple)
                for v in ex.args
                    vindex = MOI.add_variable(model)
                    MOI.set(model, MOI.VariableName(), vindex, String(v))
                end
            else
                @assert isa(ex, Symbol)
                vindex = MOI.add_variable(model)
                MOI.set(model, MOI.VariableName(), vindex, String(ex))
            end
        elseif label == :constrainedvariable
            @assert length(ex.args) == 3
            @assert ex.args[1] == :in
            set = Core.eval(MOI, ex.args[3])
            if isa(ex.args[2], Symbol)
                # constrainedvariable: x in LessThan(1.0)
                x, _ = MOI.add_constrained_variable(model, set)
                MOI.set(model, MOI.VariableName(), x, String(ex.args[2]))
            else
                # constraintedvariable: [a, b, c] in Set
                @assert isa(ex.args[2], Expr)
                @assert ex.args[2].head == :vect
                x, _ = MOI.add_constrained_variables(model, set)
                for i in 1:length(x)
                    name = String(ex.args[2].args[i])
                    MOI.set(model, MOI.VariableName(), x[i], name)
                end
            end
        elseif label == :maxobjective
            f = _parsed_to_moi(model, _parse_function(ex, T))
            MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        elseif label == :minobjective
            f = _parsed_to_moi(model, _parse_function(ex, T))
            MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        else
            # constraint
            @assert isexpr(ex, :call)
            f = _parsed_to_moi(model, _parse_function(ex.args[2], T))
            if ex.args[1] == :in
                # Could be safer here
                set = Core.eval(MOI, ex.args[3])
            elseif ex.args[1] == :<=
                set = MOI.LessThan(Core.eval(Base, ex.args[3]))
            elseif ex.args[1] == :>=
                set = MOI.GreaterThan(Core.eval(Base, ex.args[3]))
            elseif ex.args[1] == :(==)
                set = MOI.EqualTo(Core.eval(Base, ex.args[3]))
            else
                error("Unrecognized expression $ex")
            end
            F, S = typeof(f), typeof(set)
            if !MOI.supports_constraint(model, F, S)
                throw(MOI.UnsupportedConstraint{F,S}())
            end
            cindex = MOI.add_constraint(model, f, set)
            if F != MOI.VariableIndex
                MOI.set(model, MOI.ConstraintName(), cindex, String(label))
            end
        end
    end
    return
end

_split_type(ex) = Float64, ex

function _split_type(ex::Expr)
    if isexpr(ex, Symbol("::"), 1)
        return Core.eval(Base, ex.args[1]), Symbol("")
    else
        @assert isexpr(ex, Symbol("::"), 2)
        return Core.eval(Base, ex.args[2]), ex.args[1]
    end
end
