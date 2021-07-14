using Base.Meta: isexpr

struct _ParsedScalarAffineTerm
    coefficient::Float64
    variable::Symbol
end

struct _ParsedScalarAffineFunction
    terms::Vector{_ParsedScalarAffineTerm}
    constant::Float64
end

struct _ParsedVectorAffineTerm
    output_index::Int64
    scalar_term::_ParsedScalarAffineTerm
end

struct _ParsedVectorAffineFunction
    terms::Vector{_ParsedVectorAffineTerm}
    constant::Vector{Float64}
end

struct _ParsedScalarQuadraticTerm
    coefficient::Float64
    variable_1::Symbol
    variable_2::Symbol
end

struct _ParsedScalarQuadraticFunction
    affine_terms::Vector{_ParsedScalarAffineTerm}
    quadratic_terms::Vector{_ParsedScalarQuadraticTerm}
    constant::Float64
end

struct _ParsedVectorQuadraticTerm
    output_index::Int64
    scalar_term::_ParsedScalarQuadraticTerm
end

struct _ParsedVectorQuadraticFunction
    affine_terms::Vector{_ParsedVectorAffineTerm}
    quadratic_terms::Vector{_ParsedVectorQuadraticTerm}
    constant::Vector{Float64}
end

struct _ParsedSingleVariable
    variable::Symbol
end

struct _ParsedVectorOfVariables
    variables::Vector{Symbol}
end

# Not written with any considerations for performance
function _parse_function(ex)
    if isa(ex, Symbol)
        return _ParsedSingleVariable(ex)
    elseif isexpr(ex, :vect)
        if all(s -> isa(s, Symbol), ex.args)
            return _ParsedVectorOfVariables(copy(ex.args))
        else
            singlefunctions = _parse_function.(ex.args)
            affine_terms = _ParsedVectorAffineTerm[]
            quadratic_terms = _ParsedVectorQuadraticTerm[]
            constant = Float64[]
            for (outindex, f) in enumerate(singlefunctions)
                if isa(f, _ParsedSingleVariable)
                    push!(
                        affine_terms,
                        _ParsedVectorAffineTerm(
                            outindex,
                            _ParsedScalarAffineTerm(1.0, f.variable),
                        ),
                    )
                    push!(constant, 0.0)
                elseif isa(f, _ParsedScalarAffineFunction)
                    append!(
                        affine_terms,
                        _ParsedVectorAffineTerm.(outindex, f.terms),
                    )
                    push!(constant, f.constant)
                else
                    @assert isa(f, _ParsedScalarQuadraticFunction)
                    append!(
                        affine_terms,
                        _ParsedVectorAffineTerm.(outindex, f.affine_terms),
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
                return _ParsedVectorAffineFunction(affine_terms, constant)
            else
                return _ParsedVectorQuadraticFunction(
                    affine_terms,
                    quadratic_terms,
                    constant,
                )
            end
        end
    else
        # only accept Expr(:call, :+, ...), no recursive expressions
        # TODO: generalize. x - y + z would be useful
        if isexpr(ex, :call) && ex.args[1] == :*
            # handle 2x as (+)(2x)
            ex = Expr(:call, :+, ex)
        end
        if ex isa Number
            ex = Expr(:call, :+, ex)
        end
        @assert isexpr(ex, :call)
        if ex.args[1] != :+
            error("Expected `+`, got `$(ex.args[1])`.")
        end
        affine_terms = _ParsedScalarAffineTerm[]
        quadratic_terms = _ParsedScalarQuadraticTerm[]
        constant = 0.0
        for subex in ex.args[2:end]
            if isexpr(subex, :call) && subex.args[1] == :*
                if length(subex.args) == 3
                    # constant * variable
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    push!(
                        affine_terms,
                        _ParsedScalarAffineTerm(subex.args[2], subex.args[3]),
                    )
                else
                    # constant * variable * variable for quadratic
                    @assert length(subex.args) == 4 "Multiplication with more than three terms not supported"
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    @assert isa(subex.args[4], Symbol)
                    if subex.args[3] == subex.args[4]
                        coefficient = 2 * subex.args[2]
                    else
                        coefficient = subex.args[2]
                    end
                    push!(
                        quadratic_terms,
                        _ParsedScalarQuadraticTerm(
                            coefficient,
                            subex.args[3],
                            subex.args[4],
                        ),
                    )
                end
            elseif isa(subex, Symbol)
                push!(affine_terms, _ParsedScalarAffineTerm(1.0, subex))
            else
                @assert isa(subex, Number)
                constant += subex
            end
        end
        if length(quadratic_terms) == 0
            return _ParsedScalarAffineFunction(affine_terms, constant)
        else
            return _ParsedScalarQuadraticFunction(
                affine_terms,
                quadratic_terms,
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

_parsed_to_moi(model, s::Union{Float64,Int64}) = s

for typename in [
    :_ParsedScalarAffineTerm,
    :_ParsedScalarAffineFunction,
    :_ParsedVectorAffineTerm,
    :_ParsedVectorAffineFunction,
    :_ParsedScalarQuadraticTerm,
    :_ParsedScalarQuadraticFunction,
    :_ParsedVectorQuadraticTerm,
    :_ParsedVectorQuadraticFunction,
    :_ParsedSingleVariable,
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

# Ideally, this should be load_from_string
"""
    loadfromstring!(model, s)

A utility function to aid writing tests.

## WARNING

This function is not intended for widespread use! It is mainly used as a tool to
simplify writing tests in MathOptInterface. Do not use it as an exchange format
for storing or transmitting problem instances. Use the FileFormats submodule
instead.

## Example

```
MOI.Utilities.loadfromstring!(
    model,
    \"\"\"
    variables: x, y, z
    minobjective: 2x + 3y
    con1: x + y <= 1
    con2: [x, y] in MOI.Nonnegatives(2)
    x >= 0.0
    \"\"\"
)
```

## Notes

Special labels are:
 - variables
 - minobjective
 - maxobjectives
Everything else denotes a constraint with a name.

Do not name `SingleVariable` constraints.

## Exceptions

 * `x - y` does NOT currently parse. Instead, write `x + -1.0 * y`.
 * `x^2` does NOT currently parse. Instead, write `x * x`.
"""
function loadfromstring!(model, s)
    parsedlines = filter(ex -> ex !== nothing, Meta.parse.(split(s, "\n")))
    for line in parsedlines
        label, ex = _separate_label(line)
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
        elseif label == :maxobjective
            f = _parsed_to_moi(model, _parse_function(ex))
            MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        elseif label == :minobjective
            f = _parsed_to_moi(model, _parse_function(ex))
            MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
        else
            # constraint
            @assert isexpr(ex, :call)
            f = _parsed_to_moi(model, _parse_function(ex.args[2]))
            if ex.args[1] == :in
                # Could be safer here
                set = Core.eval(MOI, ex.args[3])
            elseif ex.args[1] == :<=
                set = MOI.LessThan(ex.args[3])
            elseif ex.args[1] == :>=
                set = MOI.GreaterThan(ex.args[3])
            elseif ex.args[1] == :(==)
                set = MOI.EqualTo(ex.args[3])
            else
                error("Unrecognized expression $ex")
            end
            F, S = typeof(f), typeof(set)
            if !MOI.supports_constraint(model, F, S)
                throw(MOI.UnsupportedConstraint{F,S}())
            end
            cindex = MOI.add_constraint(model, f, set)
            if F != MOI.SingleVariable
                MOI.set(model, MOI.ConstraintName(), cindex, String(label))
            end
        end
    end
end
