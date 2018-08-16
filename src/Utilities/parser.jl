using Base.Meta: isexpr

# A parser for a simple human-readable of an MOI model.
# This should be thought of as a compact way to write small models
# for tests, and not an exchange format.
#
# variables: x, y, z
# minobjective: 2x + 3y
# con1: x + y <= 1
# con2: [x,y] in Set
#
# special labels: variables, minobjective, maxobjective
# everything else denotes a constraint with a name
# all constraints must be named
# "x - y" does NOT currently parse, needs to be written as "x + -1.0*y"
# "x^2" does NOT currently parse, needs to be written as "x*x"

struct ParsedScalarAffineTerm
    coefficient::Float64
    variable_index::Symbol
end

struct ParsedScalarAffineFunction
    terms::Vector{ParsedScalarAffineTerm}
    constant::Float64
end

struct ParsedVectorAffineTerm
    output_index::Int64
    scalar_term::ParsedScalarAffineTerm
end

struct ParsedVectorAffineFunction
    terms::Vector{ParsedVectorAffineTerm}
    constant::Vector{Float64}
end

struct ParsedScalarQuadraticTerm
    coefficient::Float64
    variable_index_1::Symbol
    variable_index_2::Symbol
end

struct ParsedScalarQuadraticFunction
    affine_terms::Vector{ParsedScalarAffineTerm}
    quadratic_terms::Vector{ParsedScalarQuadraticTerm}
    constant::Float64
end

struct ParsedVectorQuadraticTerm
    output_index::Int64
    scalar_term::ParsedScalarQuadraticTerm
end

struct ParsedVectorQuadraticFunction
    affine_terms::Vector{ParsedVectorAffineTerm}
    quadratic_terms::Vector{ParsedVectorQuadraticTerm}
    constant::Vector{Float64}
end

struct ParsedSingleVariable
    variable::Symbol
end

struct ParsedVectorOfVariables
    variables::Vector{Symbol}
end

# Not written with any considerations for performance
function parsefunction(ex)
    if isa(ex, Symbol)
        return ParsedSingleVariable(ex)
    elseif isexpr(ex, :vect)
        if all(s -> isa(s, Symbol), ex.args)
            return ParsedVectorOfVariables(copy(ex.args))
        else
            singlefunctions = parsefunction.(ex.args)
            affine_terms = ParsedVectorAffineTerm[]
            quadratic_terms = ParsedVectorQuadraticTerm[]
            constant = Float64[]
            for (outindex,f) in enumerate(singlefunctions)
                if isa(f, ParsedSingleVariable)
                    push!(affine_terms, ParsedVectorAffineTerm(outindex, ParsedScalarAffineTerm(1.0, f.variable)))
                    push!(constant, 0.0)
                elseif isa(f, ParsedScalarAffineFunction)
                    append!(affine_terms, ParsedVectorAffineTerm.(outindex, f.terms))
                    push!(constant, f.constant)
                else
                    @assert isa(f, ParsedScalarQuadraticFunction)
                    append!(affine_terms, ParsedVectorAffineTerm.(outindex, f.affine_terms))
                    append!(quadratic_terms, ParsedVectorQuadraticTerm.(outindex, f.quadratic_terms))
                    push!(constant, f.constant)
                end
            end
            if length(quadratic_terms) == 0
                return ParsedVectorAffineFunction(affine_terms, constant)
            else
                return ParsedVectorQuadraticFunction(affine_terms, quadratic_terms, constant)
            end
        end
    else
        # only accept Expr(:call, :+, ...), no recursive expressions
        # TODO: generalize. x - y + z would be useful
        if isexpr(ex, :call) && ex.args[1] == :*
            # handle 2x as (+)(2x)
            ex = Expr(:call,:+,ex)
        end
        if ex isa Number
            ex = Expr(:call,:+,ex)
        end
        @assert isexpr(ex, :call) && ex.args[1] == :+
        affine_terms = ParsedScalarAffineTerm[]
        quadratic_terms = ParsedScalarQuadraticTerm[]
        constant = 0.0
        for subex in ex.args[2:end]
            if isexpr(subex, :call) && subex.args[1] == :*
                if length(subex.args) == 3
                    # constant * variable
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    push!(affine_terms, ParsedScalarAffineTerm(subex.args[2], subex.args[3]))
                else
                    # constant * variable * variable for quadratic
                    @assert length(subex.args) == 4 "Multiplication with more than three terms not supported"
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    @assert isa(subex.args[4], Symbol)
                    if subex.args[3] == subex.args[4]
                        coefficient = 2*subex.args[2]
                    else
                        coefficient = subex.args[2]
                    end
                    push!(quadratic_terms, ParsedScalarQuadraticTerm(coefficient, subex.args[3], subex.args[4]))
                end
            elseif isa(subex, Symbol)
                push!(affine_terms, ParsedScalarAffineTerm(1.0, subex))
            else
                @assert isa(subex, Number)
                constant += subex
            end
        end
        if length(quadratic_terms) == 0
            return ParsedScalarAffineFunction(affine_terms, constant)
        else
            return ParsedScalarQuadraticFunction(affine_terms, quadratic_terms, constant)
        end
    end
end

# see tests for examples
if VERSION > v"0.7-"
    function separatelabel(ex)
        if isexpr(ex, :call) && ex.args[1] == :(:)
            return ex.args[2], ex.args[3]
        elseif isexpr(ex, :tuple)
            ex = copy(ex)
            @assert isexpr(ex.args[1], :call) && ex.args[1].args[1] == :(:)
            label = ex.args[1].args[2]
            ex.args[1] = ex.args[1].args[3]
            return label, ex
        elseif isexpr(ex, :call)
            ex = copy(ex)
            @assert isexpr(ex.args[2], :call) && ex.args[2].args[1] == :(:)
            label = ex.args[2].args[2]
            ex.args[2] = ex.args[2].args[3]
            return label, ex
        else
            error("Unrecognized expression $ex")
        end
    end
else
    function separatelabel(ex)
        if isexpr(ex, :(:))
            return ex.args[1], ex.args[2]
        elseif isexpr(ex, :tuple)
            ex = copy(ex)
            @assert isexpr(ex.args[1], :(:))
            label = ex.args[1].args[1]
            ex.args[1] = ex.args[1].args[2]
            return label, ex
        elseif isexpr(ex, :call)
            ex = copy(ex)
            @assert isexpr(ex.args[2], :(:))
            label = ex.args[2].args[1]
            ex.args[2] = ex.args[2].args[2]
            return label, ex
        else
            error("Unrecognized expression $ex")
        end
    end
end

function parsedtoMOI(model, s::Symbol)
    return MOI.get(model, MOI.VariableIndex, String(s))
end

# Used for Vector{Symbol}, Vector{ParsedScalarAffineTerm}, Vector{ParsedVectorAffineTerm},
# Vector{ParsedScalarQuadraticTerm} and Vector{ParsedVectorQuadraticTerm}
parsedtoMOI(model, s::Vector) = parsedtoMOI.(model, s)

parsedtoMOI(model, s::Union{Float64, Int64}) = s


for typename in [:ParsedScalarAffineTerm,:ParsedScalarAffineFunction,:ParsedVectorAffineTerm,:ParsedVectorAffineFunction,
                 :ParsedScalarQuadraticTerm,:ParsedScalarQuadraticFunction,:ParsedVectorQuadraticTerm,:ParsedVectorQuadraticFunction,
                 :ParsedSingleVariable,:ParsedVectorOfVariables]
    moiname = Compat.Meta.parse(replace(string(typename), "Parsed" => "MOI."))
    fields = fieldnames(eval(typename))
    constructor = Expr(:call, moiname, [Expr(:call,:parsedtoMOI,:model,Expr(:.,:f,Base.Meta.quot(field))) for field in fields]...)
    @eval parsedtoMOI(model, f::$typename) = $constructor
end

function loadfromstring!(model, s)
    parsedlines = filter(ex -> ex != nothing, Compat.Meta.parse.(split(s,"\n")))

    for line in parsedlines
        label, ex = separatelabel(line)
        if label == :variables
            if isexpr(ex, :tuple)
                for v in ex.args
                    vindex = MOI.addvariable!(model)
                    MOI.set!(model, MOI.VariableName(), vindex, String(v))
                end
            else
                @assert isa(ex, Symbol)
                vindex = MOI.addvariable!(model)
                MOI.set!(model, MOI.VariableName(), vindex, String(ex))
            end
        elseif label == :maxobjective
            f = parsedtoMOI(model, parsefunction(ex))
            MOI.set!(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
        elseif label == :minobjective
            f = parsedtoMOI(model, parsefunction(ex))
            MOI.set!(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
        else
            # constraint
            @assert isexpr(ex, :call)
            f = parsedtoMOI(model, parsefunction(ex.args[2]))
            if ex.args[1] == :in
                # Could be safer here
                set = Compat.Core.eval(MOI, ex.args[3])
            elseif ex.args[1] == :<=
                set = MOI.LessThan(ex.args[3])
            elseif ex.args[1] == :>=
                set = MOI.GreaterThan(ex.args[3])
            elseif ex.args[1] == :(==)
                set = MOI.EqualTo(ex.args[3])
            else
                error("Unrecognized expression $ex")
            end
            cindex = MOI.addconstraint!(model, f, set)
            MOI.set!(model, MOI.ConstraintName(), cindex, String(label))
        end
    end
end
