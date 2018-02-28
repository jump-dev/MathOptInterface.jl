using Base.Meta: isexpr


"""
A parser for a simple human-readable of an MOI model.
This should be thought of as a compact way to write small models
for tests, and not an exchange format.

variables: x, y, z
minobjective: 2x + 3y
con1: x + y <= 1
con2: [x,y] in Set

special labels: variables, minobjective, maxobjective
everything else denotes a constraint with a name
all constraints must be named
"x - y" does NOT currently parse, needs to be written as "x + -1.0*y"
"x^2" does NOT currently parse, needs to be written as "x*x"
"""

struct ParsedScalarAffineFunction
    variables::Vector{Symbol}
    coefficients::Vector{Float64}
    constant::Float64
end

struct ParsedVectorAffineFunction
    outputindex::Vector{Int}
    variables::Vector{Symbol}
    coefficients::Vector{Float64}
    constant::Vector{Float64}
end

struct ParsedScalarQuadraticFunction
    affine_variables::Vector{Symbol}
    affine_coefficients::Vector{Float64}
    quadratic_rowvariables::Vector{Symbol}
    quadratic_colvariables::Vector{Symbol}
    quadratic_coefficients::Vector{Float64}
    constant::Float64
end

struct ParsedVectorQuadraticFunction
    affine_outputindex::Vector{Int}
    affine_variables::Vector{Symbol}
    affine_coefficients::Vector{Float64}
    quadratic_outputindex::Vector{Int}
    quadratic_rowvariables::Vector{Symbol}
    quadratic_colvariables::Vector{Symbol}
    quadratic_coefficients::Vector{Float64}
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
            affine_outputindex = Int[]
            affine_variables = Symbol[]
            affine_coefficients = Float64[]
            quadratic_outputindex = Int[]
            quadratic_rowvariables = Symbol[]
            quadratic_colvariables = Symbol[]
            quadratic_coefficients = Float64[]
            constant = Float64[]
            for (outindex,f) in enumerate(singlefunctions)
                if isa(f, ParsedSingleVariable)
                    push!(affine_outputindex, outindex)
                    push!(affine_variables, f.variable)
                    push!(affine_coefficients, 1.0)
                    push!(constant, 0.0)
                elseif isa(f, ParsedScalarAffineFunction)
                    append!(affine_outputindex, fill(outindex,length(f.variables)))
                    append!(affine_variables, f.variables)
                    append!(affine_coefficients, f.coefficients)
                    push!(constant, f.constant)
                else
                    @assert isa(f, ParsedScalarQuadraticFunction)
                    append!(affine_outputindex, fill(outindex,length(f.affine_variables)))
                    append!(affine_variables, f.affine_variables)
                    append!(affine_coefficients, f.affine_coefficients)
                    append!(quadratic_outputindex, fill(outindex,length(f.quadratic_rowvariables)))
                    append!(quadratic_rowvariables, f.quadratic_rowvariables)
                    append!(quadratic_colvariables, f.quadratic_colvariables)
                    append!(quadratic_coefficients, f.quadratic_coefficients)
                    push!(constant, f.constant)
                end
            end
            if length(quadratic_outputindex) == 0
                return ParsedVectorAffineFunction(affine_outputindex, affine_variables, affine_coefficients, constant)
            else
                return ParsedVectorQuadraticFunction(affine_outputindex, affine_variables, affine_coefficients, quadratic_outputindex, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)
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
        affine_variables = Symbol[]
        affine_coefficients = Float64[]
        quadratic_rowvariables = Symbol[]
        quadratic_colvariables = Symbol[]
        quadratic_coefficients = Float64[]
        constant = 0.0
        for subex in ex.args[2:end]
            if isexpr(subex, :call) && subex.args[1] == :*
                if length(subex.args) == 3
                    # constant * variable
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    push!(affine_coefficients, subex.args[2])
                    push!(affine_variables, subex.args[3])
                else
                    # constant * variable * variable for quadratic
                    @assert length(subex.args) == 4 "Multiplication with more than three terms not supported"
                    @assert isa(subex.args[2], Number)
                    @assert isa(subex.args[3], Symbol)
                    @assert isa(subex.args[4], Symbol)
                    if subex.args[3] == subex.args[4]
                        push!(quadratic_coefficients, 2*subex.args[2])
                    else
                        push!(quadratic_coefficients, subex.args[2])
                    end
                    push!(quadratic_rowvariables, subex.args[3])
                    push!(quadratic_colvariables, subex.args[4])
                end
            elseif isa(subex, Symbol)
                push!(affine_coefficients, 1.0)
                push!(affine_variables, subex)
            else
                @assert isa(subex, Number)
                constant += subex
            end
        end
        if length(quadratic_rowvariables) == 0
            return ParsedScalarAffineFunction(affine_variables, affine_coefficients, constant)
        else
            return ParsedScalarQuadraticFunction(affine_variables, affine_coefficients, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)
        end
    end
end

# see tests for examples
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

function variabletoindex(model, s::Symbol)
    return MOI.get(model, MOI.VariableIndex, String(s))
end

function variabletoindex(model, s::Vector{Symbol})
    return MOI.get.(model, MOI.VariableIndex, String.(s))
end

variabletoindex(model, s) = s


for typename in [:ParsedScalarAffineFunction,:ParsedVectorAffineFunction,
                 :ParsedScalarQuadraticFunction,:ParsedVectorQuadraticFunction,
                 :ParsedSingleVariable,:ParsedVectorOfVariables]
    moiname = parse(replace(string(typename), "Parsed", "MOI."))
    fields = fieldnames(eval(typename))
    constructor = Expr(:call, moiname, [Expr(:call,:variabletoindex,:model,Expr(:.,:f,Base.Meta.quot(field))) for field in fields]...)
    @eval parsedtoMOI(model, f::$typename) = $constructor
end

function loadfromstring!(model, s)
    parsedlines = filter(ex -> ex != nothing,parse.(split(s,"\n")))

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
                set = eval(MOI, ex.args[3])
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
