#=
    This file contains the logic to convert MathOptInterface functions
    to MathOptFormat objects.
=#

Object!(m::MOFFile, func::MOI.SingleVariable) = variable(getvariable!(m, func.variable))

Object!(m::MOFFile, func::MOI.VectorOfVariables) = variableset(getvariable!.(m, func.variables))

Object!(m::MOFFile, func::MOI.ScalarAffineFunction) = affine(
    getvariable!.(m, func.variables),
    func.coefficients,
    func.constant
)

Object!(m::MOFFile, func::MOI.VectorAffineFunction) = vectoraffine(
    func.outputindex,
    getvariable!.(m, func.variables),
    func.coefficients,
    func.constant
)

Object!(m::MOFFile, func::MOI.ScalarQuadraticFunction) = quadratic(
    getvariable!.(m, func.affine_variables),
    func.affine_coefficients,
    getvariable!.(m, func.quadratic_rowvariables),
    getvariable!.(m, func.quadratic_colvariables),
    func.quadratic_coefficients,
    func.constant
)


#=
    This file contains the logic to convert MathOptInterface sets
    to MathOptFormat objects.
=#

Object(set::MOI.EqualTo) = equalto(set.value)

Object(set::MOI.LessThan) = lessthan(set.upper)

Object(set::MOI.GreaterThan) = greaterthan(set.lower)

Object(::MOI.Integer) = integer()

Object(::MOI.ZeroOne) = zeroone()

#=
    Other MathOptInterface methods
=#

function getvariable!(m::MOFFile, v::MOI.VariableReference)
    if !haskey(m.variables, v)
        m.variables[v] = "x$(v.value)"
        push!(m.d["variables"], m.variables[v])
    end
    m.variables[v]
end

function Object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

function MOI.setobjective!(m::MOFFile, sense, func)
    setobjective!(m, Object(sense), Object!(m, func))
end

function MOI.addconstraint!(m::MOFFile, func, set)
    addconstraint!(m, Object!(m, func), Object(set))
end
