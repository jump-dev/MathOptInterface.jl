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

Object!(m::MOFFile, func::MOI.VectorQuadraticFunction) = vectorquadratic(
    func.affine_outputindex,
    getvariable!.(m, func.affine_variables),
    func.affine_coefficients,
    func.quadratic_outputindex,
    getvariable!.(m, func.quadratic_rowvariables),
    getvariable!.(m, func.quadratic_colvariables),
    func.quadratic_coefficients,
    func.constant
)

#=
    This file contains the logic to convert MathOptInterface sets
    to MathOptFormat objects.
=#

Object(set::MOI.EqualTo)     = equalto(set.value)
Object(set::MOI.LessThan)    = lessthan(set.upper)
Object(set::MOI.GreaterThan) = greaterthan(set.lower)
Object(set::MOI.Interval)    = interval(set.lower, set.upper)

Object(::MOI.Integer) = integer()
Object(::MOI.ZeroOne) = zeroone()

Object(set::MOI.Reals)        = reals(set.dim)
Object(set::MOI.Zeros)        = zeros(set.dim)
Object(set::MOI.Nonnegatives) = nonnegatives(set.dim)
Object(set::MOI.Nonpositives) = nonpositives(set.dim)

Object(set::MOI.Semicontinuous) = semicontinuous(set.l, set.u)
Object(set::MOI.Semiinteger)    = semiinteger(set.l, set.u)

Object(set::MOI.SOS1) = sos1(set.weights)
Object(set::MOI.SOS2) = sos2(set.weights)

Object(set::MOI.SecondOrderCone) = secondordercone(set.dim)
Object(set::MOI.RotatedSecondOrderCone) = rotatedsecondordercone(set.dim)
Object(set::MOI.ExponentialCone) = exponentialcone()
Object(set::MOI.DualExponentialCone) = dualexponentialcone()
Object(set::MOI.PowerCone) = powercone(set.a)
Object(set::MOI.DualPowerCone) = dualpowercone(set.a)
Object(set::MOI.PositiveSemidefiniteConeTriangle) = positivesemidefiniteconetriangle(set.dim)
Object(set::MOI.PositiveSemidefiniteConeScaled) = positivesemidefiniteconescaled(set.dim)

#=
    Other MathOptInterface methods
=#

function getvariable!(m::MOFFile, v::MOI.VariableReference)
    if !haskey(m.ext, v)
        push!(m.d["variables"], "x$(v.value)")
        m.ext[v] = length(m.d["variables"])
    end
    m.d["variables"][m.ext[v]]
end

function Object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

function MOI.addvariable!(m::MOFFile)
    i = length(m["variables"]) + 1
    v = MOI.VariableReference(i)
    push!(m["variables"], "x$(i)")
    m.ext[v] = i
    v
end
MOI.addvariables!(m::MOFFile, n::Int) = [MOI.addvariable!(m) for i in 1:n]

function MOI.setobjective!(m::MOFFile, sense, func::MOI.AbstractFunction)
    setobjective!(m, Object(sense), Object!(m, func))
end

function MOI.addconstraint!(m::MOFFile, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    addconstraint!(m, Object!(m, func), Object(set))
end
