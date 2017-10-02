#=
    This file contains the logic to convert MathOptInterface functions
    to MathOptFormat objects.
=#

Object!(m::MOFFile, func::MOI.SingleVariable) = variable(getvariable!(m, func.variable))

Object!(m::MOFFile, func::MOI.VectorOfVariables) = variableset(getvariable!.(m, func.variables))

Object!(m::MOFFile, func::MOI.ScalarAffineFunction) = linear(
    getvariable!.(m, func.variables),
    func.coefficients,
    func.constant
)

#=
    This file contains the logic to convert MathOptInterface sets
    to MathOptFormat objects.
=#

Object(set::MOI.EqualTo) = equalto(set.value)

Object(set::MOI.LessThan) = lessthan(set.upper)

Object(set::MOI.GreaterThan) = greaterthan(set.lower)
