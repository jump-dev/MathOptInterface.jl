#=
    Functions defined by MathOptFormat. These are largely inspired by
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/functions.jl
=#

"""
    object!(m::MOFInstance, f::MOI.AbstractFunction)

Convert a MOI function into the MathOptFormat representation.
"""
function object! end

getname!(m::MOFInstance, v::MOI.VariableIndex) = MOI.get(m, MOI.VariableName(), v)

object!(m::MOFInstance, f::MOI.SingleVariable) = Object("head"=>"SingleVariable", "variable"=> getname!(m, f.variable))

object!(m::MOFInstance, f::MOI.VectorOfVariables) = Object("head"=>"VectorOfVariables", "variables"=>getname!.(m, f.variables))

object!(m::MOFInstance, f::MOI.ScalarAffineFunction) = Object(
    "head"         => "ScalarAffineFunction",
    "variables"    => getname!.(m, f.variables),
    "coefficients" => f.coefficients,
    "constant"     => f.constant
)

object!(m::MOFInstance, f::MOI.VectorAffineFunction) = Object(
    "head"         => "VectorAffineFunction",
    "outputindex"  => f.outputindex,
    "variables"    => getname!.(m, f.variables),
    "coefficients" => f.coefficients,
    "constant"     => f.constant
)

object!(m::MOFInstance, f::MOI.ScalarQuadraticFunction) = Object(
    "head"                   => "ScalarQuadraticFunction",
    "affine_variables"       => getname!.(m, f.affine_variables),
    "affine_coefficients"    => f.affine_coefficients,
    "quadratic_rowvariables" => getname!.(m, f.quadratic_rowvariables),
    "quadratic_colvariables" => getname!.(m, f.quadratic_colvariables),
    "quadratic_coefficients" => f.quadratic_coefficients,
    "constant"               => f.constant
)

object!(m::MOFInstance, f::MOI.VectorQuadraticFunction) = Object(
    "head"                   => "VectorQuadraticFunction",
    "affine_outputindex"     => f.affine_outputindex,
    "affine_variables"       => getname!.(m, f.affine_variables),
    "affine_coefficients"    => f.affine_coefficients,
    "quadratic_outputindex"  => f.quadratic_outputindex,
    "quadratic_rowvariables" => getname!.(m, f.quadratic_rowvariables),
    "quadratic_colvariables" => getname!.(m, f.quadratic_colvariables),
    "quadratic_coefficients" => f.quadratic_coefficients,
    "constant"               => f.constant
)
