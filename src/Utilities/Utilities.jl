module Utilities

using Compat # For lastindex and Nothing

using MathOptInterface
const MOI = MathOptInterface

const MOIU = MOI.Utilities # used in macro

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const SAT{T} = MOI.ScalarAffineTerm{T}
const VAT{T} = MOI.VectorAffineTerm{T}
const SQT{T} = MOI.ScalarQuadraticTerm{T}
const VQT{T} = MOI.VectorQuadraticTerm{T}

const SConstC{T} = MOI.ScalarConstantChange{T}
const VConstC{T} = MOI.VectorConstantChange{T}
const SCoeffC{T} = MOI.ScalarCoefficientChange{T}
const MulC{T} = MOI.MultirowChange{T}

const VI = MOI.VariableIndex
const CI{F,S} = MOI.ConstraintIndex{F,S}

include("functions.jl")
include("sets.jl")
include("copy.jl")

include("model.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("universalfallback.jl")

end # module
