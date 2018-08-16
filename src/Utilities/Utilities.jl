module Utilities

using Compat # For firstindex, lastindex and Nothing
using Compat.LinearAlgebra # For dot

using MathOptInterface
const MOI = MathOptInterface

const MOIU = MOI.Utilities # used in macro

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VI = MOI.VariableIndex
const CI{F,S} = MOI.ConstraintIndex{F,S}

include("functions.jl")
include("sets.jl")
include("constraints.jl")
include("copy.jl")
include("results.jl")

include("model.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("universalfallback.jl")

end # module
