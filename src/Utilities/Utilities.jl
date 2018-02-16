module Utilities

using Compat # For lastindex

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
const CI = MOI.ConstraintIndex

include("functions.jl")
include("sets.jl")
include("copy.jl")

include("model.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")

end # module
