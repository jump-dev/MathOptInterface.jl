module Utilities

using LinearAlgebra # For dot
using OrderedCollections # for OrderedDict in UniversalFallback 

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

function print_with_acronym(io::IO, s::AbstractString)
    s = replace(s, "MathOptInterface.Utilities" => "MOIU")
    s = replace(s, "MathOptInterface.Bridges" => "MOIB")
    s = replace(s, "MathOptInterface.Test" => "MOIT")
    s = replace(s, "MathOptInterface" => "MOI")
    print(io, s)
end

include("functions.jl")
include("mutable_arithmetics.jl")
include("sets.jl")
include("constraints.jl")
include("dense_dict.jl")
include("copy.jl")
include("results.jl")
include("variables.jl")

include("model.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("universalfallback.jl")

include("CleverDicts.jl")
include("lazy_iterators.jl")

end # module
