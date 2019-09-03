module Objective

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

# Definition of an objective bridge
include("bridge.jl")

# Mapping between objective function attributes and bridges
include("map.jl")

end
