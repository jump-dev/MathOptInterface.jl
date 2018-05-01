#=
    These tests aim to minimally test each expected feature in MOI, in addition
    to the full end-to-end tests in contlinear.jl etc
=#

const unittests = Dict{String, Function}()

include("variables.jl")
include("objectives.jl")
include("constraints.jl")

@moitestset unit
