using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

using Test

# Tests for solvers are located in MOI.Test.

include("dummy.jl")

# MOI tests not relying on any submodule
@testset "MOI" begin
    include("isbits.jl")
    include("isapprox.jl")
    include("interval.jl")
    include("errors.jl")
    include("functions.jl")
    include("sets.jl")
    include("attributes.jl")
end

# Utilities submodule tests
@testset "MOI.$(submodule)" for submodule in [
    "Benchmarks",
    "Bridges",
    "Formats"
    "Test",
    "Utilities",
]
    include("$(submodule)/$(submodule).jl")
end

# Test hygiene of @model macro
include("hygiene.jl")

