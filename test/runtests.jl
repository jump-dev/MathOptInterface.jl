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
@testset "MOI.Utilities" begin
    include("Utilities/Utilities.jl")
end

# Test submodule tests
# It tests that the ConstraintPrimal value requested in the tests is consistent with the VariablePrimal
@testset "MOI.Test" begin
    include("Test/config.jl")
    include("Test/unit.jl")
    include("Test/contlinear.jl")
    include("Test/contconic.jl")
    include("Test/contquadratic.jl")
    include("Test/intlinear.jl")
    include("Test/intconic.jl")
end

@testset "MOI.Bridges" begin
    # Bridges submodule tests
    include("bridge.jl")
end

# Test hygiene of @model macro
include("hygiene.jl")
