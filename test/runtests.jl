using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

using Compat
using Compat.Test

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

# Needed by test spread over several files, defining it here make it easier to comment out tests
# Model supporting every MOI functions and sets
MOIU.@model(Model,
            (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
             MOI.Semicontinuous, MOI.Semiinteger),
            (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
             MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
             MOI.GeometricMeanCone, MOI.ExponentialCone, MOI.DualExponentialCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
             MOI.RootDetConeTriangle, MOI.RootDetConeSquare, MOI.LogDetConeTriangle,
             MOI.LogDetConeSquare),
            (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

# Model supporting only SecondOrderCone as non-LP cone.
MOIU.@model(ModelForMock, (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone),
            (),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction,))

# Utilities submodule tests
@testset "MOI.Utilities" begin
    include("Utilities/functions.jl")
    include("Utilities/sets.jl")
    include("Utilities/constraints.jl")
    include("Utilities/model.jl")
    include("Utilities/universalfallback.jl")
    include("Utilities/parser.jl")
    include("Utilities/mockoptimizer.jl")
    include("Utilities/cachingoptimizer.jl")
    include("Utilities/copy.jl")
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
