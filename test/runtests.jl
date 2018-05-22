using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

using Compat
using Compat.Test

# Tests for solvers are located in MOI.Test.

# MOI tests not relying on any submodule
@testset "MOI" begin
    include("isbits.jl")
    include("isapprox.jl")
    include("interval.jl")
end

# Needed by test spread over several files, defining it here make it easier to comment out tests
# Model supporting every MOI functions and sets
MOIU.@model(Model,
               (ZeroOne, Integer),
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, PositiveSemidefiniteConeSquare, RootDetConeTriangle, RootDetConeSquare, LogDetConeTriangle, LogDetConeSquare),
               (PowerCone, DualPowerCone, SOS1, SOS2),
               (SingleVariable,),
               (ScalarAffineFunction, ScalarQuadraticFunction),
               (VectorOfVariables,),
               (VectorAffineFunction, VectorQuadraticFunction))
# Model supporting only SecondOrderCone as non-LP cone.
@MOIU.model ModelForMock (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

# Utilities submodule tests
@testset "MOI.Utilities" begin
    include("functions.jl")
    include("sets.jl")
    include("model.jl")
    include("universalfallback.jl")
    include("parser.jl")
    include("mockoptimizer.jl")
    include("cachingoptimizer.jl")
    include("copy.jl")
end

# Test submodule tests
# It tests that the ConstraintPrimal value requested in the tests is consistent with the VariablePrimal
@testset "MOI.Test" begin
    include("Test/config.jl")
#    include("Test/unit.jl")
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
