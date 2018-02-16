using MathOptInterface, Base.Test
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities

# There is no standalone code in this package to test.
# Tests for solvers are located in MOI.Test.

include("isapprox.jl")
include("interval.jl")

# Needed by test spread over several files, defining it here make it easier to comment out tests
# Model supporting every MOI functions and sets
MOIU.@model(Model,
               (), # <- example of giving no set
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle),
               (PowerCone, DualPowerCone),
               (SingleVariable,), # <- example of giving only one set
               (ScalarAffineFunction, ScalarQuadraticFunction),
               (VectorOfVariables,),
               (VectorAffineFunction, VectorQuadraticFunction))
# Model supporting only SecondOrderCone as non-LP cone.
@MOIU.model ModelForMock (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

include("functions.jl")
include("sets.jl")
include("model.jl")
include("parser.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("copy.jl")
