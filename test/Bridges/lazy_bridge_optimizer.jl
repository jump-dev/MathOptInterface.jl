using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("utilities.jl")

_functionize_error(b, bridge_type, func, name) = ErrorException(
    "Need to apply a `$bridge_type` to a `$func` $name because the" *
    " variable is bridged but $name bridges are not supported by" *
    " `$(typeof(b))`."
)

_lazy_functionize_error(bridge_type, func, name) = ErrorException(
    "Need to apply a `$bridge_type` to a `$func` $name because the variable is" *
    " bridged but no such $name bridge type was added. Add one" *
    " with `add_bridge`."
)

MOIU.@model(StandardLPModel,
            (), (MOI.EqualTo, MOIT.UnknownScalarSet), (MOI.Nonnegatives,), (),
            (), (MOI.ScalarAffineFunction,), (MOI.VectorOfVariables,), ())
MOI.supports_constraint(::StandardLPModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.GreaterThan{T}}) where {T} = false
MOI.supports_constraint(::StandardLPModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.LessThan{T}}) where {T} = false
MOI.supports_constraint(::StandardLPModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.EqualTo{T}}) where {T} = false
MOI.supports_constraint(::StandardLPModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Reals}) = false
MOI.supports(::StandardLPModel{T}, ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}) where {T} = false
MOI.supports(::StandardLPModel, ::MOI.ObjectiveFunction{MOI.SingleVariable}) = false

@testset "Bridged variable in `SingleVariable` constraint with $S" for T in [Float64, Int], S in [MOIT.UnknownScalarSet{T}]
    set = S(one(T))
    @testset "No constraint bridge" begin
        model = StandardLPModel{T}()
        bridged = MOIB.Variable.Vectorize{T}(model)
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.SingleVariable(x)
        err = _functionize_error(bridged, MOIB.Constraint.ScalarFunctionizeBridge, "SingleVariable", "constraint")
        @test_throws err MOI.add_constraint(bridged, fx, set)
    end
    @testset "LazyBridgeOptimizer" begin
        model = StandardLPModel{T}()
        bridged = MOIB.LazyBridgeOptimizer(model)
        MOIB.add_bridge(bridged, MOIB.Variable.VectorizeBridge{T})
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.SingleVariable(x)
        @testset "without `Constraint.ScalarFunctionizeBridge`" begin
            err = _lazy_functionize_error(MOIB.Constraint.ScalarFunctionizeBridge, "SingleVariable", "constraint")
            @test_throws err MOI.add_constraint(bridged, fx, set)
        end
        @testset "with `Constraint.ScalarFunctionizeBridge`" begin
            MOIB.add_bridge(bridged, MOIB.Constraint.ScalarFunctionizeBridge{T})
            cx = MOI.add_constraint(bridged, fx, set)
            @test MOI.get(bridged, MOI.ConstraintFunction(), cx) == fx
            @test MOI.get(bridged, MOI.ConstraintSet(), cx) == set
            a = MOI.get(model, MOI.ListOfVariableIndices())[1]
            fa = MOI.SingleVariable(a)
            ca = MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, S}())[1]
            @test MOI.get(model, MOI.ConstraintFunction(), ca) ≈ convert(MOI.ScalarAffineFunction{T}, fa)
            @test MOI.get(model, MOI.ConstraintSet(), ca) == MOIU.shift_constant(set, -one(T))
        end
    end
end

@testset "Bridged variable in `VectorOfVariables` constraint with $T" for T in [Float64, Int]
    set = MOI.Zeros(1)
    @testset "No constraint bridge" begin
        model = StandardLPModel{T}()
        bridged = MOIB.Variable.Vectorize{T}(model)
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.VectorOfVariables([x])
        err = _functionize_error(bridged, MOIB.Constraint.VectorFunctionizeBridge, "VectorOfVariables", "constraint")
        @test_throws err MOI.add_constraint(bridged, fx, set)
    end
    @testset "LazyBridgeOptimizer" begin
        model = StandardLPModel{T}()
        bridged = MOIB.LazyBridgeOptimizer(model)
        MOIB.add_bridge(bridged, MOIB.Variable.VectorizeBridge{T})
        MOIB.add_bridge(bridged, MOIB.Constraint.ScalarizeBridge{T})
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.VectorOfVariables([x])
        @testset "without `Constraint.ScalarFunctionizeBridge`" begin
            err = _lazy_functionize_error(MOIB.Constraint.VectorFunctionizeBridge, "VectorOfVariables", "constraint")
            @test_throws err MOI.add_constraint(bridged, fx, set)
        end
        @testset "with `Constraint.ScalarFunctionizeBridge`" begin
            MOIB.add_bridge(bridged, MOIB.Constraint.VectorFunctionizeBridge{T})
            cx = MOI.add_constraint(bridged, fx, set)
            @test MOI.get(bridged, MOI.ConstraintFunction(), cx) == fx
            @test MOI.get(bridged, MOI.ConstraintSet(), cx) == set
            a = MOI.get(model, MOI.ListOfVariableIndices())[1]
            fa = MOI.SingleVariable(a)
            ca = MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}())[1]
            @test MOI.get(model, MOI.ConstraintFunction(), ca) ≈ convert(MOI.ScalarAffineFunction{T}, fa)
            @test MOI.get(model, MOI.ConstraintSet(), ca) == MOI.EqualTo(-one(T))
        end
    end
end

@testset "Bridged variable in `SingleVariable` objective function with $T" for T in [Float64, Int]
    @testset "No objective bridge" begin
        model = StandardLPModel{T}()
        bridged = MOIB.Variable.Vectorize{T}(model)
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.SingleVariable(x)
        err = _functionize_error(bridged, MOIB.Objective.FunctionizeBridge, "SingleVariable", "objective")
        @test_throws err MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    end
    @testset "LazyBridgeOptimizer" begin
        model = StandardLPModel{T}()
        bridged = MOIB.LazyBridgeOptimizer(model)
        MOIB.add_bridge(bridged, MOIB.Variable.VectorizeBridge{T})
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.SingleVariable(x)
        @testset "without `Objective.FunctionizeBridge`" begin
            err = _lazy_functionize_error(MOIB.Objective.FunctionizeBridge, "SingleVariable", "objective")
            @test_throws err MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
        end
        @testset "with `Objective.FunctionizeBridge`" begin
            MOIB.add_bridge(bridged, MOIB.Objective.FunctionizeBridge{T})
            MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
            @test MOI.get(bridged, MOI.ObjectiveFunctionType()) == MOI.SingleVariable
            @test MOI.get(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}()) ≈ fx
            a = MOI.get(model, MOI.ListOfVariableIndices())[1]
            fa = MOI.SingleVariable(a)
            @test MOI.get(model, MOI.ObjectiveFunctionType()) == MOI.ScalarAffineFunction{T}
            @test MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()) ≈ fa + one(T)
        end
    end
end

MOIU.@model(
    LPModel,
    (), (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan), (), (),
    (), (MOI.ScalarAffineFunction,), (), ()
)

@testset "Name test" begin
    model = LPModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    MOIT.nametest(bridged)
end

# Model similar to SDPA format, it gives a good example because it does not
# support a lot hence need a lot of bridges
MOIU.@model(SDPAModel,
            (), (MOI.EqualTo,), (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle), (),
            (), (MOI.ScalarAffineFunction,), (MOI.VectorOfVariables,), ())
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.GreaterThan{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.LessThan{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.EqualTo{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Reals}) = false
MOI.supports(::SDPAModel{T}, ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}) where {T} = false
MOI.supports(::SDPAModel, ::MOI.ObjectiveFunction{MOI.SingleVariable}) = false

@testset "Name test with SDPAModel{Float64}" begin
    model = SDPAModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    MOIT.nametest(bridged)
end

@testset "Show SPDA model" begin
    model = SDPAModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    # no bridges
    @test sprint(show, bridged) === raw"""
    MOIB.LazyBridgeOptimizer{SDPAModel{Float64}}
    with 0 variable bridges
    with 0 constraint bridges
    with 0 objective bridges
    with inner model SDPAModel{Float64}"""

    MOI.add_constrained_variable(bridged, MOI.LessThan(1.0))
    # add variable bridges
    @test sprint(show, bridged) == raw"""
    MOIB.LazyBridgeOptimizer{SDPAModel{Float64}}
    with 2 variable bridges
    with 0 constraint bridges
    with 0 objective bridges
    with inner model SDPAModel{Float64}"""
end

@testset "SDPA format with $T" for T in [Float64, Int]
    model = SDPAModel{T}()
    bridged = MOIB.LazyBridgeOptimizer(model)
    @testset "Variable" begin
        @testset "Nonpositives" begin
            @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Nonpositives)
            @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Nonpositives)
            MOIB.add_bridge(bridged, MOIB.Variable.NonposToNonnegBridge{T})
            @test MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Nonpositives)
            @test MOIB.bridge_type(bridged, MOI.Nonpositives) == MOIB.Variable.NonposToNonnegBridge{T}
        end
        @testset "Zeros" begin
            @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
            @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Zeros)
            MOIB.add_bridge(bridged, MOIB.Variable.ZerosBridge{T})
            @test MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Zeros)
            @test MOIB.bridge_type(bridged, MOI.Zeros) == MOIB.Variable.ZerosBridge{T}
        end
        @testset "Free" begin
            @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Reals)
            @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Reals)
            @test_throws MOI.UnsupportedConstraint{MOI.VectorOfVariables, MOI.Reals} MOI.add_variable(bridged)
            @test_throws MOI.UnsupportedConstraint{MOI.VectorOfVariables, MOI.Reals} MOI.add_variables(bridged, 2)
            MOIB.add_bridge(bridged, MOIB.Variable.FreeBridge{T})
            @test MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Reals)
            @test MOIB.bridge_type(bridged, MOI.Reals) == MOIB.Variable.FreeBridge{T}
        end
        @testset "Vectorize" begin
            @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})
            @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.GreaterThan{T})
            @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})
            @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.LessThan{T})
            @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{T})
            @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.EqualTo{T})
            MOIB.add_bridge(bridged, MOIB.Variable.VectorizeBridge{T})
            @test MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.GreaterThan{T})
            @test MOIB.bridge_type(bridged, MOI.GreaterThan{T}) == MOIB.Variable.VectorizeBridge{T, MOI.Nonnegatives}
            @test MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.LessThan{T})
            @test MOIB.bridge_type(bridged, MOI.LessThan{T}) == MOIB.Variable.VectorizeBridge{T, MOI.Nonpositives}
            @test MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.EqualTo{T})
            @test MOIB.bridge_type(bridged, MOI.EqualTo{T}) == MOIB.Variable.VectorizeBridge{T, MOI.Zeros}
        end
        @testset "RSOCtoPSD" begin
            @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
            @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
            MOIB.add_bridge(bridged, MOIB.Variable.RSOCtoPSDBridge{T})
            @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
            MOIB.add_bridge(bridged, MOIB.Constraint.ScalarFunctionizeBridge{T})
            @test MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
            @test MOIB.bridge_type(bridged, MOI.RotatedSecondOrderCone) == MOIB.Variable.RSOCtoPSDBridge{T}
        end
        @testset "Combining two briges" begin
            x, cx = MOI.add_constrained_variable(bridged, MOI.LessThan(one(T)))
            test_delete_bridged_variable(bridged, x, MOI.LessThan{T}, 1, (
                (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
                (MOI.VectorOfVariables, MOI.Nonpositives, 0)),
                used_bridges = 2)
        end
    end
    @testset "Constraint" begin
        @testset "Slack" begin
            @test !MOI.supports_constraint(bridged, MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone)
            MOIB.add_bridge(bridged, MOIB.Constraint.VectorSlackBridge{T})
            @test !MOI.supports_constraint(bridged, MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone)
            MOIB.add_bridge(bridged, MOIB.Constraint.ScalarizeBridge{T})
            @test MOI.supports_constraint(bridged, MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone)
            @test MOIB.bridge_type(bridged, MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone) ==
                MOIB.Constraint.VectorSlackBridge{T, MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}
            @test MOI.supports_constraint(bridged, MOI.VectorAffineFunction{T}, MOI.Zeros)
            @test MOIB.bridge_type(bridged, MOI.VectorAffineFunction{T}, MOI.Zeros) ==
                MOIB.Constraint.ScalarizeBridge{T, MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}}
        end
        @testset "Vectorize" begin
            @test !MOI.supports_constraint(bridged, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
            MOIB.add_bridge(bridged, MOIB.Constraint.VectorizeBridge{T})
            @test MOI.supports_constraint(bridged, MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T})
            @test MOIB.bridge_type(bridged, MOI.ScalarAffineFunction{T},
                                   MOI.GreaterThan{T}) ==
                MOIB.Constraint.VectorizeBridge{
                    T, MOI.VectorAffineFunction{T}, MOI.Nonnegatives,
                    MOI.ScalarAffineFunction{T}
                }

        end
        @testset "Quadratic" begin
            @test !MOI.supports_constraint(bridged, MOI.ScalarQuadraticFunction{T}, MOI.GreaterThan{T})
            @test !MOI.supports_constraint(bridged, MOI.ScalarQuadraticFunction{T}, MOI.LessThan{T})
            MOIB.add_bridge(bridged, MOIB.Constraint.QuadtoSOCBridge{T})
            @test MOI.supports_constraint(bridged, MOI.ScalarQuadraticFunction{T}, MOI.GreaterThan{T})
            @test MOIB.bridge_type(bridged, MOI.ScalarQuadraticFunction{T},
                                   MOI.GreaterThan{T}) == MOIB.Constraint.QuadtoSOCBridge{T}
            @test MOI.supports_constraint(bridged, MOI.ScalarQuadraticFunction{T}, MOI.LessThan{T})
            @test MOIB.bridge_type(bridged, MOI.ScalarQuadraticFunction{T},
                                   MOI.LessThan{T}) == MOIB.Constraint.QuadtoSOCBridge{T}
        end
    end
    @testset "Objective" begin
        F = MOI.ScalarQuadraticFunction{T}
        @test !MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
        @test !MOI.supports(bridged, MOI.ObjectiveFunction{F}())
        MOIB.add_bridge(bridged, MOIB.Objective.SlackBridge{T})
        @test !MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
        @test !MOI.supports(bridged, MOI.ObjectiveFunction{F}())
        MOIB.add_bridge(bridged, MOIB.Objective.FunctionizeBridge{T})
        @test MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
        @test MOIB.bridge_type(bridged, MOI.SingleVariable) == MOIB.Objective.FunctionizeBridge{T}
        @test MOI.supports(bridged, MOI.ObjectiveFunction{F}())
        @test MOIB.bridge_type(bridged, F) == MOIB.Objective.SlackBridge{T, F, F}
    end
end

@testset "Continuous Linear with SDPAModel{$T}" for T in [Float64, Rational{Int}]
    model = SDPAModel{T}()
    bridged = MOIB.full_bridge_optimizer(model, T)
    # For `ScalarAffineFunction`-in-`GreaterThan`,
    # `Constraint.ScalarSlackBridge` -> `Variable.VectorizeBridge`
    # is equivalent to
    # `Constraint.VectorizeBridge` -> `Constraint.VectorSlackBridge`
    # however, `Variable.VectorizeBridge` do not support modification of the
    # set hence it makes some tests of `contlineartest` fail so we disable it.
    MOIB.remove_bridge(bridged, MOIB.Constraint.ScalarSlackBridge{T})
    exclude = ["partial_start"] # `VariablePrimalStart` not supported.
    MOIT.contlineartest(bridged, MOIT.TestConfig{T}(solve=false), exclude)
end

@testset "Continuous Conic with SDPAModel{Float64}" begin
    model = SDPAModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    exclude = ["exp", "dualexp", "pow", "dualpow", "logdet", "rootdets"]
    MOIT.contconictest(bridged, MOIT.TestConfig(solve=false), exclude)
end

# Model not supporting RotatedSecondOrderCone
MOIU.@model(NoRSOCModel,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.NormInfinityCone, MOI.NormOneCone,
             MOI.ExponentialCone, MOI.PositiveSemidefiniteConeTriangle),
            (MOI.PowerCone,),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

# We only use floating point types as there is √2
@testset "Constrained variables in RSOC with $T" for T in [Float64, BigFloat]
    bridged = MOIB.full_bridge_optimizer(NoRSOCModel{T}(), T)
    @test MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)
    # It should be selected over `MOIB.Variable.RSOCtoPSDBridge` even if they
    # are tied in terms of number of bridges because it is added first in
    # `MOIB.full_bridge_optimizer`.
    @test MOIB.bridge_type(bridged, MOI.RotatedSecondOrderCone) == MOIB.Variable.RSOCtoSOCBridge{T}
    x, cx = MOI.add_constrained_variables(bridged, MOI.RotatedSecondOrderCone(3))
    for i in 1:3
        @test MOIB.bridge(bridged, x[i]) isa MOIB.Variable.RSOCtoSOCBridge{T}
    end
    @test MOIB.bridge(bridged, cx) isa MOIB.Variable.RSOCtoSOCBridge{T}
end

# Model not supporting VectorOfVariables and SingleVariable
MOIU.@model(NoVariableModel,
            (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone),
            (),
            (),
            (MOI.ScalarAffineFunction,),
            (),
            (MOI.VectorAffineFunction,))
function MOI.supports_constraint(::NoVariableModel{T}, ::Type{MOI.SingleVariable},
                                 ::Type{<:MOIU.SUPPORTED_VARIABLE_SCALAR_SETS{T}}) where T
    return false
end

# Only supports GreaterThan and Nonnegatives
MOIU.@model(GreaterNonnegModel,
            (),
            (MOI.GreaterThan,),
            (MOI.Nonnegatives,),
            (),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))
function MOI.supports_constraint(
    ::GreaterNonnegModel{T}, ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.EqualTo{T}, MOI.LessThan{T}, MOI.Interval{T}}}) where T
    return false
end


MOIU.@model(ModelNoVAFinSOC,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.NormInfinityCone, MOI.NormOneCone,
             MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.ExponentialCone),
            (MOI.PowerCone, MOI.DualPowerCone),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

MOI.supports_constraint(::ModelNoVAFinSOC{Float64},
                        ::Type{MOI.VectorAffineFunction{Float64}},
                        ::Type{MOI.SecondOrderCone}) = false

# Model supporting nothing
MOIU.@model NothingModel () () () () () () () ()
function MOI.supports_constraint(
    ::NothingModel{T}, ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.EqualTo{T}, MOI.GreaterThan{T}, MOI.LessThan{T},
                   MOI.Interval{T}, MOI.Integer, MOI.ZeroOne}}) where T
    return false
end

struct BridgeAddingNoConstraint{T} <: MOI.Bridges.Constraint.AbstractBridge end
MOIB.added_constrained_variable_types(::Type{<:BridgeAddingNoConstraint}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{<:BridgeAddingNoConstraint}) = Tuple{DataType, DataType}[]
function MOI.supports_constraint(::Type{<:BridgeAddingNoConstraint},
                                 ::Type{MOI.SingleVariable},
                                 ::Type{MOI.Integer})
    return true
end
function MOIB.Constraint.concrete_bridge_type(::Type{<:BridgeAddingNoConstraint{T}},
                                              ::Type{MOI.SingleVariable},
                                              ::Type{MOI.Integer}) where {T}
    return BridgeAddingNoConstraint{T}
end

const LessThanIndicatorSetOne{T} = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, MOI.LessThan{T}}
MOIU.@model(ModelNoZeroIndicator,
            (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval,
             MOI.Semicontinuous, MOI.Semiinteger),
            (MOI.Reals, MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives,
             MOI.NormInfinityCone, MOI.NormOneCone,
             MOI.SecondOrderCone, MOI.RotatedSecondOrderCone,
             MOI.GeometricMeanCone, MOI.ExponentialCone, MOI.DualExponentialCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.PositiveSemidefiniteConeSquare,
             MOI.RootDetConeTriangle, MOI.RootDetConeSquare, MOI.LogDetConeTriangle,
             MOI.LogDetConeSquare),
            (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2, LessThanIndicatorSetOne),
            (), (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))


@testset "Bridge adding no constraint" begin
    mock = MOIU.MockOptimizer(NothingModel{Int}())
    bridged = MOIB.LazyBridgeOptimizer(mock)
    MOI.Bridges.add_bridge(bridged, BridgeAddingNoConstraint{Float64})
    @test MOI.Bridges.supports_bridging_constraint(bridged,
                                                   MOI.SingleVariable,
                                                   MOI.Integer)
end

@testset "Unsupported constraint with cycles" begin
    # Test that `supports_constraint` works correctly when it is not
    # supported but the bridges forms a cycle
    mock = MOIU.MockOptimizer(NothingModel{Float64}())
    bridged = MOIB.full_bridge_optimizer(mock, Float64)
    @test !MOI.supports_constraint(
        bridged, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test !MOI.supports_constraint(
        bridged, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
end

mock = MOIU.MockOptimizer(NoRSOCModel{Float64}())
bridged_mock = MOIB.LazyBridgeOptimizer(mock)

@testset "UnsupportedConstraint when it cannot be bridged" begin
    x = MOI.add_variables(bridged_mock, 4)
    err = MOI.UnsupportedConstraint{MOI.VectorOfVariables,
                                    MOI.RotatedSecondOrderCone}()
    @test_throws err begin
        MOI.add_constraint(bridged_mock, MOI.VectorOfVariables(x),
                           MOI.RotatedSecondOrderCone(4))
    end
end

MOIB.add_bridge(bridged_mock, MOIB.Constraint.SplitIntervalBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.Constraint.RSOCtoPSDBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.Constraint.SOCtoPSDBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.Constraint.RSOCBridge{Float64})

@testset "Name test" begin
    MOIT.nametest(bridged_mock)
end

@testset "Copy test" begin
    MOIT.failcopytestc(bridged_mock)
    MOIT.failcopytestia(bridged_mock)
    MOIT.failcopytestva(bridged_mock)
    MOIT.failcopytestca(bridged_mock)
    MOIT.copytest(bridged_mock, NoRSOCModel{Float64}())
end

# Test that RSOCtoPSD is used instead of RSOC+SOCtoPSD as it is a shortest path.
@testset "Bridge selection" begin
    MOI.empty!(bridged_mock)
    @test !(MOI.supports_constraint(bridged_mock,
                                    MOI.VectorAffineFunction{Float64},
                                    MOI.LogDetConeTriangle))
    x = MOI.add_variables(bridged_mock, 3)
    err = MOI.UnsupportedConstraint{MOI.VectorAffineFunction{Float64},
                                    MOI.LogDetConeTriangle}()
    @test_throws err begin
        MOIB.bridge_type(bridged_mock, MOI.VectorAffineFunction{Float64},
                         MOI.LogDetConeTriangle)
    end
    c = MOI.add_constraint(bridged_mock, MOI.VectorOfVariables(x),
                           MOI.RotatedSecondOrderCone(3))
    @test MOIB.bridge_type(
        bridged_mock, MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone) == MOIB.Constraint.RSOCtoPSDBridge{
            Float64, MOI.VectorAffineFunction{Float64}, MOI.VectorOfVariables}
    @test MOIB.bridge(bridged_mock, c) isa MOIB.Constraint.RSOCtoPSDBridge
    @test bridged_mock.constraint_dist[(MOI.VectorOfVariables,
                                        MOI.RotatedSecondOrderCone)] == 1
end

@testset "Supports" begin
    full_bridged_mock = MOIB.full_bridge_optimizer(mock, Float64)
    @testset "Mismatch vector/scalar" begin
        for S in [MOI.Nonnegatives, MOI.Nonpositives, MOI.Zeros]
            @test !MOI.supports_constraint(full_bridged_mock, MOI.SingleVariable, S)
        end
        for S in [MOI.GreaterThan{Float64}, MOI.LessThan{Float64}, MOI.EqualTo{Float64}]
            @test !MOI.supports_constraint(full_bridged_mock, MOI.VectorOfVariables, S)
        end
    end
    greater_nonneg_mock = MOIU.MockOptimizer(GreaterNonnegModel{Float64}())
    full_bridged_greater_nonneg = MOIB.full_bridge_optimizer(
        greater_nonneg_mock, Float64)
    for F in [MOI.SingleVariable, MOI.ScalarAffineFunction{Float64},
              MOI.ScalarQuadraticFunction{Float64}]
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.Interval{Float64})
        @test !MOI.supports_constraint(
            greater_nonneg_mock, F, MOI.LessThan{Float64})
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg, F, MOI.LessThan{Float64})
    end
    for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
              MOI.VectorQuadraticFunction{Float64}]
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.PositiveSemidefiniteConeSquare)
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.GeometricMeanCone)
        @test !MOI.supports_constraint(
            greater_nonneg_mock, F, MOI.Nonpositives)
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg, F, MOI.Nonnegatives)
    end
    for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]
        # The bridges in this for loop do not support yet
        # VectorQuadraticFunction. See TODO's for the reason.
        # TODO: Missing vcat for quadratic for supporting quadratic.
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.RotatedSecondOrderCone)
        # TODO: Det bridges need to use MOIU.operate to support quadratic.
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.LogDetConeTriangle)
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.RootDetConeTriangle)
    end
    mock2 = MOIU.MockOptimizer(ModelNoVAFinSOC{Float64}())
    @test !MOI.supports_constraint(mock2, MOI.VectorAffineFunction{Float64},
                                   MOI.SecondOrderCone)
    full_bridged_mock2 = MOIB.full_bridge_optimizer(mock2, Float64)
    @test MOI.supports_constraint(full_bridged_mock2, MOI.VectorAffineFunction{Float64},
                                  MOI.SecondOrderCone)
    mock_indicator = MOIU.MockOptimizer(ModelNoZeroIndicator{Float64}())
    full_bridged_mock_indicator = MOIB.full_bridge_optimizer(mock_indicator, Float64)
    @test !MOI.supports_constraint(mock_indicator, MOI.VectorAffineFunction{Float64},
                                MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, MOI.LessThan{Float64}})
    @test MOI.supports_constraint(full_bridged_mock_indicator, MOI.VectorAffineFunction{Float64},
                                MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO, MOI.LessThan{Float64}})
    @testset "Unslack" begin
        for T in [Float64, Int]
            no_variable_mock = MOIU.MockOptimizer(NoVariableModel{T}())
            full_bridged_no_variable = MOIB.full_bridge_optimizer(
                no_variable_mock, T)
            for S in [MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T},
                      MOI.ZeroOne, MOI.Integer]
                @test MOI.supports_constraint(
                    full_bridged_no_variable, MOI.SingleVariable, S)
            end
            for S in [MOI.Nonpositives, MOI.Nonnegatives,
                      MOI.Zeros, MOI.SecondOrderCone]
                @test MOI.supports_constraint(
                    full_bridged_no_variable, MOI.VectorOfVariables, S)
            end
        end
    end
end
