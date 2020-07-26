using Test

import MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include(joinpath(dirname(@__DIR__), "utilities.jl"))

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "Vectorize" begin
    bridged_mock = MOI.Bridges.Constraint.Vectorize{Float64}(mock)

    MOIT.scalar_function_constant_not_zero(bridged_mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = Iterators.product(
            [
                MOI.SingleVariable,
                MOI.ScalarAffineFunction{Float64},
                # TODO: add when operate(vcat, ...) is implemented for quadratic
                # MOI.ScalarQuadraticFunction{Float64},
            ], [
                MOI.EqualTo{Float64},
                MOI.GreaterThan{Float64},
                MOI.LessThan{Float64},
            ],
        )
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0], [1]],
        )
    )
    MOIT.linear2test(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    MOIT.linear4test(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4/3, 4/3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]),
    )
    MOIT.linear5test(mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    MOIT.linear6test(mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1/2, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1], [-2]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[2], [0], [0]]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]],
        )
    )
    # linear14 has double variable bounds for the z variable
    mock.eval_variable_constraint_dual = false
    MOIT.linear14test(bridged_mock, config)
    mock.eval_variable_constraint_dual = true

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock, ones(3), (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[2]]
    )
    MOIT.psdt0vtest(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}
            }()
        )
    )

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
    end

    test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.Zeros, 0),)
    )

    @testset "_vectorized_convert" begin
        @testset "SingleVariable" begin
            model = MOI.Utilities.Model{Float64}()
            x = MOI.add_variable(model)
            f = MOI.SingleVariable(x)
            f_vov = MOI.Bridges.Constraint._vectorized_convert(MOI.VectorOfVariables, f)
            @test f_vov ≈ MOI.VectorOfVariables([x])
            f_vaf = MOI.Bridges.Constraint._vectorized_convert(
                MOI.VectorAffineFunction{Float64}, f
            )
            @test f_vaf ≈ MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))], [0.0]
            )
            f_vqf = MOI.Bridges.Constraint._vectorized_convert(
                MOI.VectorQuadraticFunction{Float64}, f
            )
            @test f_vqf ≈ MOI.VectorQuadraticFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
                MOI.VectorQuadraticTerm{Float64}[],
                [0.0],
            )
        end
        @testset "ScalarAffineFunction" begin
            model = MOI.Utilities.Model{Float64}()
            x = MOI.add_variable(model)
            f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
            @test_throws(
                MethodError,
                MOI.Bridges.Constraint._vectorized_convert(MOI.VectorOfVariables, f)
            )
            f_vaf = MOI.Bridges.Constraint._vectorized_convert(
                MOI.VectorAffineFunction{Float64}, f
            )
            @test f_vaf ≈ MOI.VectorAffineFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))], [1.0]
            )
            f_vqf = MOI.Bridges.Constraint._vectorized_convert(
                MOI.VectorQuadraticFunction{Float64}, f
            )
            @test f_vqf ≈ MOI.VectorQuadraticFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
                MOI.VectorQuadraticTerm{Float64}[],
                [1.0],
            )
        end
        @testset "ScalarQuadraticFunction" begin
            model = MOI.Utilities.Model{Float64}()
            x = MOI.add_variable(model)
            f = MOI.ScalarQuadraticFunction(
                [MOI.ScalarAffineTerm(2.0, x)],
                [MOI.ScalarQuadraticTerm(3.0, x, x)],
                1.0,
            )
            @test_throws(
                MethodError,
                MOI.Bridges.Constraint._vectorized_convert(
                    MOI.VectorOfVariables, f
                )
            )
            @test_throws(
                MethodError,
                MOI.Bridges.Constraint._vectorized_convert(
                    MOI.VectorAffineFunction{Float64}, f
                )
            )
            f_vqf = MOI.Bridges.Constraint._vectorized_convert(
                MOI.VectorQuadraticFunction{Float64}, f
            )
            @test f_vqf ≈ MOI.VectorQuadraticFunction(
                [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
                MOI.VectorQuadraticTerm{Float64}[
                    MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(3.0, x, x))
                ],
                [1.0],
            )
        end
    end
end
