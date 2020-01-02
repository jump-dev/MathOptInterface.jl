using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "NormSpectral" begin
    bridged_mock = MOIB.Constraint.NormSpectral{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.NormSpectralCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    d1 = 1 / 6
    d2 = 0.25
    x = -inv(2 * sqrt(6))
    psd_dual = [d1, 0, d1, 0, 0, d1, x, x, x, d2, x, x, x, 0, d2]
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [sqrt(6)],
        (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [psd_dual])

    MOIT.normspec1test(bridged_mock, config)

    @testset "Test mock model" begin
        var_names = ["t"]
        MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
        psd = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}())
        @test length(psd) == 1
        MOI.set(mock, MOI.ConstraintName(), psd[1], "psd")

        s = """
        variables: t
        psd: [t, 0.0, t, 0.0, 0.0, t, 1.0, 1.0, 1.0, t, 1.0, 1.0, 1.0, 0.0, t] in MathOptInterface.PositiveSemidefiniteConeTriangle(5)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["psd"])
    end

    @testset "Test bridged model" begin
        var_names = ["t"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
        spec = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormSpectralCone}())
        @test length(spec) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), spec[1], "spec")

        s = """
        variables: t
        spec: [t, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0] in MathOptInterface.NormSpectralCone(2, 3)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["spec"])
    end

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormSpectralCone}()))
    test_delete_bridge(bridged_mock, ci, 1, ((MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0),))
end



# @testset "NormOne" begin
#     bridged_mock = MOIB.Constraint.NormOne{Float64}(mock)
#
#     MOIT.basic_constraint_tests(bridged_mock, config,
#         include = [(F, MOI.NormOneCone) for F in [
#             MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
#         ]])
#
#     mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.5, 0.5, 0.5, 0.5],
#         (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[1.0, 1.0, 0.0, 0.0]],
#         (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [[1.0]],
#         (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [0]])
#
#     MOIT.normone1vtest(bridged_mock, config)
#     MOIT.normone1ftest(bridged_mock, config)
#
#     @testset "Test mock model" begin
#         var_names = ["x", "y", "z", "u", "v"]
#         MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
#         nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
#         @test length(nonneg) == 1
#         MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
#         greater = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}())
#         @test length(greater) == 1
#         MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
#         zeros = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
#         @test length(zeros) == 2
#         MOI.set(mock, MOI.ConstraintName(), zeros[1], "x_eq")
#         MOI.set(mock, MOI.ConstraintName(), zeros[2], "y_eq")
#
#         s = """
#         variables: x, y, z, u, v
#         nonneg: [u + -1.0y, v + -1.0z, u + y, v + z] in MathOptInterface.Nonnegatives(4)
#         greater: x + -1.0u + -1.0v >= 0.0
#         x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
#         y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
#         maxobjective: y + z
#         """
#         model = MOIU.Model{Float64}()
#         MOIU.loadfromstring!(model, s)
#         MOIU.test_models_equal(mock, model, var_names, ["nonneg", "greater", "x_eq", "y_eq"])
#     end
#
#     @testset "Test bridged model" begin
#         var_names = ["x", "y", "z"]
#         MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
#         normone = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}())
#         @test length(normone) == 1
#         MOI.set(bridged_mock, MOI.ConstraintName(), normone[1], "normone")
#         zeros = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
#         @test length(zeros) == 2
#         MOI.set(bridged_mock, MOI.ConstraintName(), zeros[1], "x_eq")
#         MOI.set(bridged_mock, MOI.ConstraintName(), zeros[2], "y_eq")
#
#         s = """
#         variables: x, y, z
#         normone: [1.0x, y, z] in MathOptInterface.NormOneCone(3)
#         x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
#         y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
#         maxobjective: y + z
#         """
#         model = MOIU.Model{Float64}()
#         MOIU.loadfromstring!(model, s)
#         MOIU.test_models_equal(bridged_mock, model, var_names, ["normone", "x_eq", "y_eq"])
#     end
#
#     ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}()))
#     test_delete_bridge(bridged_mock, ci, 3, (
#         (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
#         (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0)))
# end
