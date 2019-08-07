using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Variable.RSOCtoPSD{Float64}(mock)

@testset "RSOC4" begin
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0, 2.0, 1.0, 0.0, 2.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})  => [0.25],
        (MOI.SingleVariable, MOI.EqualTo{Float64})  => [-0.5],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
        (MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle) => [[1.0, -0.5, 0.25, -0.5, 0.25, 0.25]])
    mock.eval_variable_constraint_dual = false
    MOIT.rotatedsoc4test(bridged_mock, config)
    mock.eval_variable_constraint_dual = true

    @testset "Delete" begin
        v = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
        @test length(v) == 4

        message = string("Cannot delete variable as it is constrained with other",
                         " variables in a `MOI.VectorOfVariables`.")
        for i in 1:4
            err = MOI.DeleteNotAllowed(v[i], message)
            @test_throws err MOI.delete(bridged_mock, v[i])
        end

        test_delete_bridged_variables(bridged_mock, v, MOI.RotatedSecondOrderCone, 4, (
            (MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle, 0),
            (MOI.SingleVariable, MOI.EqualTo{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
        ))
    end
end
