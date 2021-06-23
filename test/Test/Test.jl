using Test
using MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities

@testset "$(file)" for file in readdir(@__DIR__)
    if file == "Test.jl"
        continue
    end
    include(file)
end

MOI.Test.basic_constraint_tests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(),
)

MOI.Test.runtests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(basis = true),
    exclude = ["test_nonlinear_"],
)

MOI.Test.runtests(
    MOIU.MockOptimizer(
        MOIU.UniversalFallback(MOIU.Model{Float64}()),
        eval_objective_value = false,
    ),
    MOI.Test.Config(optimal_status = MOI.LOCALLY_SOLVED),
    include = ["test_nonlinear_"],
)
