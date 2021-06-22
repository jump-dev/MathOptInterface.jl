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

MOI.Test.runtests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(basis = true),
    # Oops! Name clash.
    exclude = [
        "test_linear_mixed_complementarity",
        "test_qp_complementarity_constraint",
    ],
)

MOI.Test.runtests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(modify_lhs = false),
    # Oops! Name clash.
    exclude = [
        "test_linear_mixed_complementarity",
        "test_qp_complementarity_constraint",
    ],
)
