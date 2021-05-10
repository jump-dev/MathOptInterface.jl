using Test
using MathOptInterface
const MOI = MathOptInterface

@testset "deprecations" begin
    attr = MOI.VariablePrimal()
    @test_deprecated begin
        @test MOI._result_index_field(attr) == 1
    end
    for attr_type in (
        MOI.ObjectiveValue,
        MOI.DualObjectiveValue,
        MOI.VariablePrimal,
        MOI.ConstraintPrimal,
        MOI.ConstraintDual,
        MOI.ConstraintBasisStatus,
        MOI.PrimalStatus,
        MOI.DualStatus,
        MOI.NLPBlockDual,
    )
        attr = attr_type(1)
        @test_logs(
            (:warn, "Field attr.N is deprecated, use attr.result_index"),
            attr.N,
        )
    end
end

@testset "ScalarAffineTerm" begin
    x = MOI.VariableIndex(1)
    t = MOI.ScalarAffineTerm(1.0, x)
    @test_logs (:warn,) t.variable_index == x
end

@testset "ScalarQuadraticTerm" begin
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    t = MOI.ScalarQuadraticTerm(1.0, x, y)
    @test_logs (:warn,) t.variable_index_1 == x
    @test_logs (:warn,) t.variable_index_2 == y
end

@testset "RawOptimizerAttribute" begin
    @test_logs (:warn,) MOI.RawParameter(:a) == MOI.RawOptimizerAttribute("a")
end
