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
        @test_logs (:warn, "Field attr.N is deprecated, use attr.result_index") attr.N
    end
end
