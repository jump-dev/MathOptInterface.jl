module TestDeprecate

using Test
using MathOptInterface
const MOI = MathOptInterface

function test_deprecations_N()
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

function test_deprecations_ScalarAffineTerm()
    x = MOI.VariableIndex(1)
    t = MOI.ScalarAffineTerm(1.0, x)
    @test_logs (:warn,) t.variable_index == x
end

function test_deprecations_ScalarQuadraticTerm()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    t = MOI.ScalarQuadraticTerm(1.0, x, y)
    @test_logs (:warn,) t.variable_index_1 == x
    @test_logs (:warn,) t.variable_index_2 == y
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end

TestDeprecate.runtests()
