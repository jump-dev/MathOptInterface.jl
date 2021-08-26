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

function test_deprecations_ScalarQuadraticFunction()
    @test_logs(
        (:warn,),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarAffineTerm{Int}[],
            MOI.ScalarQuadraticTerm{Int}[],
            0,
        ),
    )
    return
end

function test_deprecations_VectorQuadraticFunction()
    @test_logs(
        (:warn,),
        MOI.VectorQuadraticFunction(
            MOI.VectorAffineTerm{Int}[],
            MOI.VectorQuadraticTerm{Int}[],
            [0, 1],
        ),
    )
    return
end

function test_RawOptimizerAttribute()
    @test_logs (:warn,) MOI.RawParameter(:a) == MOI.RawOptimizerAttribute("a")
end

function test_default_copy_to()
    dest = MOI.Utilities.Model{Float64}()
    src = MOI.Utilities.Model{Float64}()
    @test_logs (:warn,) MOI.Utilities.default_copy_to(dest, src, true)
    return
end

function test_copy_to_copy_names()
    dest = MOI.Utilities.Model{Float64}()
    src = MOI.Utilities.Model{Float64}()
    @test_logs (:warn,) MOI.copy_to(dest, src; copy_names = true)
    return
end

function test_IndexMap()
    @test_logs (:warn,) MOI.IndexMap(1)
    return
end

function test_CleverDicts()
    K, V = MOI.VariableIndex, MOI.VariableIndex
    @test_logs (:warn,) MOI.Utilities.CleverDicts.CleverDict{K,V}(1)
    @test_logs (:warn,) MOI.Utilities.CleverDicts.CleverDict{K,V}(
        MOI.Utilities.CleverDicts.key_to_index,
        MOI.Utilities.CleverDicts.index_to_key,
        1,
    )
    return
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
