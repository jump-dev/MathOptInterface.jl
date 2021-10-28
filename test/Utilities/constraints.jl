module TestConstraints

using Test
import MathOptInterface
const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_normalize_and_add_constrant_VariableIndex()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    ci = MOI.Utilities.normalize_and_add_constraint(
        model,
        x,
        MOI.EqualTo(1.0),
        allow_modify_function = false,
    )
    @test MOI.get(model, MOI.ConstraintFunction(), ci) == x
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.EqualTo(1.0)
    return
end

function test_normalize_and_add_constrant_ScalarAffineFunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 2.0)
    int = MOI.Utilities.normalize_and_add_constraint(model, f, MOI.Integer())
    @test f.constant == 2.0
    @test MOI.get(model, MOI.ConstraintFunction(), int) ≈ f
    @test MOI.get(model, MOI.ConstraintSet(), int) == MOI.Integer()
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    ci = MOI.Utilities.normalize_and_add_constraint(model, f, MOI.EqualTo(3.0))
    @test f.constant == 2.0
    @test MOI.get(model, MOI.ConstraintFunction(), ci) ≈ g
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.EqualTo(1.0)
    ci = MOI.Utilities.normalize_and_add_constraint(
        model,
        f,
        MOI.Interval(-1.0, 1.0),
        allow_modify_function = true,
    )
    @test f.constant == 0.0
    @test MOI.get(model, MOI.ConstraintFunction(), ci) ≈ g
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.Interval(-3.0, -1.0)
    return
end

end  # module

TestConstraints.runtests()
