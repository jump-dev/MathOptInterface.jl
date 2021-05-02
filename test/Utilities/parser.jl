using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

function structeq(a::T, b::T) where {T}
    return all(f -> getfield(a, f) == getfield(b, f), fieldnames(T))
end

@testset "parsefunction" begin
    @test structeq(MOIU.parsefunction(:x), MOIU.ParsedSingleVariable(:x))
    @test structeq(
        MOIU.parsefunction(:([x, y, z])),
        MOIU.ParsedVectorOfVariables([:x, :y, :z]),
    )

    @test structeq(
        MOIU.parsefunction(:(x + y + 2.0)),
        MOIU.ParsedScalarAffineFunction(
            MOIU.ParsedScalarAffineTerm.([1.0, 1.0], [:x, :y]),
            2.0,
        ),
    )
    @test structeq(
        MOIU.parsefunction(:(x + -3y + 2.0)),
        MOIU.ParsedScalarAffineFunction(
            MOIU.ParsedScalarAffineTerm.([1.0, -3.0], [:x, :y]),
            2.0,
        ),
    )
    @test structeq(
        MOIU.parsefunction(:(2 * x * y + y + 1.0)),
        MOIU.ParsedScalarQuadraticFunction(
            MOIU.ParsedScalarAffineTerm.([1.0], [:y]),
            MOIU.ParsedScalarQuadraticTerm.([2.0], [:x], [:y]),
            1.0,
        ),
    )

    err = ErrorException("Expected `+`, got `-`.")
    @test_throws err MOIU.parsefunction(:(x - y))

    @test structeq(
        MOIU.parsefunction(:([x, 2x + y + 5.0])),
        MOIU.ParsedVectorAffineFunction(
            MOIU.ParsedVectorAffineTerm.(
                [1, 2, 2],
                MOIU.ParsedScalarAffineTerm.([1.0, 2.0, 1.0], [:x, :x, :y]),
            ),
            [0.0, 5.0],
        ),
    )
    @test structeq(
        MOIU.parsefunction(:([x, 2x + y + 5.0, 1 * x * x])),
        MOIU.ParsedVectorQuadraticFunction(
            MOIU.ParsedVectorAffineTerm.(
                [1, 2, 2],
                MOIU.ParsedScalarAffineTerm.([1.0, 2.0, 1.0], [:x, :x, :y]),
            ),
            MOIU.ParsedVectorQuadraticTerm.(
                [3],
                MOIU.ParsedScalarQuadraticTerm.([2.0], [:x], [:x]),
            ),
            [0.0, 5.0, 0.0],
        ),
    )
end

@testset "separatelabel" begin
    @test MOIU.separatelabel(:(variables:x)) == (:variables, :x)
    @test MOIU.separatelabel(:(variables:x, y)) == (:variables, :((x, y)))
    @test MOIU.separatelabel(:(minobjective:x+y)) == (:minobjective, :(x + y))
    @test MOIU.separatelabel(:(con1:2x <= 1)) == (:con1, :(2x <= 1))
    @test MOIU.separatelabel(:(con1:[x, y] in S)) == (:con1, :([x, y] in S))
end

@testset "loadfromstring" begin
    @testset "one variable" begin
        s = """
        variables: x
        x >= 1.0
        """
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        bound = MOI.add_constraint(
            model,
            MOI.SingleVariable(x),
            MOI.GreaterThan(1.0),
        )

        model2 = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x"], String[])
    end

    @testset "linear constraints" begin
        s = """
        variables: x, y
        linear1: x + y >= 1.0
        linear2: x + y <= 1.0
        linear3: x + y == 1.0
        """
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        y = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.VariableName(), y, "y")
        linear1 = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
                0.0,
            ),
            MOI.GreaterThan(1.0),
        )
        linear2 = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
                0.0,
            ),
            MOI.LessThan(1.0),
        )
        linear3 = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
                0.0,
            ),
            MOI.EqualTo(1.0),
        )
        MOI.set(model, MOI.ConstraintName(), linear1, "linear1")
        MOI.set(model, MOI.ConstraintName(), linear2, "linear2")
        MOI.set(model, MOI.ConstraintName(), linear3, "linear3")

        model2 = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(
            model,
            model2,
            ["x", "y"],
            ["linear1", "linear2", "linear3"],
        )
    end

    @testset "minimization: linear objective" begin
        s = """
        variables: x, y
        minobjective: x + -2y + 1.0
        """
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        y = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.VariableName(), y, "y")
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, -2.0], [x, y]),
                1.0,
            ),
        )
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

        model2 = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y"], String[])
    end

    @testset "maximization: linear objective" begin
        s = """
        variables: x, y
        maxobjective: x + -2y + 1.0
        """
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        y = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.VariableName(), y, "y")
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, -2.0], [x, y]),
                1.0,
            ),
        )
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

        model2 = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(model, model2, ["x", "y"], String[])
    end

    @testset "SOC constraints" begin
        s = """
        variables: x, y, z
        varsoc: [x,y,z] in SecondOrderCone(3)
        affsoc: [2x,y+1,-1*z] in SecondOrderCone(3)
        affsoc2: [1.0,2.0,3.0] in SecondOrderCone(3)
        """
        model = MOIU.Model{Float64}()
        x = MOI.add_variable(model)
        y = MOI.add_variable(model)
        z = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.VariableName(), y, "y")
        MOI.set(model, MOI.VariableName(), z, "z")
        varsoc = MOI.add_constraint(
            model,
            MOI.VectorOfVariables([x, y, z]),
            MOI.SecondOrderCone(3),
        )
        affsoc = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                MOI.VectorAffineTerm.(
                    [1, 2, 3],
                    MOI.ScalarAffineTerm.([2.0, 1.0, -1.0], [x, y, z]),
                ),
                [0.0, 1.0, 0.0],
            ),
            MOI.SecondOrderCone(3),
        )
        affsoc2 = MOI.add_constraint(
            model,
            MOI.VectorAffineFunction(
                MOI.VectorAffineTerm{Float64}[],
                [1.0, 2.0, 3.0],
            ),
            MOI.SecondOrderCone(3),
        )
        MOI.set(model, MOI.ConstraintName(), varsoc, "varsoc")
        MOI.set(model, MOI.ConstraintName(), affsoc, "affsoc")
        MOI.set(model, MOI.ConstraintName(), affsoc2, "affsoc2")

        model2 = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model2, s)
        MOIU.test_models_equal(
            model,
            model2,
            ["x", "y", "z"],
            ["varsoc", "affsoc", "affsoc2"],
        )
    end

    @testset "Invalid variable name" begin
        s = """
        variables: x
        y >= 1.0
        """
        model = MOIU.Model{Float64}()
        err = ErrorException("Invalid variable name y.")
        @test_throws err MOIU.loadfromstring!(model, s)
    end
end
