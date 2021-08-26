module TestVariables

using MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities

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

function test_get_bounds_Float64()
    T = Float64
    model = MOIU.Model{T}()

    x = MOI.add_variable(model)
    y_v, y_c = MOI.add_constrained_variable(model, MOI.GreaterThan{T}(1.0))
    z_v, z_c = MOI.add_constrained_variable(model, MOI.LessThan{T}(-1.0))
    w_v, w_c = MOI.add_constrained_variable(model, MOI.Interval{T}(-2.0, 3.0))
    u_v, u_c =
        MOI.add_constrained_variable(model, MOI.Semicontinuous{T}(1.0, 3.5))
    v_v, v_c =
        MOI.add_constrained_variable(model, MOI.Semiinteger{T}(-1.0, 2.5))
    e_v, e_c = MOI.add_constrained_variable(model, MOI.EqualTo(1.35))

    @test -Inf == @inferred MOIU.get_bounds(model, T, x)[1]
    @test Inf == @inferred MOIU.get_bounds(model, T, x)[2]

    @test 1.0 == @inferred MOIU.get_bounds(model, T, y_v)[1]
    @test Inf == @inferred MOIU.get_bounds(model, T, y_v)[2]

    @test -Inf == @inferred MOIU.get_bounds(model, T, z_v)[1]
    @test -1.0 == @inferred MOIU.get_bounds(model, T, z_v)[2]

    @test -2.0 == @inferred MOIU.get_bounds(model, T, w_v)[1]
    @test 3.0 == @inferred MOIU.get_bounds(model, T, w_v)[2]

    @test 0.0 == @inferred MOIU.get_bounds(model, T, u_v)[1]
    @test 3.5 == @inferred MOIU.get_bounds(model, T, u_v)[2]

    @test -1.0 == @inferred MOIU.get_bounds(model, T, v_v)[1]
    @test 2.5 == @inferred MOIU.get_bounds(model, T, v_v)[2]

    @test 1.35 == @inferred MOIU.get_bounds(model, T, e_v)[1]
    @test 1.35 == @inferred MOIU.get_bounds(model, T, e_v)[2]

    MOI.add_constraint(model, y_v, MOI.LessThan{T}(3.6))

    @test 1.0 == @inferred MOIU.get_bounds(model, T, y_v)[1]
    @test 3.6 == @inferred MOIU.get_bounds(model, T, y_v)[2]
    return
end

function test_get_bounds_UInt128()
    T = UInt128
    model = MOIU.Model{T}()

    x = MOI.add_variable(model)
    y_v, y_c = MOI.add_constrained_variable(model, MOI.GreaterThan{T}(1))
    z_v, z_c = MOI.add_constrained_variable(model, MOI.LessThan{T}(1))
    w_v, w_c = MOI.add_constrained_variable(model, MOI.Interval{T}(2, 3))
    u_v, u_c = MOI.add_constrained_variable(model, MOI.Semicontinuous{T}(1, 3))
    v_v, v_c = MOI.add_constrained_variable(model, MOI.Semiinteger{T}(4, 7))

    @test typemin(T) == @inferred MOIU.get_bounds(model, T, x)[1]
    @test typemax(T) == @inferred MOIU.get_bounds(model, T, x)[2]

    @test 1 == @inferred MOIU.get_bounds(model, T, y_v)[1]
    @test typemax(T) == @inferred MOIU.get_bounds(model, T, y_v)[2]

    @test typemin(T) == @inferred MOIU.get_bounds(model, T, z_v)[1]
    @test 1 == @inferred MOIU.get_bounds(model, T, z_v)[2]

    @test 2 == @inferred MOIU.get_bounds(model, T, w_v)[1]
    @test 3 == @inferred MOIU.get_bounds(model, T, w_v)[2]

    @test 0 == @inferred MOIU.get_bounds(model, T, u_v)[1]
    @test 3 == @inferred MOIU.get_bounds(model, T, u_v)[2]

    @test 0 == @inferred MOIU.get_bounds(model, T, v_v)[1]
    @test 7 == @inferred MOIU.get_bounds(model, T, v_v)[2]

    MOI.add_constraint(model, y_v, MOI.LessThan{T}(5))

    @test 1 == @inferred MOIU.get_bounds(model, T, y_v)[1]
    @test 5 == @inferred MOIU.get_bounds(model, T, y_v)[2]
end

end  # module

TestVariables.runtests()
