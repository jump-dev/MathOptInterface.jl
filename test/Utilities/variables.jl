using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

w = MOI.VariableIndex(0)
fw = MOI.SingleVariable(w)
x = MOI.VariableIndex(1)
fx = MOI.SingleVariable(x)
y = MOI.VariableIndex(2)
fy = MOI.SingleVariable(y)
z = MOI.VariableIndex(3)
fz = MOI.SingleVariable(z)

@testset "get_bounds" begin
    @testset "Float64" begin
        T = Float64
        model = MOIU.Model{T}()

        x = MOI.add_variable(model)
        y_v, y_c = MOI.add_constrained_variable(model, MOI.GreaterThan{T}(1.0))
        z_v, z_c = MOI.add_constrained_variable(model, MOI.LessThan{T}(-1.0))
        w_v, w_c = MOI.add_constrained_variable(model, MOI.Interval{T}(-2.0, 3.0))

        @test -Inf == @inferred MOIU.get_bounds(model, T, x)[1]
        @test Inf == @inferred MOIU.get_bounds(model, T, x)[2]

        @test 1.0 == @inferred MOIU.get_bounds(model, T, y_v)[1]
        @test Inf == @inferred MOIU.get_bounds(model, T, y_v)[2]

        @test -Inf == @inferred MOIU.get_bounds(model, T, z_v)[1]
        @test -1.0 == @inferred MOIU.get_bounds(model, T, z_v)[2]

        @test -2.0 == @inferred MOIU.get_bounds(model, T, w_v)[1]
        @test 3.0 == @inferred MOIU.get_bounds(model, T, w_v)[2]
    end

    @testset "UInt128" begin
        T = UInt128
        model = MOIU.Model{T}()

        x = MOI.add_variable(model)
        y_v, y_c = MOI.add_constrained_variable(model, MOI.GreaterThan{T}(1))
        z_v, z_c = MOI.add_constrained_variable(model, MOI.LessThan{T}(1))
        w_v, w_c = MOI.add_constrained_variable(model, MOI.Interval{T}(2, 3))

        @test typemin(T) == @inferred MOIU.get_bounds(model, T, x)[1]
        @test typemax(T) == @inferred MOIU.get_bounds(model, T, x)[2]

        @test 1.0 == @inferred MOIU.get_bounds(model, T, y_v)[1]
        @test typemax(T) == @inferred MOIU.get_bounds(model, T, y_v)[2]

        @test typemin(T) == @inferred MOIU.get_bounds(model, T, z_v)[1]
        @test 1.0 == @inferred MOIU.get_bounds(model, T, z_v)[2]

        @test 2.0 == @inferred MOIU.get_bounds(model, T, w_v)[1]
        @test 3.0 == @inferred MOIU.get_bounds(model, T, w_v)[2]
    end
end
