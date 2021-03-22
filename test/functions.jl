using Test
using MathOptInterface
const MOI = MathOptInterface

@testset "Functions" begin
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @testset "Broadcast" begin
        xf = MOI.SingleVariable(x)
        yf = MOI.SingleVariable(y)
        zf = MOI.SingleVariable(z)
        function sum_indices(sv1::MOI.SingleVariable, sv2::MOI.SingleVariable)
            return sv1.variable.value + sv2.variable.value
        end
        @test sum_indices.(xf, [yf, zf]) == [3, 4]
    end
    @testset "Copy" begin
        @testset "VectorOfVariables" begin
            f = MOI.VectorOfVariables([x, y])
            f_copy = copy(f)
            f_copy.variables[2] = z
            @test f.variables[2] == y
        end
    end
end

@testset "Base.convert" begin
    @testset "SingleVariable" begin
        model = MOI.Utilities.Model{Float64}()
        x = MOI.add_variable(model)
        f = MOI.SingleVariable(x)
        f_vov = convert(MOI.VectorOfVariables, f)
        @test f_vov ≈ MOI.VectorOfVariables([x])
        f_vaf = convert(MOI.VectorAffineFunction{Float64}, f)
        @test f_vaf ≈ MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            [0.0],
        )
        f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
        @test f_vqf ≈ MOI.VectorQuadraticFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
            MOI.VectorQuadraticTerm{Float64}[],
            [0.0],
        )
    end
    @testset "ScalarAffineFunction" begin
        model = MOI.Utilities.Model{Float64}()
        x = MOI.add_variable(model)
        f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
        @test_throws(MethodError, convert(MOI.VectorOfVariables, f))
        f_vaf = convert(MOI.VectorAffineFunction{Float64}, f)
        @test f_vaf ≈ MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
            [1.0],
        )
        f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
        @test f_vqf ≈ MOI.VectorQuadraticFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
            MOI.VectorQuadraticTerm{Float64}[],
            [1.0],
        )
    end
    @testset "ScalarQuadraticFunction" begin
        model = MOI.Utilities.Model{Float64}()
        x = MOI.add_variable(model)
        f = MOI.ScalarQuadraticFunction(
            [MOI.ScalarAffineTerm(2.0, x)],
            [MOI.ScalarQuadraticTerm(3.0, x, x)],
            1.0,
        )
        @test_throws(MethodError, convert(MOI.VectorOfVariables, f))
        @test_throws(MethodError, convert(MOI.VectorAffineFunction{Float64}, f))
        f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
        @test f_vqf ≈ MOI.VectorQuadraticFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
            MOI.VectorQuadraticTerm{Float64}[MOI.VectorQuadraticTerm(
                1,
                MOI.ScalarQuadraticTerm(3.0, x, x),
            )],
            [1.0],
        )
    end
end
