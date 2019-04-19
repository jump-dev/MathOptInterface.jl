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
