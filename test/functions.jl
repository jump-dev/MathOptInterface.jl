@testset "Functions" begin
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @testset "Copy" begin
        @testset "VectorOfVariables" begin
            f = MOI.VectorOfVariables([x, y])
            f_copy = copy(f)
            f_copy.variables[2] = z
            @test f.variables[2] == y
        end
    end
end
