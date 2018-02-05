@testset "Functions isapprox" begin
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @testset "Variable" begin
        @testset "Single" begin
            @test MOI.SingleVariable(x) == MOI.SingleVariable(x)
            @test MOI.SingleVariable(x) != MOI.SingleVariable(y)
        end
        @testset "Vector" begin
            @test MOI.VectorOfVariables([x, y]) == MOI.VectorOfVariables([x, y])
            @test MOI.VectorOfVariables([y, x]) != MOI.VectorOfVariables([x, y])
            @test MOI.VectorOfVariables([x, x]) != MOI.VectorOfVariables([x])
            @test MOI.VectorOfVariables([x]) != MOI.VectorOfVariables([y])
        end
    end
    @testset "Affine" begin
        @testset "Scalar" begin
            @test MOI.ScalarAffineFunction([x, z], [1, 1], 1) ≈ MOI.ScalarAffineFunction([x, y, z], [1, 1e-7, 1], 1.) atol=1e-6
            @test MOI.ScalarAffineFunction([x, y], [1, 1e-7], 1.) ≈ MOI.ScalarAffineFunction([x], [1], 1) atol=1e-6
            f = MOI.ScalarAffineFunction([x, y], [2, 4], 6)
            g = deepcopy(f)
            @test g ≈ f
            f.coefficients[2] = 3
            @test !(g ≈ f)
        end
        @testset "Vector" begin
            f = MOI.VectorAffineFunction([1, 1, 2],
                                         [x, y, y],
                                         [2, 4, 3], [6, 8])
            g = deepcopy(f)
            @test f ≈ g
            f.coefficients[3] = 9
            @test !(f ≈ g)
            push!(f.outputindex, 2)
            push!(f.variables, y)
            push!(f.coefficients, -6)
            @test f ≈ g
        end
    end
    @testset "Quadratic" begin
        @testset "Affine" begin
            f = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 8)
            g = deepcopy(f)
            @test f ≈ g
            push!(f.affine_variables, y)
            push!(f.affine_coefficients, 2)
            @test !(f ≈ g)
            g = deepcopy(f)
            push!(f.quadratic_rowvariables, y)
            push!(f.quadratic_colvariables, x)
            push!(f.quadratic_coefficients, 2)
            @test !(f ≈ g)
            push!(f.quadratic_rowvariables, y)
            push!(f.quadratic_colvariables, x)
            push!(f.quadratic_coefficients, -2)
            @test f ≈ g
        end
        @testset "Vector" begin
            f = MOI.VectorQuadraticFunction([1, 2, 1], [x, x, y], [3, 1, 1], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [10, 11, 12])
            g = deepcopy(f)
            @test f ≈ g
            f.affine_outputindex[1] = 3
            f.affine_coefficients[1] = 4
            @test !(f ≈ g)
            push!(g.affine_outputindex, 1)
            push!(g.affine_variables, x)
            push!(g.affine_coefficients, -3)
            push!(g.affine_outputindex, 3)
            push!(g.affine_variables, x)
            push!(g.affine_coefficients, 4)
            @test f ≈ g
            f.quadratic_outputindex[1] = 3
            @test !(f ≈ g)
        end
    end
end
