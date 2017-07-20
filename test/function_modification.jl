@testset "Function modification" begin
    x = MOI.VariableReference(1)
    y = MOI.VariableReference(2)
    @testset "Scalar" begin
        @testset "Affine" begin
            f = MOI.ScalarAffineFunction([x, y], [2, 4], 5)
            f = MOI.modifyfunction(f, MOI.ScalarConstantChange(6))
            @test f.constant == 6
            f = MOI.modifyfunction(f, MOI.ScalarCoefficientChange(y, 3))
            @test f.variables == [x, y]
            @test f.coefficients == [2, 3]
            f = MOI.modifyfunction(f, MOI.ScalarCoefficientChange(x, 0))
            @test f.variables == [y]
            @test f.coefficients == [3]
        end
        @testset "Quadratic" begin
            f = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 7)
            f = MOI.modifyfunction(f, MOI.ScalarConstantChange(9))
            @test f.constant == 9
            f = MOI.modifyfunction(f, MOI.ScalarCoefficientChange(y, 0))
            @test f.affine_variables == [x]
            @test f.affine_coefficients == [3]
            f = MOI.modifyfunction(f, MOI.ScalarCoefficientChange(y, 2))
            @test f.affine_variables == [x, y]
            @test f.affine_coefficients == [3, 2]
        end
    end
    @testset "Vector" begin
        @testset "Affine" begin
            f = MOI.VectorAffineFunction([1, 1, 2], [x, y, y], [2, 4, 3], [5, 7])
            f = MOI.modifyfunction(f, MOI.VectorConstantChange([6, 8]))
            @test f.constant == [6, 8]
            f = MOI.modifyfunction(f, MOI.MultirowChange(y, [2], [9]))
            @test f.outputindex == [1, 1, 2]
            @test f.variables == [x, y, y]
            @test f.coefficients == [2, 4, 9]
            f = MOI.modifyfunction(f, MOI.MultirowChange(y, [1], [0]))
            @test f.outputindex == [1, 2]
            @test f.variables == [x, y]
            @test f.coefficients == [2, 9]
        end
        @testset "Quadratic" begin
            f = MOI.VectorQuadraticFunction([1, 2, 2], [x, x, y], [3, 1, 2], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [7, 3, 4])
            f = MOI.modifyfunction(f, MOI.VectorConstantChange([10, 11, 12]))
            @test f.constant == [10, 11, 12]
            f = MOI.modifyfunction(f, MOI.MultirowChange(y, [2, 1], [0, 1]))
            @test f.affine_outputindex == [1, 2, 1]
            @test f.affine_variables == [x, x, y]
            @test f.affine_coefficients == [3, 1, 1]
            f = MOI.modifyfunction(f, MOI.MultirowChange(x, [1, 3], [0, 4]))
            @test f.affine_outputindex == [2, 1, 3]
            @test f.affine_variables == [x, y, x]
            @test f.affine_coefficients == [1, 1, 4]
        end
    end
end
