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
            @test MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 1], [x, z]), 1) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1e-7, 1.0], [x, y, z]), 1.0) atol=1e-6
            @test MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1e-7], [x, y]), 1.0) ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1, x)], 1) atol=1e-6
            f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 4], [x, y]), 6)
            g = deepcopy(f)
            @test g ≈ f
            f.terms[2] = MOI.ScalarAffineTerm(3, y)
            @test !(g ≈ f)
        end
        @testset "Vector" begin
            f = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1, 2],
                                                               MOI.ScalarAffineTerm.([2, 4, 3],
                                                                                     [x, y, y])), [6, 8])
            g = deepcopy(f)
            @test f ≈ g
            f.terms[3] = MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(9, y))
            @test !(f ≈ g)
            push!(f.terms, MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(-6, y)))
            @test f ≈ g
        end
    end
    @testset "Quadratic" begin
        @testset "Affine" begin
            f = MOI.ScalarQuadraticFunction([MOI.ScalarAffineTerm(3, x)],
                                            MOI.ScalarQuadraticTerm.([1, 2, 3],
                                                                     [x, y, x],
                                                                     [x, y, y]), 8)
            g = deepcopy(f)
            @test f ≈ g
            push!(f.affine_terms, MOI.ScalarAffineTerm(2, y))
            @test !(f ≈ g)
            g = deepcopy(f)
            push!(f.quadratic_terms, MOI.ScalarQuadraticTerm(2, y, x))
            @test !(f ≈ g)
            push!(f.quadratic_terms, MOI.ScalarQuadraticTerm(-2, y, x))
            @test f ≈ g
        end
        @testset "Vector" begin
            f = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([1, 2, 1], MOI.ScalarAffineTerm.([3, 1, 1],
                                                                                                   [x, x, y])),
                                            MOI.VectorQuadraticTerm.([1, 1, 2], MOI.ScalarQuadraticTerm.([1, 2, 3],
                                                                                                         [x, y, x],
                                                                                                         [x, y, y])), [10, 11, 12])
            g = deepcopy(f)
            @test f ≈ g
            f.affine_terms[1] = MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(4, x))
            @test !(f ≈ g)
            push!(g.affine_terms, MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-3, x)))
            push!(g.affine_terms, MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(4, x)))
            @test f ≈ g
            f.quadratic_terms[1] =  MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1, x, x))
            @test !(f ≈ g)
        end
    end
end
