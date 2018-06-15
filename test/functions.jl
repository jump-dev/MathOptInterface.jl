@testset "Function tests" begin
    w = MOI.VariableIndex(0)
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @testset "VectorAffineTerm/VectorQuadraticTerm construction" begin
        scalaraffine = MOI.ScalarAffineTerm(2.0, z)
        @test MOI.VectorAffineTerm(Int32(3), scalaraffine) === MOI.VectorAffineTerm(Int64(3), scalaraffine)
        @test MOI.VectorAffineTerm{Float64}(Int32(3), scalaraffine) === MOI.VectorAffineTerm(Int64(3), scalaraffine)
        scalarquad = MOI.ScalarQuadraticTerm(2.0, y, z)
        @test MOI.VectorQuadraticTerm(Int32(3), scalarquad) === MOI.VectorQuadraticTerm(Int64(3), scalarquad)
        @test MOI.VectorQuadraticTerm{Float64}(Int32(3), scalarquad) === MOI.VectorQuadraticTerm(Int64(3), scalarquad)
    end
    @testset "evalvariables" begin
        # We do tests twice to make sure the function is not modified
        vals = Dict(w=>0, x=>3, y=>1, z=>5)
        fsv = MOI.SingleVariable(z)
        @test MOI.output_dimension(fsv) == 1
        @test MOIU.evalvariables(vi -> vals[vi], fsv) ≈ 5
        @test MOIU.evalvariables(vi -> vals[vi], fsv) ≈ 5
        fvv = MOI.VectorOfVariables([x, z, y])
        @test MOI.output_dimension(fvv) == 3
        @test MOIU.evalvariables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
        @test MOIU.evalvariables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
        fsa = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(3.0, z), MOI.ScalarAffineTerm(2.0, y)], 2.0)
        @test MOI.output_dimension(fsa) == 1
        @test MOIU.evalvariables(vi -> vals[vi], fsa) ≈ 22
        @test MOIU.evalvariables(vi -> vals[vi], fsa) ≈ 22
        fva = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1, 2], MOI.ScalarAffineTerm.([1.0, 3.0, 2.0], [x, z, y])), [-3.0, 2.0])
        @test MOI.output_dimension(fva) == 2
        @test MOIU.evalvariables(vi -> vals[vi], fva) ≈ [12, 7]
        @test MOIU.evalvariables(vi -> vals[vi], fva) ≈ [12, 7]
        fsq = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.(1.0, [x, y]),
                                          MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]), -3.0)
        @test MOI.output_dimension(fsq) == 1
        @test MOIU.evalvariables(vi -> vals[vi], fsq) ≈ 16
        @test MOIU.evalvariables(vi -> vals[vi], fsq) ≈ 16
        fvq = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
                                          MOI.VectorQuadraticTerm.([1, 2, 2], MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y])), [-3.0, -2.0])
        @test MOI.output_dimension(fvq) == 2
        @test MOIU.evalvariables(vi -> vals[vi], fvq) ≈ [13, 1]
        @test MOIU.evalvariables(vi -> vals[vi], fvq) ≈ [13, 1]
    end
    @testset "mapvariables" begin
        fsq = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.(1.0, [x, y]),
                                          MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]), -3.0)
        gsq = MOIU.mapvariables(Dict(x => y, y => z, w => w, z => x), fsq)
        sats = MOI.ScalarAffineTerm.(1.0, [y, z])
        sqts = MOI.ScalarQuadraticTerm.(1.0, [y, w, w], [x, x, z])
        @test gsq.affine_terms == sats
        @test gsq.quadratic_terms == sqts
        @test gsq.constant == -3.
        fvq = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
                                          MOI.VectorQuadraticTerm.([1, 2, 2], MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y])), [-3.0, -2.0])
        gvq = MOIU.mapvariables(Dict(x => y, y => z, w => w, z => x), fvq)
        @test gvq.affine_terms == MOI.VectorAffineTerm.([2, 1], sats)
        @test gvq.quadratic_terms == MOI.VectorQuadraticTerm.([1, 2, 2], sqts)
        @test MOIU.constant(gvq) == [-3., -2.]
    end
    @testset "Conversion VectorOfVariables -> VectorAffineFunction" begin
        f = MOI.VectorAffineFunction{Int}(MOI.VectorOfVariables([z, x, y]))
        @test f isa MOI.VectorAffineFunction{Int}
        @test f.terms == MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(ones(Int, 3), [z, x, y]))
        @test all(iszero.(MOIU.constant(f)))
        f = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([x, w]))
        @test f isa MOI.VectorAffineFunction{Float64}
        @test f.terms == MOI.VectorAffineTerm.(1:2, MOI.ScalarAffineTerm.(1.0, [x, w]))
        @test all(iszero.(MOIU.constant(f)))
    end
    @testset "Iteration and indexing on VectorOfVariables" begin
        f = MOI.VectorOfVariables([z, w, x, y])
        it = MOIU.eachscalar(f)
        @test length(it) == 4
        @test eltype(it) == MOI.SingleVariable
        @test collect(it) == [MOI.SingleVariable(z), MOI.SingleVariable(w), MOI.SingleVariable(x), MOI.SingleVariable(y)]
        @test it[2] == MOI.SingleVariable(w)
        @test it[end] == MOI.SingleVariable(y)
    end
    @testset "Indexing on VectorAffineFunction" begin
        f = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1, 3, 2, 2, 1, 3, 1, 2],
                                                           MOI.ScalarAffineTerm.([1, 7, 2, 9, 3, 1, 6, 4, 1],
                                                                                 [x, y, z, z, y, z, x, x, y])),
                                     [2, 7, 5])
        it = MOIU.eachscalar(f)
        @test length(it) == 3
        @test eltype(it) == MOI.ScalarAffineFunction{Int}
        g = it[2]
        @test g isa MOI.ScalarAffineFunction
        @test g.terms == MOI.ScalarAffineTerm.([1, 9, 3, 1], [x, z, y, y])
        @test g.constant == 7
        g = it[1]
        @test g isa MOI.ScalarAffineFunction
        @test g.terms == MOI.ScalarAffineTerm.([7, 1, 4], [y, z, x])
        @test g.constant == 2
        g = it[end]
        @test g isa MOI.ScalarAffineFunction
        @test g.terms == MOI.ScalarAffineTerm.([2, 6], [z, x])
        @test g.constant == 5
        h = it[[3, 1]]
        @test h isa MOI.VectorAffineFunction
        @test h.terms == MOI.VectorAffineTerm.([1, 1, 2, 2, 2], MOI.ScalarAffineTerm.([2, 6, 7, 1, 4], [z, x, y, z, x]))
        @test MOIU.constant(h) == [5, 2]
        F = MOIU.moivcat(it[[1, 2]], it[3])
        @test F isa MOI.VectorAffineFunction{Int}
        @test F.terms == MOI.VectorAffineTerm.([1, 1, 1, 2, 2, 2, 2, 3, 3], MOI.ScalarAffineTerm.([7, 1, 4, 1, 9, 3, 1, 2, 6], [y, z, x, x, z, y, y, z, x]))
        @test MOIU.constant(F) == MOIU.constant(f)
    end
    @testset "Indexing on VectorQuadraticFunction" begin
        f = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([2, 1, 3, 2, 2],
                                                              MOI.ScalarAffineTerm.([1, 7, 2, 9, 3],
                                                                                    [x, y, z, z, y])),
                                        MOI.VectorQuadraticTerm.([2, 3, 1, 2],
                                                                 MOI.ScalarQuadraticTerm.([1, 6, 4, 3],
                                                                                          [z, x, x, y],
                                                                                          [y, z, z, y])),
                                        [2, 7, 5])
        it = MOIU.eachscalar(f)
        @test length(it) == 3
        @test eltype(it) == MOI.ScalarQuadraticFunction{Int}
        g = it[2]
        @test g isa MOI.ScalarQuadraticFunction
        @test g.affine_terms == MOI.ScalarAffineTerm.([1, 9, 3], [x, z, y])
        @test g.quadratic_terms == MOI.ScalarQuadraticTerm.([1, 3], [z, y], [y, y])
        @test g.constant == 7
        g = it[end]
        @test g isa MOI.ScalarQuadraticFunction
        @test g.affine_terms == MOI.ScalarAffineTerm.([2], [z])
        @test g.quadratic_terms == MOI.ScalarQuadraticTerm.([6], [x], [z])
        @test g.constant == 5
    end
    @testset "Scalar" begin
        @testset "Affine" begin
            @test MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 1], [x, z]), 1) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 1e-7, 1], [x, y, z]), 1.0) atol=1e-6
            @test MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1e-7, y)], 1.0) ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1, x)], 1) atol=1e-6
            f = MOIU.canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 1, 3, -2, -3], [y, x, z, x, z]), 5))
            @test MOI.output_dimension(f) == 1
            @test f.terms == MOI.ScalarAffineTerm.([-1, 2], [x, y])
            @test f.constant == 5
            f = MOIU.canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 3, 1, 2, -3, 2, -1, -2, -2, 3, 2],
                                                                              [w, y, w, x,  x, z,  y,  z,  w, x, y]), 5))
            @test f.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
            @test f.constant == 5
            f = MOIU.modifyfunction(f, MOI.ScalarConstantChange(6))
            @test f.constant == 6
            g = deepcopy(f)
            @test g ≈ f
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 3))
            @test !(g ≈ f)
            @test g.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
            @test f.terms == MOI.ScalarAffineTerm.([2, 3], [x, y])
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(x, 0))
            @test f.terms == MOI.ScalarAffineTerm.([3], [y])
        end
        @testset "Quadratic" begin
            f = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([3], [x]), MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 7)
            @test MOI.output_dimension(f) == 1
            f = MOIU.modifyfunction(f, MOI.ScalarConstantChange(9))
            @test f.constant == 9
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 0))
            @test f.affine_terms == MOI.ScalarAffineTerm.([3], [x])
            g = deepcopy(f)
            @test f ≈ g
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 2))
            @test !(f ≈ g)
            @test g.affine_terms == MOI.ScalarAffineTerm.([3], [x])
            @test f.affine_terms == MOI.ScalarAffineTerm.([3, 2], [x, y])
        end
    end
    @testset "Vector" begin
        @testset "Affine" begin
            f = MOIU.canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1, 2,  1,  1,  2, 2,  2, 2, 1, 1,  2, 1,  2],
                                                                              MOI.ScalarAffineTerm.([3, 2, 3, -3, -1, -2, 3, -2, 1, 3, 5, -2, 0, -1],
                                                                                                    [x, x, z,  y,  y,  x, y,  z, x, y, y,  x, x,  z])), [5, 7]))
            @test MOI.output_dimension(f) == 2
            @test f.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]))
            @test MOIU.constant(f) == [5, 7]
            f = MOIU.modifyfunction(f, MOI.VectorConstantChange([6, 8]))
            @test MOIU.constant(f) == [6, 8]
            g = deepcopy(f)
            @test f ≈ g
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [(2, 9)]))
            @test !(f ≈ g)
            @test f.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 9], [x, y, y]))
            @test g.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]))
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [(1, 0)]))
            @test f.terms == MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([2, 9], [x, y]))
        end
        @testset "Quadratic" begin
            f = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([1, 2, 2], MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y])), MOI.VectorQuadraticTerm.([1, 1, 2], MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])), [7, 3, 4])
            @test MOI.output_dimension(f) == 3
            f = MOIU.modifyfunction(f, MOI.VectorConstantChange([10, 11, 12]))
            @test MOIU.constant(f) == [10, 11, 12]
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [(2, 0), (1, 1)]))
            @test f.affine_terms == MOI.VectorAffineTerm.([1, 2, 1], MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]))
            g = deepcopy(f)
            f = MOIU.modifyfunction(f, MOI.MultirowChange(x, [(1, 0), (3, 4)]))
            @test f.affine_terms == MOI.VectorAffineTerm.([2, 1, 3], MOI.ScalarAffineTerm.([1, 1, 4], [x, y, x]))
            @test g.affine_terms == MOI.VectorAffineTerm.([1, 2, 1], MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]))
        end
    end
end
