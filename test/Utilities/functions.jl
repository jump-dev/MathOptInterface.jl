using Compat
using Compat.Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

@testset "Function tests" begin
    w = MOI.VariableIndex(0)
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @testset "Vectorization" begin
        g = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 1],
                                                           MOI.ScalarAffineTerm.([5, 2],
                                                                                 [y, x])),
                                                           [3, 1, 4])
        @testset "vectorize" begin
            g1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2, x)], 3)
            g2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Int}[], 1)
            g3 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(5, y)], 4)
            @test g ≈ MOIU.vectorize([g1, g2, g3])
        end
        @testset "operate vcat" begin
            v = MOI.VectorOfVariables([y, w])
            f = MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([2, 4], [x, z]), 5)

            wf = MOI.SingleVariable(w)
            xf = MOI.SingleVariable(x)
            @test MOIU.promote_operation(vcat, Int, typeof(wf), typeof(f),
                                         typeof(v), Int, typeof(g), typeof(xf),
                                         Int) == MOI.VectorAffineFunction{Int}
            expected_terms = MOI.VectorAffineTerm.(
                [1, 2, 2, 3, 4, 8, 6, 9],
                MOI.ScalarAffineTerm.([1, 2, 4, 1, 1, 5, 2, 1],
                                      [w, x, z, y, w, y, x, x]))
            expected_constants = [0, 5, 0, 0, 3, 3, 1, 4, 0, -4]
            F = MOIU.operate(vcat, Int, wf, f, v, 3, g, xf, -4)
            @test F.terms == expected_terms
            @test F.constants == expected_constants
        end
    end
    @testset "MultirowChange construction" begin
        chg1 = MOI.MultirowChange(w, [(Int32(2), 2.0), (Int32(1), 3.0)])
        chg2 = MOI.MultirowChange(w, [(Int64(2), 2.0), (Int64(1), 3.0)])
        @test chg1.variable == chg2.variable
        @test chg1.new_coefficients == chg2.new_coefficients
    end
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
        F = MOIU.operate(vcat, Int, it[[1, 2]], it[3])
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
            @testset "zero" begin
                f = @inferred MOIU.zero(MOI.ScalarAffineFunction{Float64})
                @test iszero(f)
                @test MOIU.isapprox_zero(f, 1e-16)
            end
            @testset "promote_operation" begin
                @test MOIU.promote_operation(+, Float64, MOI.SingleVariable,
                                             MOI.SingleVariable) == MOI.ScalarAffineFunction{Float64}
                @test MOIU.promote_operation(+, Float64,
                                             MOI.ScalarAffineFunction{Float64},
                                             Float64) == MOI.ScalarAffineFunction{Float64}
                @test MOIU.promote_operation(+, Int,
                                             MOI.ScalarAffineFunction{Int},
                                             MOI.ScalarAffineFunction{Int}) == MOI.ScalarAffineFunction{Int}
            end
            @testset "Comparison" begin
                @test MOIU.operate(+, Float64, MOI.SingleVariable(x),
                                   MOI.SingleVariable(z)) + 1.0 ≈
                      MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 1e-7, 1], [x, y, z]), 1.0) atol=1e-6
                f1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1e-7, y)], 1.0)
                f2 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 1.0)
                @test f1 ≈ f2 atol=1e-6
                fdiff = f1 - f2
                @testset "With iszero" begin
                    @test !iszero(fdiff)
                    @test iszero(f1 - f1)
                    @test iszero(f2 - f2)
                end
                @testset "With tolerance" begin
                    MOIU.canonicalize!(fdiff)
                    @test !MOIU.isapprox_zero(fdiff, 1e-8)
                    @test MOIU.isapprox_zero(fdiff, 1e-6)
                end
            end
            @testset "canonical" begin
                f = MOIU.canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 1, 3, -2, -3], [y, x, z, x, z]), 5))
                @test MOI.output_dimension(f) == 1
                @test f.terms == MOI.ScalarAffineTerm.([-1, 2], [x, y])
                @test f.constant == 5
                f = MOIU.canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 3, 1, 2, -3, 2],
                                                                                  [w, y, w, x,  x, z]), 2) +
                                   MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1, -2, -2, 3, 2],
                                                                                  [ y,  z,  w, x, y]), 3))
                @test f.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
                @test f.constant == 5
            end
            f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 4], [x, y]),
                                         5)
            @testset "convert" begin
                @test_throws InexactError convert(MOI.SingleVariable, f)
                quad_f = MOI.ScalarQuadraticFunction(f.terms,
                                                     MOI.ScalarQuadraticTerm{Int}[],
                                                     f.constant)
                @test convert(MOI.ScalarQuadraticFunction{Int}, f) ≈ quad_f
                g = convert(MOI.ScalarAffineFunction{Float64}, MOI.SingleVariable(x))
                @test convert(MOI.SingleVariable, g) == MOI.SingleVariable(x)
            end
            @testset "operate with Float64 coefficient type" begin
                f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 4.0],
                                                                   [x, y]),
                                             5.0)
                @test f ≈ 2.0f / 2.0
            end
            @testset "operate with Int coefficient type" begin
                f = MOIU.canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 3, 1, 2, -3, 2],
                                                                                  [w, y, w, x,  x, z]), 2) +
                                   MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1, -2, -2, 3, 2],
                                                                                  [ y,  z,  w, x, y]), 3))
                @test f === +f
                @test f ≈ MOI.SingleVariable(x) + MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 4],
                                                                                                 [x, y]), 5)
                @test f ≈ f * 1
                @test f ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 2], [x, y]), 2) * 2 + 1
                @test f ≈ MOI.SingleVariable(x) - MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1, -4], [x, y]), -5)
                @test f ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3, 4], [x, y]), 5) - MOI.SingleVariable(x)
            end
            @testset "modification" begin
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
        end
        @testset "Quadratic" begin
            @testset "zero" begin
                f = @inferred MOIU.zero(MOI.ScalarQuadraticFunction{Float64})
                @test MOIU.isapprox_zero(f, 1e-16)
            end
            @testset "promote_operation" begin
                @test MOIU.promote_operation(+, Int,
                                             MOI.ScalarQuadraticFunction{Int},
                                             MOI.ScalarQuadraticFunction{Int}) == MOI.ScalarQuadraticFunction{Int}
                @test MOIU.promote_operation(+, Int,
                                             MOI.ScalarQuadraticFunction{Int},
                                             MOI.ScalarAffineFunction{Int}) == MOI.ScalarQuadraticFunction{Int}
                @test MOIU.promote_operation(+, Int,
                                             MOI.ScalarAffineFunction{Int},
                                             MOI.ScalarQuadraticFunction{Int}) == MOI.ScalarQuadraticFunction{Int}
                @test MOIU.promote_operation(*, Int,
                                             MOI.SingleVariable,
                                             MOI.SingleVariable) == MOI.ScalarQuadraticFunction{Int}
                @test MOIU.promote_operation(*, Float64,
                                             MOI.SingleVariable,
                                             MOI.ScalarAffineFunction{Float64}) == MOI.ScalarQuadraticFunction{Float64}
                @test MOIU.promote_operation(*, Int,
                                             MOI.ScalarAffineFunction{Int},
                                             MOI.SingleVariable) == MOI.ScalarQuadraticFunction{Int}
                @test MOIU.promote_operation(*, Float64,
                                             MOI.ScalarAffineFunction{Float64},
                                             MOI.ScalarAffineFunction{Float64}) == MOI.ScalarQuadraticFunction{Float64}
                @test MOIU.promote_operation(/, Float64,
                                             MOI.ScalarQuadraticFunction{Float64},
                                             Float64) == MOI.ScalarQuadraticFunction{Float64}
            end
            fx = MOI.SingleVariable(x)
            fy = MOI.SingleVariable(y)
            f = 7 + 3fx + 1fx * fx + 2fy * fy + 3fx * fy
            MOIU.canonicalize!(f)
            @test MOI.output_dimension(f) == 1
            @testset "Comparison" begin
                @testset "With iszero" begin
                    @test !iszero(f)
                    @test iszero(0 * f)
                    @test iszero(f - f)
                end
                @testset "With tolerance" begin
                    @test !MOIU.isapprox_zero(f, 1e-8)
                    # Test isapprox_zero with zero terms
                    @test MOIU.isapprox_zero(0 * f, 1e-8)
                    g = 1.0fx * fy - (1 + 1e-6) * fy * fx
                    MOIU.canonicalize!(g)
                    @test MOIU.isapprox_zero(g, 1e-5)
                    @test !MOIU.isapprox_zero(g, 1e-7)
                end
            end
            @testset "convert" begin
                @test_throws InexactError convert(MOI.SingleVariable, f)
                @test_throws InexactError convert(MOI.ScalarAffineFunction{Int},
                                                  f)
                g = convert(MOI.ScalarQuadraticFunction{Float64}, fx)
                @test convert(MOI.SingleVariable, g) == fx
            end
            @testset "operate" begin
                @test f ≈ 7 + (fx + 2fy) * (1fx + fy) + 3fx
                @test f ≈ -(-7 - 3fx) + (fx + 2fy) * (1fx + fy)
                @test f ≈ -((fx + 2fy) * (MOIU.operate(-, Int, fx) - fy)) + 3fx + 7
                @test f ≈ 7 + MOIU.operate(*, Int, fx, fx) + 3fx * (fy + 1) + 2fy * fy
                @test f ≈ (fx + 2) * (fx + 1) + (fy + 1) * (2fy + 3fx) + (5 - 3fx - 2fy)
                @test f ≈ begin
                    MOI.ScalarQuadraticFunction([MOI.ScalarAffineTerm(3, x)],
                                                MOI.ScalarQuadraticTerm.([1], [x], [x]), 4) +
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([2, 3], [y, x], [y, y]), 3)
                end
                @test f ≈ begin
                    MOI.ScalarQuadraticFunction([MOI.ScalarAffineTerm(3, x)],
                                                MOI.ScalarQuadraticTerm.([1], [x], [x]), 10) -
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                                MOI.ScalarQuadraticTerm.([-2, -3], [y, x], [y, y]), 3)
                end
                @test f ≈ begin
                    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) +
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                                MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 2)
                end
                @test f ≈ begin
                    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) -
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                                MOI.ScalarQuadraticTerm.([-1, -2, -3], [x, y, x], [x, y, y]), -2)
                end
                @test f ≈ begin
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                                MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 2) +
                    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5)
                end
                @test f ≈ begin
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                                MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 12) -
                    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-3, x)], 5)
                end
                @test f ≈ begin
                    MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([2], [x]),
                                                MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 7) +
                    MOI.SingleVariable(x)
                end
                @test f ≈ MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([3], [x]),
                                                      MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]), 10) - 3
            end
            @testset "modification" begin
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
    @testset "Conversion to canonical form" begin
        function isapprox_ordered(f1::T, f2::T) where {T <: Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}}
            ((MOI.term_indices.(f1.terms) == MOI.term_indices.(f2.terms)) &&
             (MOI._constant(f1) ≈ MOI._constant(f2)) &&
             (MOI.coefficient.(f1.terms) ≈ MOI.coefficient.(f2.terms)))
        end
        function test_canonicalization(f::T, expected::T) where {T <: Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}}
            @test MOIU.iscanonical(expected)
            g = @inferred(MOIU.canonical(f))
            @test isapprox_ordered(g, expected)
            @test MOIU.iscanonical(g)
            @test MOIU.iscanonical(expected)
            @test isapprox_ordered(MOIU.canonical(g), g)
            @test MOIU.canonical(g) !== g
            @test @allocated(MOIU.canonicalize!(f)) == 0
        end
        @testset "ScalarAffine" begin
            @test MOIU.iscanonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])), 1.0))
            @test !MOIU.iscanonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1])), 1.0))
            @test !MOIU.iscanonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([2, 1])), 2.0))
            @test !MOIU.iscanonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.0], MOI.VariableIndex.([1, 2])), 2.0))

            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]), 1.5),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]), 1.5),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0], [MOI.VariableIndex(1)]), -2.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]), -2.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0], [MOI.VariableIndex(2)]), -2.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]), -2.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0], [MOI.VariableIndex(2)]),  1.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0], [MOI.VariableIndex(2)]),  1.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 4.0], MOI.VariableIndex.([2, 1])), 0.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([4.0, 3.0], MOI.VariableIndex.([1, 2])), 0.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.1], MOI.VariableIndex.([1, 1])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.1], MOI.VariableIndex.([1])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.5, 2.0], MOI.VariableIndex.([1, 3, 1])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 0.5], MOI.VariableIndex.([1, 3])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.5, 2.0], MOI.VariableIndex.([1, 3, 1])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 0.5], MOI.VariableIndex.([1, 3])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0, 4.0, 5.0], MOI.VariableIndex.([1, 3, 1, 3, 2])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([4.0, 5.0, 6.0], MOI.VariableIndex.([1, 2, 3])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], MOI.VariableIndex.([7, 7])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0, 0.5], MOI.VariableIndex.([7, 7, 2])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.5], MOI.VariableIndex.([2])), 5.0),
                )
            test_canonicalization(
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -0.5, 0.5, -0.5], MOI.VariableIndex.([7, 7, 2, 7])), 5.0),
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.5], MOI.VariableIndex.([2])), 5.0),
                )
        end
        @testset "VectorAffine" begin
            @test MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.(Int[], MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[]))), Float64[]))
            @test !MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1]))), [1.0]))
            @test !MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([0.0, 1.0], MOI.VariableIndex.([1, 2]))), [1.0]))
            @test !MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 2]))), [1.0]))
            @test !MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([1.0, -1.0], MOI.VariableIndex.([1, 1]))), [1.0]))
            @test !MOIU.iscanonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 1]))), [1.0]))

            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.(Int[], MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[]))), Float64[]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.(Int[], MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[]))), Float64[])
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3], MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([5]))), [1.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.(Int[], MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[]))), [1.0])
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 2], MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([5, 6]))), [1.0, 2.0, 3.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 3], MOI.ScalarAffineTerm.([2.0, 1.0], MOI.VariableIndex.([6, 5]))), [1.0, 2.0, 3.0])
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 3], MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([6, 5]))), [1.0, 2.0, 3.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 3], MOI.ScalarAffineTerm.([2.0, 1.0], MOI.VariableIndex.([5, 6]))), [1.0, 2.0, 3.0])
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3, 3], MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([1, 1]))), [1.0, 2.0, 3.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([3], MOI.ScalarAffineTerm.([3.0], MOI.VariableIndex.([1]))), [1.0, 2.0, 3.0])
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1, 1], MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], MOI.VariableIndex.([1, 2, 1]))), [4.0, 5.0, 6.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([4.0, 2.0], MOI.VariableIndex.([1, 2]))), [4.0, 5.0, 6.0]),
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1, 1], MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], MOI.VariableIndex.([2, 1, 1]))), [4.0, 5.0, 6.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([5.0, 1.0], MOI.VariableIndex.([1, 2]))), [4.0, 5.0, 6.0]),
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 3, 3], MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], MOI.VariableIndex.([1, 1, 1]))), [4.0, 5.0, 6.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 3], MOI.ScalarAffineTerm.([1.0, 5.0], MOI.VariableIndex.([1, 1]))), [4.0, 5.0, 6.0]),
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 3, 3], MOI.ScalarAffineTerm.([1.0, -3.0, 3.0], MOI.VariableIndex.([1, 1, 1]))), [4.0, 5.0, 6.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2], MOI.ScalarAffineTerm.([1.0], MOI.VariableIndex.([1]))), [4.0, 5.0, 6.0]),
                )
            test_canonicalization(
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 3, 3, 3], MOI.ScalarAffineTerm.([1.0, 3.0, -1.0, -2.0], MOI.VariableIndex.([1, 1, 1, 1]))), [4.0, 5.0, 6.0]),
                MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2], MOI.ScalarAffineTerm.([1.0], MOI.VariableIndex.([1]))), [4.0, 5.0, 6.0]),
                )
        end
    end

    @testset "Vector operate tests" begin
        w = MOI.VariableIndex(0)
        x = MOI.VariableIndex(1)
        y = MOI.VariableIndex(2)

        @testset "Vector terms tests" begin
            at = MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(10.0, x))
            qt = MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(6.0, x, y))
            @test MOIU.operate_term(*, 3.0, at) ==
                MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(30.0, x))
            @test MOIU.operate_term(*, at, at) ==
                MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(100.0, x, x))
            @test MOIU.operate_term(*, 3.0, qt) ==
                MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(18.0, x, y))
            @test MOIU.operate_term(/, at, 2.0) ==
                MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(5.0, x))
            @test MOIU.operate_term(/, qt, 3.0) ==
                MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, y))
        end

        @testset "Vector promote tests" begin
            T = Float64
            for t1 in [MOI.VectorAffineFunction{T}, MOI.VectorOfVariables, Vector{T}]
                for t2 in [MOI.VectorAffineFunction{T}, MOI.VectorOfVariables]
                    @test MOIU.promote_operation(+, T, t1, t2) == MOI.VectorAffineFunction{T}
                    @test MOIU.promote_operation(-, T, t1, t2) == MOI.VectorAffineFunction{T}
                end
            end
            for t1 in [MOI.VectorQuadraticFunction{T}, MOI.VectorAffineFunction{T},
                    MOI.VectorOfVariables, Vector{T}]
                for t2 in [MOI.VectorQuadraticFunction{T}]
                    @test MOIU.promote_operation(+, T, t1, t2) == MOI.VectorQuadraticFunction{T}
                    @test MOIU.promote_operation(-, T, t1, t2) == MOI.VectorQuadraticFunction{T}
                end
            end
            for t in [MOI.VectorOfVariables, MOI.VectorAffineFunction{T}]
                @test MOIU.promote_operation(-, T, t) == MOI.VectorAffineFunction{T}
            end
            t = MOI.VectorQuadraticFunction{T}
            @test MOIU.promote_operation(-, T, t) == MOI.VectorQuadraticFunction{T}
        end

        α = [1, 2, 3]
        v = MOI.VectorOfVariables([y, w, y])
        g = MOI.VectorAffineFunction(
                MOI.VectorAffineTerm.([3, 1],
                    MOI.ScalarAffineTerm.([5, 2], [y, x])),
                [3, 1, 4])
        f = MOI.VectorQuadraticFunction(
                MOI.VectorAffineTerm.([1, 2, 2],
                    MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y])),
                MOI.VectorQuadraticTerm.([1, 1, 2],
                    MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])),
                [7, 3, 4])
        v_plus_g = MOI.VectorAffineFunction(
                    MOI.VectorAffineTerm.([3, 1, 1, 2, 3],
                        MOI.ScalarAffineTerm.([5, 2, 1, 1, 1], [y, x, y, w, y])),
                    [3, 1, 4])
        g_plus_α = MOI.VectorAffineFunction(
                    MOI.VectorAffineTerm.([3, 1],
                        MOI.ScalarAffineTerm.([5, 2],  [y, x])),
                    [4, 3, 7])
        α_minus_v = MOI.VectorAffineFunction(
                        MOI.VectorAffineTerm.([1, 2, 3],
                            MOI.ScalarAffineTerm.([-1, -1, -1], [y, w, y])),
                        [1, 2, 3])
        v_minus_v_plus_v = MOI.VectorAffineFunction(
                            MOI.VectorAffineTerm.([1, 2, 3, 1, 2, 3, 1, 2, 3],
                                MOI.ScalarAffineTerm.([1, 1, 1, -1, -1, -1, 1, 1, 1],
                                    [y, w, y, y, w, y, y, w, y])),
                            [0, 0, 0])
        f_plus_α = MOI.VectorQuadraticFunction(
                    MOI.VectorAffineTerm.([1, 2, 2],
                        MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y])),
                    MOI.VectorQuadraticTerm.([1, 1, 2],
                        MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])),
                    [8, 5, 7])
        f_minus_g = MOI.VectorQuadraticFunction(
                        MOI.VectorAffineTerm.([1, 2, 2, 3, 1],
                            MOI.ScalarAffineTerm.([3, 1, 2, -5, -2], [x, x, y, y, x])),
                        MOI.VectorQuadraticTerm.([1, 1, 2],
                            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])),
                        [4, 2, 0])
        @test v + g ≈ v_plus_g
        @test g + α ≈ g_plus_α
        @test α + g ≈ g_plus_α
        @test α - v ≈ α_minus_v
        @test MOIU.operate(+, Int, MOIU.operate(-, Int, v, v), v) ≈ v_minus_v_plus_v
        @test f + α ≈ f_plus_α
        @test f - g ≈ f_minus_g
        @test f - f + f - g ≈ f_minus_g
        @test v + f + α - v ≈ f_plus_α
        @test v - f - α - v ≈ - f_plus_α
        @test MOIU.operate!(-, Int, v, f) - v  ≈ - f
        @test (g + v + g + v + f) - (v + g + v + g) ≈ f
        @test v - α ≈ - α_minus_v
        @test g - f ≈ - f_minus_g
    end
end
