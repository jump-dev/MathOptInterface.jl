using Test
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities
import MutableArithmetics
const MA = MutableArithmetics

w = MOI.VariableIndex(0)
fw = MOI.SingleVariable(w)
x = MOI.VariableIndex(1)
fx = MOI.SingleVariable(x)
y = MOI.VariableIndex(2)
fy = MOI.SingleVariable(y)
z = MOI.VariableIndex(3)
fz = MOI.SingleVariable(z)

# Number-like but not subtype of `Number`
struct NonNumber
    value::Int
end
Base.:*(a::NonNumber, b::NonNumber) = NonNumber(a.value * b.value)
Base.:+(a::NonNumber, b::NonNumber) = NonNumber(a.value + b.value)
Base.zero(::Type{NonNumber}) = NonNumber(0)
Base.isapprox(a::NonNumber, b::NonNumber; kws...) = isapprox(a.value, b.value; kws...)
@testset "NonNumber" begin
    two = NonNumber(2)
    three = NonNumber(3)
    six = NonNumber(6)
    three_x = MOIU.operate(*, NonNumber, three, fx)
    six_x = MOIU.operate(*, NonNumber, six, fx)
    @test six_x ≈ two * three_x
    @test six_x ≈ three_x * two
end

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
        vov = MOIU.vectorize(MOI.SingleVariable[])
        @test MOI.output_dimension(vov) == 0
        @test vov isa MOI.VectorOfVariables
        aff = MOIU.vectorize(MOI.ScalarAffineFunction{Int}[])
        @test MOI.output_dimension(aff) == 0
        @test aff isa MOI.VectorAffineFunction{Int}
        quad = MOIU.vectorize(MOI.ScalarQuadraticFunction{Int}[])
        @test MOI.output_dimension(quad) == 0
        @test quad isa MOI.VectorQuadraticFunction{Int}
    end
    @testset "operate vcat" begin
        v = MOI.VectorOfVariables([y, w])
        wf = MOI.SingleVariable(w)
        xf = MOI.SingleVariable(x)
        @testset "Variable with $T" for T in [Int, Float64]
            @test MOI.VectorOfVariables == MOIU.promote_operation(vcat, T, typeof(wf), typeof(v), typeof(xf))
            vov = MOIU.operate(vcat, T, wf, v, xf)
            @test vov.variables == [w, y, w, x]
            @test MOI.VectorOfVariables == MOIU.promote_operation(vcat, T, typeof(v), typeof(wf), typeof(xf))
            vov = MOIU.operate(vcat, T, v, wf, xf)
            @test vov.variables == [y, w, w, x]
            @test MOI.VectorOfVariables == MOIU.promote_operation(vcat, T, typeof(wf), typeof(xf), typeof(v))
            vov = MOIU.operate(vcat, T, wf, xf, v)
            @test vov.variables == [w, x, y, w]
        end
        f = MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2, 4], [x, z]), 5)
        g = MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
            [3, 1, 4])
        @testset "Affine" begin
            @test MOIU.promote_operation(vcat, Int, typeof(wf), typeof(f),
                                         typeof(v), Int, typeof(g), typeof(xf),
                                         Int) == MOI.VectorAffineFunction{Int}
            F = MOIU.operate(vcat, Int, wf, f, v, 3, g, xf, -4)
            expected_terms = MOI.VectorAffineTerm.(
                [1, 2, 2, 3, 4, 8, 6, 9],
                MOI.ScalarAffineTerm.([1, 2, 4, 1, 1, 5, 2, 1],
                                      [w, x, z, y, w, y, x, x]))
            expected_constants = [0, 5, 0, 0, 3, 3, 1, 4, 0, -4]
            F = MOIU.operate(vcat, Int, wf, f, v, 3, g, xf, -4)
            @test F.terms == expected_terms
            @test F.constants == expected_constants
        end
        @testset "Quadratic" begin
            @test MOIU.promote_operation(
                vcat, Int, MOI.VectorQuadraticFunction{Int}, typeof(wf),
                typeof(f), typeof(v), Int, MOI.ScalarQuadraticFunction{Int},
                typeof(g), typeof(xf), Int) == MOI.VectorQuadraticFunction{Int}
            # TODO
        end
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
@testset "eval_variables" begin
    # We do tests twice to make sure the function is not modified
    vals = Dict(w=>0, x=>3, y=>1, z=>5)
    fsv = MOI.SingleVariable(z)
    @test MOI.output_dimension(fsv) == 1
    @test MOIU.eval_variables(vi -> vals[vi], fsv) ≈ 5
    @test MOIU.eval_variables(vi -> vals[vi], fsv) ≈ 5
    fvv = MOI.VectorOfVariables([x, z, y])
    @test MOI.output_dimension(fvv) == 3
    @test MOIU.eval_variables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
    @test MOIU.eval_variables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
    fsa = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(3.0, z), MOI.ScalarAffineTerm(2.0, y)], 2.0)
    @test MOI.output_dimension(fsa) == 1
    @test MOIU.eval_variables(vi -> vals[vi], fsa) ≈ 22
    @test MOIU.eval_variables(vi -> vals[vi], fsa) ≈ 22
    fva = MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1, 2], MOI.ScalarAffineTerm.([1.0, 3.0, 2.0], [x, z, y])), [-3.0, 2.0])
    @test MOI.output_dimension(fva) == 2
    @test MOIU.eval_variables(vi -> vals[vi], fva) ≈ [12, 7]
    @test MOIU.eval_variables(vi -> vals[vi], fva) ≈ [12, 7]
    fsq = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.(1.0, [x, y]),
                                      MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]), -3.0)
    @test MOI.output_dimension(fsq) == 1
    @test MOIU.eval_variables(vi -> vals[vi], fsq) ≈ 16
    @test MOIU.eval_variables(vi -> vals[vi], fsq) ≈ 16
    fvq = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
                                      MOI.VectorQuadraticTerm.([1, 2, 2], MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y])), [-3.0, -2.0])
    @test MOI.output_dimension(fvq) == 2
    @test MOIU.eval_variables(vi -> vals[vi], fvq) ≈ [13, 1]
    @test MOIU.eval_variables(vi -> vals[vi], fvq) ≈ [13, 1]
end
@testset "substitute_variables" begin
    # We do tests twice to make sure the function is not modified
    subs = Dict(w => 1.0fy + 1.0fz, x => 2.0fy + 1.0, y => 1.0fy, z => -1.0fw)
    vals = Dict(w => 0.0, x => 3.0, y => 1.0, z => 5.0)
    subs_vals = Dict(w => 6.0, x => 3.0, y => 1.0, z => 0.0)
    fsa = fx + 3.0fz + 2.0fy + 2.0
    subs_sa = -3.0fw + 4.0fy + 3.0
    @test MOIU.eval_variables(vi -> subs_vals[vi], fsa) == MOIU.eval_variables(vi -> vals[vi], subs_sa)
    @test MOIU.substitute_variables(vi -> subs[vi], fsa) ≈ subs_sa
    @test MOIU.substitute_variables(vi -> subs[vi], fsa) ≈ subs_sa
    fva = MOIU.operate(vcat, Float64, 3.0fz - 3.0, fx + 2.0fy + 2.0)
    subs_va = MOIU.operate(vcat, Float64, -3.0fw - 3.0, 4.0fy + 3.0)
    @test MOIU.eval_variables(vi -> subs_vals[vi], fva) == MOIU.eval_variables(vi -> vals[vi], subs_va)
    @test MOIU.substitute_variables(vi -> subs[vi], fva) ≈ subs_va
    @test MOIU.substitute_variables(vi -> subs[vi], fva) ≈ subs_va
    fsq = 1.0fx  + 1.0fy + 1.0fx * fz + 1.0fw * fz     + 1.0fw   * fy + 2.0fw     * fw      - 3.0
    #     2y + 1 +     y - 2yw - w    + (y + z) * (-w) + (y + z) * y  + 2 (y + z) * (y + z) - 3
    #     2y     +     y - 2yw - w    - yw - zw        + y^2 + zy     + 2y^2 + 2z^2 + 4yz   - 2
    #     3y - w + 3y^2 + 2z^2 - 3yw - zw + 5yz - 2
    subs_sq = 3.0fy - 1.0fw + 3.0fy * fy + 2.0fz * fz - 3.0fy * fw - 1.0fz * fw + 5.0fy * fz - 2.0
    @test MOIU.eval_variables(vi -> subs_vals[vi], fsq) == MOIU.eval_variables(vi -> vals[vi], subs_sq)
    @test MOIU.substitute_variables(vi -> subs[vi], fsq) ≈ subs_sq
    @test MOIU.substitute_variables(vi -> subs[vi], fsq) ≈ subs_sq
    fvq = MOIU.operate(vcat, Float64, 1.0fy + 1.0fx*fz - 3.0, 1.0fx + 1.0fw*fz + 1.0fw*fy - 2.0)
    subs_vq = MOIU.operate(vcat, Float64,
        1.0fy - fw - 2.0fy*fw - 3.0,
        2.0fy + 1.0fy*fy - 1.0fw*fy - 1.0fw*fz + 1.0fy*fz - 1.0)
    @test MOIU.eval_variables(vi -> subs_vals[vi], fvq) == MOIU.eval_variables(vi -> vals[vi], subs_vq)
    @test MOIU.substitute_variables(vi -> subs[vi], fvq) ≈ subs_vq
    @test MOIU.substitute_variables(vi -> subs[vi], fvq) ≈ subs_vq

    complex_aff = 2.0im * fy
    # Test that variables can be substituted for `MOI.ScalarAffineFunction{S}`
    # in a `MOI.ScalarAffineFunction{T}` where `S != T`.
    @test MOIU.substitute_variables(vi -> 1.5fx, complex_aff) ≈ 3.0im * fx
    float_aff = 2.0 * fy
    @test MOIU.substitute_variables(vi -> 3fx, float_aff) ≈ 6.0 * fx
    complex_quad = 1.5im * fy * fy
    @test MOIU.substitute_variables(vi -> 2fx, complex_quad) ≈ 6.0im * fx * fx
    int_quad = 3 * fy * fx
    @test MOIU.substitute_variables(vi -> true * fy, int_quad) ≈ 3 * fy * fy
end
@testset "map_indices" begin
    fsq = MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.(1.0, [x, y]),
                                      MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]), -3.0)
    index_map = Dict(x => y, y => z, w => w, z => x)
    gsq = MOIU.map_indices(index_map, fsq)
    sats = MOI.ScalarAffineTerm.(1.0, [y, z])
    sqts = MOI.ScalarQuadraticTerm.(1.0, [y, w, w], [x, x, z])
    @test gsq.affine_terms == sats
    @test gsq.quadratic_terms == sqts
    @test gsq.constant == -3.
    fvq = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
                                      MOI.VectorQuadraticTerm.([1, 2, 2], MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y])), [-3.0, -2.0])
    gvq = MOIU.map_indices(index_map, fvq)
    @test gvq.affine_terms == MOI.VectorAffineTerm.([2, 1], sats)
    @test gvq.quadratic_terms == MOI.VectorQuadraticTerm.([1, 2, 2], sqts)
    @test MOIU.constant_vector(gvq) == [-3., -2.]

    @testset "Constants" begin
        @test MOIU.map_indices(index_map, :s) == :s
        @test MOIU.map_indices(index_map, [:a, :b]) == [:a, :b]
        @test MOIU.map_indices(index_map, "s") == "s"
        @test MOIU.map_indices(index_map, ["a", "b"]) == ["a", "b"]
    end
end
@testset "Conversion VectorOfVariables -> VectorAffineFunction" begin
    f = MOI.VectorAffineFunction{Int}(MOI.VectorOfVariables([z, x, y]))
    @test f isa MOI.VectorAffineFunction{Int}
    @test f.terms == MOI.VectorAffineTerm.(1:3, MOI.ScalarAffineTerm.(ones(Int, 3), [z, x, y]))
    @test all(iszero.(MOIU.constant_vector(f)))
    f = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([x, w]))
    @test f isa MOI.VectorAffineFunction{Float64}
    @test f.terms == MOI.VectorAffineTerm.(1:2, MOI.ScalarAffineTerm.(1.0, [x, w]))
    @test all(iszero.(MOIU.constant_vector(f)))
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
    @test MOIU.constant_vector(h) == [5, 2]
    F = MOIU.operate(vcat, Int, it[[1, 2]], it[3])
    @test F isa MOI.VectorAffineFunction{Int}
    @test F.terms == MOI.VectorAffineTerm.([1, 1, 1, 2, 2, 2, 2, 3, 3], MOI.ScalarAffineTerm.([7, 1, 4, 1, 9, 3, 1, 2, 6], [y, z, x, x, z, y, y, z, x]))
    @test MOIU.constant_vector(F) == MOIU.constant_vector(f)
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
    @testset "Variable" begin
        f = MOI.SingleVariable(MOI.VariableIndex(0))
        g = MOI.SingleVariable(MOI.VariableIndex(1))
        @testset "one" begin
            @test !isone(f)
            @test !isone(g)
        end
        @testset "zero" begin
            @test !iszero(f)
            @test !iszero(g)
            @test f + 1 ≈ 1 + f
            @test (f + 1.0) - 1.0 ≈ (2.0f) / 2.0
            @test (f - 1.0) + 1.0 ≈ (2.0f) / 2.0
            @test (1.0 + f) - 1.0 ≈ (f * 2.0) / 2.0
            @test 1.0 - (1.0 - f) ≈ (f / 2.0) * 2.0
        end
        @testset "complex operations" begin
            @test real(f) === f
            @test MA.promote_operation(real, typeof(f)) == typeof(f)
            @testset "imag with $T" for T in [Int, Int32, Float64]
                z = MOIU.operate(imag, T, f)
                @test z isa MOI.ScalarAffineFunction{T}
                @test iszero(z)
                @test MOIU.promote_operation(imag, T, typeof(f)) == typeof(z)
            end
            @test conj(f) === f
            @test MA.promote_operation(conj, typeof(f)) == typeof(f)
        end
    end
    @testset "Affine" begin
        @testset "zero" begin
            f = @inferred MOIU.zero(MOI.ScalarAffineFunction{Float64})
            @test iszero(f)
            @test MOIU.isapprox_zero(f, 1e-16)
        end
        @testset "complex operations" begin
            fx = MOI.SingleVariable(MOI.VariableIndex(1))
            fy = MOI.SingleVariable(MOI.VariableIndex(2))
            r = 3fx + 4fy
            c = 2fx + 5fy
            f = (1 + 0im) * r + c * im
            @test real(f) ≈ r
            @test MOIU.operate(imag, Int, f) ≈ c
            @test imag(f) ≈ c
            @test conj(f) ≈ (1 + 0im) * r - c * im
            @test MA.promote_operation(real, typeof(f)) == typeof(r)
            @test MA.promote_operation(imag, typeof(f)) == typeof(c)
            @test MA.promote_operation(conj, typeof(f)) == typeof(f)
        end
        @testset "promote_operation" begin
            @test MOIU.promote_operation(
                -, Int, MOI.SingleVariable
            ) == MOI.ScalarAffineFunction{Int}
            @test MOIU.promote_operation(
                -, Int, MOI.ScalarAffineFunction{Int}
            ) == MOI.ScalarAffineFunction{Int}
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
        f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.5], [x, y]), 0.5)
        @testset "convert" begin
            @test_throws InexactError convert(MOI.SingleVariable, f)
            @test_throws InexactError MOIU.convert_approx(MOI.SingleVariable, f)
            @test MOIU.convert_approx(MOI.SingleVariable, f, tol = 0.5) == MOI.SingleVariable(x)
            @test convert(typeof(f), f) === f
            quad_f = MOI.ScalarQuadraticFunction(f.terms,
                                                 MOI.ScalarQuadraticTerm{Float64}[],
                                                 f.constant)
            @test convert(MOI.ScalarQuadraticFunction{Float64}, f) ≈ quad_f
            for g in [convert(MOI.ScalarAffineFunction{Float64}, MOI.SingleVariable(x)),
                      convert(MOI.ScalarAffineFunction{Float64}, 1MOI.SingleVariable(x))]
                @test g isa MOI.ScalarAffineFunction{Float64}
                @test convert(MOI.SingleVariable, g) == MOI.SingleVariable(x)
                @test MOIU.convert_approx(MOI.SingleVariable, g) == MOI.SingleVariable(x)
            end
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
            f = MOIU.modify_function(f, MOI.ScalarConstantChange(6))
            @test f.constant == 6
            g = deepcopy(f)
            @test g ≈ f
            f = MOIU.modify_function(f, MOI.ScalarCoefficientChange(y, 3))
            @test !(g ≈ f)
            @test g.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
            @test f.terms == MOI.ScalarAffineTerm.([2, 3], [x, y])
            f = MOIU.modify_function(f, MOI.ScalarCoefficientChange(x, 0))
            @test f.terms == MOI.ScalarAffineTerm.([3], [y])
        end
    end
    @testset "Quadratic" begin
        @testset "zero" begin
            f = @inferred MOIU.zero(MOI.ScalarQuadraticFunction{Float64})
            @test MOIU.isapprox_zero(f, 1e-16)
        end
        @testset "promote_operation" begin
            @test MOIU.promote_operation(
                -, Int, MOI.ScalarQuadraticFunction{Int}
            ) == MOI.ScalarQuadraticFunction{Int}
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
        @testset "Power" begin
            @testset "Affine" begin
                aff = 1fx + 2 + fy
                @test isone(@inferred aff^0)
                @test convert(typeof(f), aff) ≈ @inferred aff^1
                @test aff * aff ≈ @inferred aff^2
                err = ArgumentError("Cannot take $(typeof(aff)) to the power 3.")
                @test_throws err aff^3
            end
            @testset "Quadratic" begin
                @test isone(@inferred f^0)
                @test f ≈ @inferred f^1
                err = ArgumentError("Cannot take $(typeof(f)) to the power 2.")
                @test_throws err f^2
            end
        end
        @testset "operate" begin
            @testset "No zero affine term" begin
                # Test that no affine 0y term is created when multiplying 1fx by fy
                for fxfy in [1fx * fy, fx * 1fy]
                    @test isempty(fxfy.affine_terms)
                    @test length(fxfy.quadratic_terms) == 1
                    @test fxfy.quadratic_terms[1] == MOI.ScalarQuadraticTerm(1, x, y) ||
                        fxfy.quadratic_terms[1] == MOI.ScalarQuadraticTerm(1, y, x)
                end
                for fxfx in [1fx * fx, fx * 1fx]
                    @test isempty(fxfx.affine_terms)
                    @test length(fxfx.quadratic_terms) == 1
                    @test fxfx.quadratic_terms[1] == MOI.ScalarQuadraticTerm(2, x, x)
                end
            end
            @testset "operate!" begin
                q = 1.0fx + 1.0fy + (1.0fx) * fz + (1.0fw) * fz
                @test q ≈ 1.0fx + 1.0fy + (1.0fw) * fz + (1.0fx) * fz
                # This calls
                aff = 1.0fx + 1.0fy
                # which tries to mutate `aff`, gets a quadratic expression
                # and mutate it with the remaining term
                @test MOIU.operate!(+, Float64, aff, (1.0fx) * fz, (1.0fw) * fz) ≈ q
            end
            @test f ≈ 7 + (fx + 2fy) * (1fx + fy) + 3fx
            @test f ≈ -(-7 - 3fx) + (fx + 2fy) * (1fx + fy)
            @test f ≈ -((fx + 2fy) * (MOIU.operate(-, Int, fx) - fy)) + 3fx + 7
            @test f ≈ 7 + MOIU.operate(*, Int, fx, fx) + 3fx * (fy + 1) + 2fy * fy
            @test f ≈ (fx + 2) * (fx + 1) + (fy + 1) * (2fy + 3fx) + (5 - 3fx - 2fy)
            @test f ≈ begin
                MOI.ScalarQuadraticFunction([MOI.ScalarAffineTerm(3, x)],
                                            MOI.ScalarQuadraticTerm.([2], [x], [x]), 4) +
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                        MOI.ScalarQuadraticTerm.([4, 3], [y, x], [y, y]), 3)
            end
            @test f ≈ begin
                MOI.ScalarQuadraticFunction([MOI.ScalarAffineTerm(3, x)],
                                            MOI.ScalarQuadraticTerm.([2], [x], [x]), 10) -
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([-4, -3], [y, x], [y, y]), 3)
            end
            @test f ≈ begin
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) +
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]), 2)
            end
            @test f ≈ begin
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) -
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([-2, -4, -3], [x, y, x], [x, y, y]), -2)
            end
            @test f ≈ begin
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]), 2) +
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5)
            end
            @test f ≈ begin
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Int}[],
                                            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]), 12) -
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-3, x)], 5)
            end
            @test f ≈ begin
                MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([2], [x]),
                                            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]), 7) +
                MOI.SingleVariable(x)
            end
            @test f ≈ MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm.([3], [x]),
                                          MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]), 10) - 3
            @test f ≈ 2.0 * MOI.ScalarQuadraticFunction(
                MOI.ScalarAffineTerm.([3.0], [x]),
                MOI.ScalarQuadraticTerm.([2.0, 4.0, 3.0], [x, y, x], [x, y, y]), 7.0) / 2.0
        end
        @testset "modification" begin
            f = MOIU.modify_function(f, MOI.ScalarConstantChange(9))
            @test f.constant == 9
            f = MOIU.modify_function(f, MOI.ScalarCoefficientChange(y, 0))
            @test f.affine_terms == MOI.ScalarAffineTerm.([3], [x])
            g = deepcopy(f)
            @test f ≈ g
            f = MOIU.modify_function(f, MOI.ScalarCoefficientChange(y, 2))
            @test !(f ≈ g)
            @test g.affine_terms == MOI.ScalarAffineTerm.([3], [x])
            @test f.affine_terms == MOI.ScalarAffineTerm.([3, 2], [x, y])
        end
    end
end
@testset "Vector" begin
    @testset "Variable" begin
        f = MOI.VectorOfVariables([MOI.VariableIndex(1), MOI.VariableIndex(2)])
        @testset "complex operations" begin
            @test real(f) === f
            @test MA.promote_operation(real, typeof(f)) == typeof(f)
            @testset "imag with $T" for T in [Int, Int32, Float64]
                z = MOIU.operate(imag, T, f)
                @test z isa MOI.VectorAffineFunction{T}
                @test isempty(z.terms)
                @test all(iszero, z.constants)
                @test MOI.output_dimension(z) == MOI.output_dimension(f)
                @test MOIU.promote_operation(imag, T, typeof(f)) == typeof(z)
            end
            @test conj(f) === f
            @test MA.promote_operation(conj, typeof(f)) == typeof(f)
        end
    end
    @testset "Affine" begin
        @testset "complex operations" begin
            fx = MOI.VectorOfVariables([MOI.VariableIndex(1)])
            fy = MOI.VectorOfVariables([MOI.VariableIndex(2)])
            r = 3fx + 4fy
            c = 2fx + 5fy
            f = (1 + 0im) * r + c * im
            @test real(f) ≈ r
            @test MOIU.operate(imag, Int, f) ≈ c
            @test imag(f) ≈ c
            @test conj(f) ≈ (1 + 0im) * r - c * im
            @test MA.promote_operation(real, typeof(f)) == typeof(r)
            @test MA.promote_operation(imag, typeof(f)) == typeof(c)
            @test MA.promote_operation(conj, typeof(f)) == typeof(f)
        end

        f = MOIU.canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1, 2,  1,  1,  2, 2,  2, 2, 1, 1,  2, 1,  2],
                                                                          MOI.ScalarAffineTerm.([3, 2, 3, -3, -1, -2, 3, -2, 1, 3, 5, -2, 0, -1],
                                                                                                [x, x, z,  y,  y,  x, y,  z, x, y, y,  x, x,  z])), [5, 7]))
        @test MOI.output_dimension(f) == 2
        @test f.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]))
        @test MOIU.constant_vector(f) == [5, 7]
        f = MOIU.modify_function(f, MOI.VectorConstantChange([6, 8]))
        @test MOIU.constant_vector(f) == [6, 8]
        g = deepcopy(f)
        @test f ≈ g
        f = MOIU.modify_function(f, MOI.MultirowChange(y, [(2, 9)]))
        @test !(f ≈ g)
        @test f.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 9], [x, y, y]))
        @test g.terms == MOI.VectorAffineTerm.([1, 1, 2], MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]))
        f = MOIU.modify_function(f, MOI.MultirowChange(y, [(1, 0)]))
        @test f.terms == MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([2, 9], [x, y]))
    end
    @testset "Quadratic" begin
        f = MOI.VectorQuadraticFunction(MOI.VectorAffineTerm.([1, 2, 2], MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y])), MOI.VectorQuadraticTerm.([1, 1, 2], MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y])), [7, 3, 4])
        @test MOI.output_dimension(f) == 3
        f = MOIU.modify_function(f, MOI.VectorConstantChange([10, 11, 12]))
        @test MOIU.constant_vector(f) == [10, 11, 12]
        f = MOIU.modify_function(f, MOI.MultirowChange(y, [(2, 0), (1, 1)]))
        @test f.affine_terms == MOI.VectorAffineTerm.([1, 2, 1], MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]))
        g = deepcopy(f)
        f = MOIU.modify_function(f, MOI.MultirowChange(x, [(1, 0), (3, 4)]))
        @test f.affine_terms == MOI.VectorAffineTerm.([2, 1, 3], MOI.ScalarAffineTerm.([1, 1, 4], [x, y, x]))
        @test g.affine_terms == MOI.VectorAffineTerm.([1, 2, 1], MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]))
    end
end
@testset "Conversion to canonical form" begin
    function isapprox_ordered(f1::T, f2::T) where {T <: Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}}
        ((MOI.term_indices.(f1.terms) == MOI.term_indices.(f2.terms)) &&
         (MOI.constant(f1) ≈ MOI.constant(f2)) &&
         (MOI.coefficient.(f1.terms) ≈ MOI.coefficient.(f2.terms)))
    end
    function isapprox_ordered(f1::T, f2::T) where {T <: Union{MOI.ScalarQuadraticFunction, MOI.VectorQuadraticFunction}}
        ((MOI.term_indices.(f1.affine_terms) == MOI.term_indices.(f2.affine_terms)) &&
         (MOI.term_indices.(f1.quadratic_terms) == MOI.term_indices.(f2.quadratic_terms)) &&
         (MOI.constant(f1) ≈ MOI.constant(f2)) &&
         (MOI.coefficient.(f1.affine_terms) ≈ MOI.coefficient.(f2.affine_terms)) &&
         (MOI.coefficient.(f1.quadratic_terms) ≈ MOI.coefficient.(f2.quadratic_terms)))
    end
    function test_canonicalization(f::T, expected::T) where {T <: Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction, MOI.ScalarQuadraticFunction, MOI.VectorQuadraticFunction}}
        @test MOIU.is_canonical(expected)
        g = @inferred(MOIU.canonical(f))
        @test isapprox_ordered(g, expected)
        @test MOIU.is_canonical(g)
        @test MOIU.is_canonical(expected)
        @test isapprox_ordered(MOIU.canonical(g), g)
        @test MOIU.canonical(g) !== g
        @test @allocated(MOIU.canonicalize!(f)) == 0
    end
    @testset "ScalarAffine" begin
        @test MOIU.is_canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])), 1.0))
        @test !MOIU.is_canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1])), 1.0))
        @test !MOIU.is_canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([2, 1])), 2.0))
        @test !MOIU.is_canonical(MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.0], MOI.VariableIndex.([1, 2])), 2.0))

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
        @test MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.(Int[], MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[]))), Float64[]))
        @test !MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1], MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1]))), [1.0]))
        @test !MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([0.0, 1.0], MOI.VariableIndex.([1, 2]))), [1.0]))
        @test !MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 2]))), [1.0]))
        @test !MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([1.0, -1.0], MOI.VariableIndex.([1, 1]))), [1.0]))
        @test !MOIU.is_canonical(MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 1], MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 1]))), [1.0]))

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
    @testset "ScalarQuadratic" begin
        x = MOI.SingleVariable(MOI.VariableIndex(1))
        y = MOI.SingleVariable(MOI.VariableIndex(2))
        @test MOIU.is_canonical(convert(MOI.ScalarQuadraticFunction{Float64}, 1.0))
        @test !MOIU.is_canonical(1.0x*y + 2.0x*x + 2.0)
        @test !MOIU.is_canonical(1.0x*x + 0.0x*y + 2.0)

        test_canonicalization(
            convert(MOI.ScalarQuadraticFunction{Float64}, 1.5),
            convert(MOI.ScalarQuadraticFunction{Float64}, 1.5)
        )
        test_canonicalization(
            0.0x*y - 2.0,
            convert(MOI.ScalarQuadraticFunction{Float64}, -2.0)
        )
        test_canonicalization(
            3.0x*y + 4.0x*x + 0.0,
            4.0x*x + 3.0x*y + 0.0
        )
        test_canonicalization(
            1.0x*y + 0.1x*y + 5.0,
            1.1x*y + 5.0
        )
    end
    @testset "VectorQuadratic" begin
        x = MOI.SingleVariable(MOI.VariableIndex(1))
        y = MOI.SingleVariable(MOI.VariableIndex(2))
        @test MOIU.is_canonical(MOIU.operate(vcat, Float64, convert(MOI.ScalarQuadraticFunction{Float64}, 1.0)))
        @test !MOIU.is_canonical(MOIU.operate(vcat, Float64, 1.0x*y + 2.0x*x + 2.0))
        @test !MOIU.is_canonical(MOIU.operate(vcat, Float64, 1.0x*x + 0.0x*y + 2.0))

        test_canonicalization(
            MOIU.operate(
                vcat,
                Float64,
                1.5,
                0.0x*y - 2.0,
                3.0x*y + 4.0x*x + 0.0,
                1.0x*y + 0.1x*y + 5.0
            ),
            MOIU.operate(
                vcat,
                Float64,
                1.5,
                -2.0,
                4.0x*x + 3.0x*y + 0.0,
                1.1x*y + 5.0
            )
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
            MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(200.0, x, x))
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

    @testset "Vector + and - Vector" begin
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
    @testset "Vector * and / constant" begin
        v = MOI.VectorOfVariables([y, w, y])
        v2 = MOIU.operate(
            vcat, Float64,
            2.0fy,
            2.0fw,
            2.0fy)
        f = MOIU.operate(
            vcat, Float64,
            2.0fx + 3.0fy + 1.0,
            3.0fx + 2.0fy + 3.0)
        f2 = MOIU.operate(
            vcat, Float64,
            4.0fx + 6.0fy + 2.0,
            6.0fx + 4.0fy + 6.0)
        g = MOIU.operate(
            vcat, Float64,
            7.0fx * fy + 2.0fx + 3.0fy + 1.0,
            6.0fx * fx + 5.0fy * fy + 3.0fx + 2.0fy + 3.0)
        g2 = MOIU.operate(
            vcat, Float64,
            14.0fx * fy + 4.0fx + 6.0fy + 2.0,
            12.0fx * fx + 10.0fy * fy + 6.0fx + 4.0fy + 6.0)
        @testset "$(typeof(a))" for (a, a2) in [(v, v2), (f, f2), (g, g2)]
            @test a * 2.0 ≈ a2
            @test 2.0 * a ≈ a2
            @test a / 0.5 ≈ a2
        end
    end
end

@testset "modifycoefficient_duplicates" begin
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, x)],
        0.0,
    )
    new_f = MOI.Utilities.modify_function(
        f, MOI.ScalarCoefficientChange(x, 3.0)
    )
    @test new_f ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3.0, x)], 0.0)
end

@testset "modifycoefficients_duplicates" begin
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        ],
        [0.0, 0.0, 0.0],
    )
    new_f = MOI.Utilities.modify_function(
        f, MOI.MultirowChange(x, [(1, 3.0), (2, 0.5)]),
    )
    @test new_f ≈ MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(3.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.5, x)),
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        ],
        [0.0, 0.0, 0.0],
    )
end
