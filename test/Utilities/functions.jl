# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestUtilitiesFunctions

using Test

import MathOptInterface as MOI
import MutableArithmetics as MA

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

const w = MOI.VariableIndex(0)
const x = MOI.VariableIndex(1)
const y = MOI.VariableIndex(2)
const z = MOI.VariableIndex(3)

function test_Vectorization_vectorize()
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
        [3, 1, 4],
    )
    g1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2, x)], 3)
    g2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Int}[], 1)
    g3 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(5, y)], 4)
    @test g ≈ MOI.Utilities.vectorize([g1, g2, g3])
    vov = MOI.Utilities.vectorize(MOI.VariableIndex[])
    @test MOI.output_dimension(vov) == 0
    @test vov isa MOI.VectorOfVariables
    aff = MOI.Utilities.vectorize(MOI.ScalarAffineFunction{Int}[])
    @test MOI.output_dimension(aff) == 0
    @test aff isa MOI.VectorAffineFunction{Int}
    quad = MOI.Utilities.vectorize(MOI.ScalarQuadraticFunction{Int}[])
    @test MOI.output_dimension(quad) == 0
    @test quad isa MOI.VectorQuadraticFunction{Int}
    return
end

function test_Vectorization_operate_vcat()
    for T in [Int, Float64]
        a = one(T)
        b = zero(T)
        c = 2a
        x = [b, c]
        @test MOI.Utilities.promote_operation(vcat, T, T, Vector{T}, T) ==
              Vector{T}
        @test MOI.Utilities.operate(vcat, T, a, x, b) == vcat(a, x, b)
        @test MOI.Utilities.promote_operation(vcat, T, T) == Vector{T}
        @test MOI.Utilities.operate(vcat, T, a) == [a]
        @test MOI.Utilities.promote_operation(vcat, T, Vector{T}) == Vector{T}
        @test MOI.Utilities.operate(vcat, T, x) == x
    end
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
        [3, 1, 4],
    )
    v = MOI.VectorOfVariables([y, w])
    for T in [Int, Float64]
        @test MOI.VectorOfVariables == MOI.Utilities.promote_operation(
            vcat,
            T,
            typeof(w),
            typeof(v),
            typeof(x),
        )
        vov = MOI.Utilities.operate(vcat, T, w, v, x)
        @test vov.variables == [w, y, w, x]
        @test MOI.VectorOfVariables == MOI.Utilities.promote_operation(
            vcat,
            T,
            typeof(v),
            typeof(w),
            typeof(x),
        )
        vov = MOI.Utilities.operate(vcat, T, v, w, x)
        @test vov.variables == [y, w, w, x]
        @test MOI.VectorOfVariables == MOI.Utilities.promote_operation(
            vcat,
            T,
            typeof(w),
            typeof(x),
            typeof(v),
        )
        vov = MOI.Utilities.operate(vcat, T, w, x, v)
        @test vov.variables == [w, x, y, w]
    end
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 4], [x, z]), 5)
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
        [3, 1, 4],
    )
    @test MOI.Utilities.promote_operation(
        vcat,
        Int,
        typeof(w),
        typeof(f),
        typeof(v),
        Int,
        typeof(g),
        typeof(x),
        Int,
    ) == MOI.VectorAffineFunction{Int}
    F = MOI.Utilities.operate(vcat, Int, w, f, v, 3, g, x, -4)
    expected_terms = MOI.VectorAffineTerm.(
        [1, 2, 2, 3, 4, 8, 6, 9],
        MOI.ScalarAffineTerm.(
            [1, 2, 4, 1, 1, 5, 2, 1],
            [w, x, z, y, w, y, x, x],
        ),
    )
    expected_constants = [0, 5, 0, 0, 3, 3, 1, 4, 0, -4]
    F = MOI.Utilities.operate(vcat, Int, w, f, v, 3, g, x, -4)
    @test F.terms == expected_terms
    @test F.constants == expected_constants
    return
end

function test_promote_operation_Quadratic()
    @test MOI.Utilities.promote_operation(
        vcat,
        Int,
        MOI.VectorQuadraticFunction{Int},
        MOI.VariableIndex,
        MOI.ScalarQuadraticFunction{Int},
        MOI.VectorOfVariables,
        Int,
        MOI.ScalarQuadraticFunction{Int},
        MOI.VectorQuadraticFunction{Int},
        MOI.VariableIndex,
        Int,
    ) == MOI.VectorQuadraticFunction{Int}
    for T in (Float64, Int)
        for TV in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})
            @test promote_type(TV, MOI.VariableIndex) == TV
        end
    end
    return
end

function test_MultirowChange_construction()
    chg1 = MOI.MultirowChange(w, [(Int32(2), 2.0), (Int32(1), 3.0)])
    chg2 = MOI.MultirowChange(w, [(Int64(2), 2.0), (Int64(1), 3.0)])
    @test chg1.variable == chg2.variable
    @test chg1.new_coefficients == chg2.new_coefficients
    return
end

function test_VectorAffineTerm_VectorQuadraticTerm_construction()
    scalaraffine = MOI.ScalarAffineTerm(2.0, z)
    @test MOI.VectorAffineTerm(Int32(3), scalaraffine) ===
          MOI.VectorAffineTerm(Int64(3), scalaraffine)
    @test MOI.VectorAffineTerm{Float64}(Int32(3), scalaraffine) ===
          MOI.VectorAffineTerm(Int64(3), scalaraffine)
    scalarquad = MOI.ScalarQuadraticTerm(2.0, y, z)
    @test MOI.VectorQuadraticTerm(Int32(3), scalarquad) ===
          MOI.VectorQuadraticTerm(Int64(3), scalarquad)
    @test MOI.VectorQuadraticTerm{Float64}(Int32(3), scalarquad) ===
          MOI.VectorQuadraticTerm(Int64(3), scalarquad)
    return
end

function test_eval_variables()
    # We do tests twice to make sure the function is not modified
    vals = Dict(w => 0, x => 3, y => 1, z => 5)
    @test MOI.output_dimension(z) == 1
    @test MOI.Utilities.eval_variables(vi -> vals[vi], z) ≈ 5
    @test MOI.Utilities.eval_variables(vi -> vals[vi], z) ≈ 5
    fvv = MOI.VectorOfVariables([x, z, y])
    @test MOI.output_dimension(fvv) == 3
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fvv) ≈ [3, 5, 1]
    fsa = MOI.ScalarAffineFunction(
        [
            MOI.ScalarAffineTerm(1.0, x),
            MOI.ScalarAffineTerm(3.0, z),
            MOI.ScalarAffineTerm(2.0, y),
        ],
        2.0,
    )
    @test MOI.output_dimension(fsa) == 1
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fsa) ≈ 22
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fsa) ≈ 22
    fva = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [2, 1, 2],
            MOI.ScalarAffineTerm.([1.0, 3.0, 2.0], [x, z, y]),
        ),
        [-3.0, 2.0],
    )
    @test MOI.output_dimension(fva) == 2
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fva) ≈ [12, 7]
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fva) ≈ [12, 7]
    fsq = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]),
        MOI.ScalarAffineTerm.(1.0, [x, y]),
        -3.0,
    )
    @test MOI.output_dimension(fsq) == 1
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fsq) ≈ 16
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fsq) ≈ 16
    fvq = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 2, 2],
            MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]),
        ),
        MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
        [-3.0, -2.0],
    )
    @test MOI.output_dimension(fvq) == 2
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fvq) ≈ [13, 1]
    @test MOI.Utilities.eval_variables(vi -> vals[vi], fvq) ≈ [13, 1]
    return
end

function test_eval_variables_scalar_nonlinear_function()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    @test MOI.Utilities.eval_variables(xi -> 0.5, model, f) ≈ log(0.5)
    f = MOI.ScalarNonlinearFunction(:*, Any[x, 2.0*x, 1.0*x+2.0])
    @test MOI.Utilities.eval_variables(xi -> 0.5, model, f) ≈
          0.5 * (2.0 * 0.5) * (1.0 * 0.5 + 2.0)
    f = MOI.ScalarNonlinearFunction(
        :ifelse,
        Any[MOI.ScalarNonlinearFunction(:<, Any[x, 1.0]), 0.0, x],
    )
    @test MOI.Utilities.eval_variables(xi -> 0.5, model, f) ≈ 0.0
    @test MOI.Utilities.eval_variables(xi -> 1.5, model, f) ≈ 1.5
    f = MOI.ScalarNonlinearFunction(
        :||,
        Any[
            MOI.ScalarNonlinearFunction(:<, Any[x, 0.0]),
            MOI.ScalarNonlinearFunction(:>, Any[x, 1.0]),
        ],
    )
    @test MOI.Utilities.eval_variables(xi -> 0.5, model, f) ≈ 0.0
    @test MOI.Utilities.eval_variables(xi -> 1.5, model, f) ≈ 1.0
    @test MOI.Utilities.eval_variables(xi -> -0.5, model, f) ≈ 1.0
    my_square(x, y) = (x - y)^2
    MOI.set(model, MOI.UserDefinedFunction(:my_square, 2), (my_square,))
    f = MOI.ScalarNonlinearFunction(:my_square, Any[x, 1.0])
    @test MOI.Utilities.eval_variables(xi -> 0.5, model, f) ≈ (0.5 - 1.0)^2
    f = MOI.ScalarNonlinearFunction(:bad_f, Any[x, 1.0])
    @test_throws(
        MOI.UnsupportedNonlinearOperator(:bad_f),
        MOI.Utilities.eval_variables(xi -> 0.5, model, f)
    )
    return
end

function test_eval_variables_vector_nonlinear_function()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.VectorNonlinearFunction(Any[1.0, x, 2.0*x, f])
    @test ≈(
        MOI.Utilities.eval_variables(xi -> 0.5, model, g),
        [1.0, 0.5, 1.0, log(0.5)],
    )
    return
end

function test_substitute_variables()
    # We do tests twice to make sure the function is not modified
    subs = Dict(w => 1.0y + 1.0z, x => 2.0y + 1.0, y => 1.0y, z => -1.0w)
    vals = Dict(w => 0.0, x => 3.0, y => 1.0, z => 5.0)
    subs_vals = Dict(w => 6.0, x => 3.0, y => 1.0, z => 0.0)
    fsa = x + 3.0z + 2.0y + 2.0
    subs_sa = -3.0w + 4.0y + 3.0
    @test MOI.Utilities.eval_variables(vi -> subs_vals[vi], fsa) ==
          MOI.Utilities.eval_variables(vi -> vals[vi], subs_sa)
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fsa) ≈ subs_sa
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fsa) ≈ subs_sa
    fva = MOI.Utilities.operate(vcat, Float64, 3.0z - 3.0, x + 2.0y + 2.0)
    subs_va = MOI.Utilities.operate(vcat, Float64, -3.0w - 3.0, 4.0y + 3.0)
    @test MOI.Utilities.eval_variables(vi -> subs_vals[vi], fva) ==
          MOI.Utilities.eval_variables(vi -> vals[vi], subs_va)
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fva) ≈ subs_va
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fva) ≈ subs_va
    fsq = 1.0x + 1.0y + 1.0x * z + 1.0w * z + 1.0w * y + 2.0w * w - 3.0
    #     2y + 1 +     y - 2yw - w    + (y + z) * (-w) + (y + z) * y  + 2 (y + z) * (y + z) - 3
    #     2y     +     y - 2yw - w    - yw - zw        + y^2 + zy     + 2y^2 + 2z^2 + 4yz   - 2
    #     3y - w + 3y^2 + 2z^2 - 3yw - zw + 5yz - 2
    subs_sq =
        3.0y - 1.0w + 3.0y * y + 2.0z * z - 3.0y * w - 1.0z * w + 5.0y * z - 2.0
    @test MOI.Utilities.eval_variables(vi -> subs_vals[vi], fsq) ==
          MOI.Utilities.eval_variables(vi -> vals[vi], subs_sq)
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fsq) ≈ subs_sq
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fsq) ≈ subs_sq
    fvq = MOI.Utilities.operate(
        vcat,
        Float64,
        1.0y + 1.0x * z - 3.0,
        1.0x + 1.0w * z + 1.0w * y - 2.0,
    )
    subs_vq = MOI.Utilities.operate(
        vcat,
        Float64,
        1.0y - w - 2.0y * w - 3.0,
        2.0y + 1.0y * y - 1.0w * y - 1.0w * z + 1.0y * z - 1.0,
    )
    @test MOI.Utilities.eval_variables(vi -> subs_vals[vi], fvq) ==
          MOI.Utilities.eval_variables(vi -> vals[vi], subs_vq)
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fvq) ≈ subs_vq
    @test MOI.Utilities.substitute_variables(vi -> subs[vi], fvq) ≈ subs_vq

    complex_aff = 2.0im * y
    # Test that variables can be substituted for `MOI.ScalarAffineFunction{S}`
    # in a `MOI.ScalarAffineFunction{T}` where `S != T`.
    @test MOI.Utilities.substitute_variables(vi -> 1.5x, complex_aff) ≈
          3.0im * x
    float_aff = 2.0 * y
    @test MOI.Utilities.substitute_variables(vi -> 3x, float_aff) ≈ 6.0 * x
    complex_quad = 1.5im * y * y
    @test MOI.Utilities.substitute_variables(vi -> 2x, complex_quad) ≈
          6.0im * x * x
    int_quad = 3 * y * x
    @test MOI.Utilities.substitute_variables(vi -> true * y, int_quad) ≈
          3 * y * y
    return
end

function test_substitute_variables_vector_nonlinear_function()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.VectorNonlinearFunction(Any[1.0, x, 2.0*x, f])
    @test ≈(
        MOI.Utilities.substitute_variables(x -> 1.5 * x, g),
        MOI.VectorNonlinearFunction([
            MOI.ScalarNonlinearFunction(:+, Any[1.0]),
            MOI.ScalarNonlinearFunction(:+, Any[1.5*x]),
            MOI.ScalarNonlinearFunction(
                :+,
                Any[MOI.ScalarNonlinearFunction(:*, Any[2.0, 1.5*x])],
            ),
            MOI.ScalarNonlinearFunction(:log, Any[1.5*x]),
        ]),
    )
    return
end

function test_canonicalize_vector_nonlinear_function()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[1.0*x+2.0*x])
    fi = 1.0 * x + 1.0 * x
    @test length(fi.terms) == 2
    g = MOI.VectorNonlinearFunction(Any[1.0, x, fi, f])
    @test g.rows[4] === f
    MOI.Utilities.canonicalize!(g)
    @test g.rows[4] === f
    @test length(f.args[1].terms) == 1
    return
end

function test_eachscalar_vector_nonlinear_function()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:log, Any[1.0*x+2.0*x])
    g = MOI.VectorNonlinearFunction(Any[1.0, x, 2.0*x, f])
    scalars = MOI.Utilities.eachscalar(g)
    @test eltype(scalars) == MOI.ScalarNonlinearFunction
    @test ≈(scalars[1], MOI.ScalarNonlinearFunction(:+, Any[1.0]))
    @test ≈(scalars[2], MOI.ScalarNonlinearFunction(:+, Any[x]))
    @test ≈(scalars[3], convert(MOI.ScalarNonlinearFunction, 2.0 * x))
    @test ≈(scalars[4], f)
    @test ≈(scalars[2:3], MOI.VectorNonlinearFunction(Any[x, 2.0*x]))
    return
end

function test_map_indices()
    fsq = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]),
        MOI.ScalarAffineTerm.(1.0, [x, y]),
        -3.0,
    )
    index_map = Dict(x => y, y => z, w => w, z => x)
    gsq = MOI.Utilities.map_indices(index_map, fsq)
    sats = MOI.ScalarAffineTerm.(1.0, [y, z])
    sqts = MOI.ScalarQuadraticTerm.(1.0, [y, w, w], [x, x, z])
    @test gsq.affine_terms == sats
    @test gsq.quadratic_terms == sqts
    @test gsq.constant == -3.0
    fvq = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 2, 2],
            MOI.ScalarQuadraticTerm.(1.0, [x, w, w], [z, z, y]),
        ),
        MOI.VectorAffineTerm.([2, 1], MOI.ScalarAffineTerm.(1.0, [x, y])),
        [-3.0, -2.0],
    )
    gvq = MOI.Utilities.map_indices(index_map, fvq)
    @test gvq.affine_terms == MOI.VectorAffineTerm.([2, 1], sats)
    @test gvq.quadratic_terms == MOI.VectorQuadraticTerm.([1, 2, 2], sqts)
    @test MOI.Utilities.constant_vector(gvq) == [-3.0, -2.0]
    @test MOI.Utilities.map_indices(index_map, :s) == :s
    @test MOI.Utilities.map_indices(index_map, [:a, :b]) == [:a, :b]
    @test MOI.Utilities.map_indices(index_map, "s") == "s"
    @test MOI.Utilities.map_indices(index_map, ["a", "b"]) == ["a", "b"]
    return
end

function test_Conversion_VectorOfVariables_VectorAffineFunction()
    f = MOI.VectorAffineFunction{Int}(MOI.VectorOfVariables([z, x, y]))
    @test f isa MOI.VectorAffineFunction{Int}
    @test f.terms == MOI.VectorAffineTerm.(
        1:3,
        MOI.ScalarAffineTerm.(ones(Int, 3), [z, x, y]),
    )
    @test all(iszero.(MOI.Utilities.constant_vector(f)))
    f = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([x, w]))
    @test f isa MOI.VectorAffineFunction{Float64}
    @test f.terms ==
          MOI.VectorAffineTerm.(1:2, MOI.ScalarAffineTerm.(1.0, [x, w]))
    @test all(iszero.(MOI.Utilities.constant_vector(f)))
    return
end

function test_iteration_and_indexing_on_VectorOfVariables()
    f = MOI.VectorOfVariables([z, w, x, y])
    it = MOI.Utilities.eachscalar(f)
    @test it isa AbstractVector{MOI.VariableIndex}
    @test size(it) == (4,)
    @test length(it) == 4
    @test eltype(it) == MOI.VariableIndex
    @test collect(it) == [z, w, x, y]
    @test it[2] == w
    @test it[end] == y
    return
end

function test_indexing_on_VectorAffineFunction()
    f = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [2, 1, 3, 2, 2, 1, 3, 1, 2],
            MOI.ScalarAffineTerm.(
                [1, 7, 2, 9, 3, 1, 6, 4, 1],
                [x, y, z, z, y, z, x, x, y],
            ),
        ),
        [2, 7, 5],
    )
    it = MOI.Utilities.eachscalar(f)
    @test it isa AbstractVector{MOI.ScalarAffineFunction{Int}}
    @test size(it) == (3,)
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
    @test sort(h.terms, by = t -> t.output_index) == MOI.VectorAffineTerm.(
        [1, 1, 2, 2, 2],
        MOI.ScalarAffineTerm.([2, 6, 7, 1, 4], [z, x, y, z, x]),
    )
    @test MOI.Utilities.constant_vector(h) == [5, 2]
    F = MOI.Utilities.operate(vcat, Int, it[[1, 2]], it[3])
    @test F isa MOI.VectorAffineFunction{Int}
    @test sort(F.terms, by = t -> t.output_index) == MOI.VectorAffineTerm.(
        [1, 1, 1, 2, 2, 2, 2, 3, 3],
        MOI.ScalarAffineTerm.(
            [7, 1, 4, 1, 9, 3, 1, 2, 6],
            [y, z, x, x, z, y, y, z, x],
        ),
    )
    @test MOI.Utilities.constant_vector(F) == MOI.Utilities.constant_vector(f)
    return
end

function test_indexing_on_VectorQuadraticFunction()
    f = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [2, 3, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 6, 4, 3], [z, x, x, y], [y, z, z, y]),
        ),
        MOI.VectorAffineTerm.(
            [2, 1, 3, 2, 2],
            MOI.ScalarAffineTerm.([1, 7, 2, 9, 3], [x, y, z, z, y]),
        ),
        [2, 7, 5],
    )
    it = MOI.Utilities.eachscalar(f)
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
    return
end

function test_Scalar_Variable_isone()
    f = MOI.VariableIndex(0)
    g = MOI.VariableIndex(1)
    @test !isone(f)
    @test !isone(g)
    return
end

function test_Scalar_Variable_iszero()
    f = MOI.VariableIndex(0)
    g = MOI.VariableIndex(1)
    @test !iszero(f)
    @test !iszero(g)
    @test f + 1 ≈ 1 + f
    @test (f + 1.0) - 1.0 ≈ (2.0f) / 2.0
    @test (f - 1.0) + 1.0 ≈ (2.0f) / 2.0
    @test (1.0 + f) - 1.0 ≈ (f * 2.0) / 2.0
    @test 1.0 - (1.0 - f) ≈ (f / 2.0) * 2.0
    return
end

function test_Scalar_Variable_complex()
    f = MOI.VariableIndex(0)
    @test real(f) === f
    @test MA.promote_operation(real, typeof(f)) == typeof(f)
    for T in [Int, Int32, Float64]
        z = MOI.Utilities.operate(imag, T, f)
        @test z isa MOI.ScalarAffineFunction{T}
        @test iszero(z)
        @test MOI.Utilities.promote_operation(imag, T, typeof(f)) == typeof(z)
    end
    @test conj(f) === f
    @test MA.promote_operation(conj, typeof(f)) == typeof(f)
    return
end

function test_Scalar_Affine_zero()
    f = @inferred MOI.Utilities.zero(MOI.ScalarAffineFunction{Float64})
    @test iszero(f)
    @test MOI.Utilities.isapprox_zero(f, 1e-16)
    return
end

function test_Scalar_Affine_complex()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    r = 3x + 4y
    c = 2x + 5y
    f = (1 + 0im) * r + c * im
    @test real(f) ≈ r
    @test MOI.Utilities.operate(imag, Int, f) ≈ c
    @test imag(f) ≈ c
    @test conj(f) ≈ (1 + 0im) * r - c * im
    @test MA.promote_operation(real, typeof(f)) == typeof(r)
    @test MA.promote_operation(imag, typeof(f)) == typeof(c)
    @test MA.promote_operation(conj, typeof(f)) == typeof(f)
    return
end

function test_Scalar_Affine_promote_operation()
    @test MOI.Utilities.promote_operation(-, Int, MOI.VariableIndex) ==
          MOI.ScalarAffineFunction{Int}
    @test MOI.Utilities.promote_operation(
        -,
        Int,
        MOI.ScalarAffineFunction{Int},
    ) == MOI.ScalarAffineFunction{Int}
    @test MOI.Utilities.promote_operation(
        +,
        Float64,
        MOI.VariableIndex,
        MOI.VariableIndex,
    ) == MOI.ScalarAffineFunction{Float64}
    @test MOI.Utilities.promote_operation(
        +,
        Float64,
        MOI.ScalarAffineFunction{Float64},
        Float64,
    ) == MOI.ScalarAffineFunction{Float64}
    @test MOI.Utilities.promote_operation(
        +,
        Int,
        MOI.ScalarAffineFunction{Int},
        MOI.ScalarAffineFunction{Int},
    ) == MOI.ScalarAffineFunction{Int}
    return
end

function test_Scalar_Affine_comparison()
    @test MOI.Utilities.operate(+, Float64, x, z) + 1.0 ≈
          MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1, 1e-7, 1], [x, y, z]),
        1.0,
    ) atol = 1e-6
    f1 = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1e-7, y)],
        1.0,
    )
    f2 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 1.0)
    @test f1 ≈ f2 atol = 1e-6
    fdiff = f1 - f2
    @test !iszero(fdiff)
    @test iszero(f1 - f1)
    @test iszero(f2 - f2)
    MOI.Utilities.canonicalize!(fdiff)
    @test !MOI.Utilities.isapprox_zero(fdiff, 1e-8)
    @test MOI.Utilities.isapprox_zero(fdiff, 1e-6)
    return
end

function test_Scalar_Affine_canonical()
    f = MOI.Utilities.canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2, 1, 3, -2, -3], [y, x, z, x, z]),
            5,
        ),
    )
    @test MOI.output_dimension(f) == 1
    @test f.terms == MOI.ScalarAffineTerm.([-1, 2], [x, y])
    @test f.constant == 5
    f = MOI.Utilities.canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1, 3, 1, 2, -3, 2], [w, y, w, x, x, z]),
            2,
        ) + MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-1, -2, -2, 3, 2], [y, z, w, x, y]),
            3,
        ),
    )
    @test f.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
    @test f.constant == 5
end

function test_Scalar_Affine_convert()
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.5], [x, y]), 0.5)
    @test_throws InexactError convert(MOI.VariableIndex, f)
    @test_throws InexactError MOI.Utilities.convert_approx(MOI.VariableIndex, f)
    @test MOI.Utilities.convert_approx(MOI.VariableIndex, f, tol = 0.5) == x
    @test convert(typeof(f), f) === f
    quad_f = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float64}[],
        f.terms,
        f.constant,
    )
    @test convert(MOI.ScalarQuadraticFunction{Float64}, f) ≈ quad_f
    for g in [
        convert(MOI.ScalarAffineFunction{Float64}, x),
        convert(MOI.ScalarAffineFunction{Float64}, 1x),
    ]
        @test g isa MOI.ScalarAffineFunction{Float64}
        @test convert(MOI.VariableIndex, g) == x
        @test MOI.Utilities.convert_approx(MOI.VariableIndex, g) == x
    end
    return
end

function test_Scalar_Affine_operate_with_Float_coefficient()
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 4.0], [x, y]), 5.0)
    @test f ≈ 2.0f / 2.0
    return
end

function test_Scalar_Affine_operate_with_Int_coefficient()
    f = MOI.Utilities.canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1, 3, 1, 2, -3, 2], [w, y, w, x, x, z]),
            2,
        ) + MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-1, -2, -2, 3, 2], [y, z, w, x, y]),
            3,
        ),
    )
    @test f === +f
    @test f ≈
          x + MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 4], [x, y]), 5)
    @test f ≈ f * 1
    @test f ≈
          MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 2], [x, y]), 2) *
          2 + 1
    @test f ≈
          x -
          MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1, -4], [x, y]), -5)
    @test f ≈
          MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3, 4], [x, y]), 5) - x
    return
end

function test_Scalar_Affine_modification()
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 4], [x, y]), 0)
    f = MOI.Utilities.modify_function(f, MOI.ScalarConstantChange(6))
    @test f.constant == 6
    g = deepcopy(f)
    @test g ≈ f
    f = MOI.Utilities.modify_function(f, MOI.ScalarCoefficientChange(y, 3))
    @test !(g ≈ f)
    @test g.terms == MOI.ScalarAffineTerm.([2, 4], [x, y])
    @test f.terms == MOI.ScalarAffineTerm.([2, 3], [x, y])
    f = MOI.Utilities.modify_function(f, MOI.ScalarCoefficientChange(x, 0))
    @test f.terms == MOI.ScalarAffineTerm.([3], [y])
    return
end

function test_Scalar_Quadratic_zero()
    f = @inferred MOI.Utilities.zero(MOI.ScalarQuadraticFunction{Float64})
    @test MOI.Utilities.isapprox_zero(f, 1e-16)
    return
end

function test_Scalar_Quadratic_promote_operation()
    @test MOI.Utilities.promote_operation(
        -,
        Int,
        MOI.ScalarQuadraticFunction{Int},
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        +,
        Int,
        MOI.ScalarQuadraticFunction{Int},
        MOI.ScalarQuadraticFunction{Int},
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        +,
        Int,
        MOI.ScalarQuadraticFunction{Int},
        MOI.ScalarAffineFunction{Int},
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        +,
        Int,
        MOI.ScalarAffineFunction{Int},
        MOI.ScalarQuadraticFunction{Int},
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        *,
        Int,
        MOI.VariableIndex,
        MOI.VariableIndex,
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        *,
        Float64,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{Float64},
    ) == MOI.ScalarQuadraticFunction{Float64}
    @test MOI.Utilities.promote_operation(
        *,
        Int,
        MOI.ScalarAffineFunction{Int},
        MOI.VariableIndex,
    ) == MOI.ScalarQuadraticFunction{Int}
    @test MOI.Utilities.promote_operation(
        *,
        Float64,
        MOI.ScalarAffineFunction{Float64},
        MOI.ScalarAffineFunction{Float64},
    ) == MOI.ScalarQuadraticFunction{Float64}
    @test MOI.Utilities.promote_operation(
        /,
        Float64,
        MOI.ScalarQuadraticFunction{Float64},
        Float64,
    ) == MOI.ScalarQuadraticFunction{Float64}
    return
end

function test_Scalar_Quadratic_Comparison_With_iszero()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    MOI.Utilities.canonicalize!(f)
    @test !iszero(f)
    @test iszero(0 * f)
    @test iszero(f - f)
    return
end

function test_Scalar_Quadratic_Comparison_With_tolerance()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    MOI.Utilities.canonicalize!(f)
    @test !MOI.Utilities.isapprox_zero(f, 1e-8)
    # Test isapprox_zero with zero terms
    @test MOI.Utilities.isapprox_zero(0 * f, 1e-8)
    g = 1.0x * y - (1 + 1e-6) * y * x
    MOI.Utilities.canonicalize!(g)
    @test MOI.Utilities.isapprox_zero(g, 1e-5)
    @test !MOI.Utilities.isapprox_zero(g, 1e-7)
    return
end

function test_Scalar_Quadratic_convert()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    MOI.Utilities.canonicalize!(f)
    @test_throws InexactError convert(MOI.VariableIndex, f)
    @test_throws InexactError convert(MOI.ScalarAffineFunction{Int}, f)
    g = convert(MOI.ScalarQuadraticFunction{Float64}, x)
    @test convert(MOI.VariableIndex, g) == x
    return
end

function test_Scalar_Quadratic_Power_Affine()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    aff = 1x + 2 + y
    @test isone(@inferred aff^0)
    @test convert(typeof(f), aff) ≈ @inferred aff^1
    @test aff * aff ≈ @inferred aff^2
    err = ArgumentError("Cannot take $(typeof(aff)) to the power 3.")
    @test_throws err aff^3
    return
end

function test_Scalar_Quadratic_Power_Quadratic()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    @test isone(@inferred f^0)
    @test f ≈ @inferred f^1
    err = ArgumentError("Cannot take $(typeof(f)) to the power 2.")
    @test_throws err f^2
    return
end

function test_Scalar_Quadratic_no_zero_affine_term()
    # Test that no affine 0y term is created when multiplying 1x by y
    for xy in [1x * y, x * 1y]
        @test isempty(xy.affine_terms)
        @test length(xy.quadratic_terms) == 1
        @test xy.quadratic_terms[1] == MOI.ScalarQuadraticTerm(1, x, y) ||
              xy.quadratic_terms[1] == MOI.ScalarQuadraticTerm(1, y, x)
    end
    for xx in [1x * x, x * 1x]
        @test isempty(xx.affine_terms)
        @test length(xx.quadratic_terms) == 1
        @test xx.quadratic_terms[1] == MOI.ScalarQuadraticTerm(2, x, x)
    end
    return
end

function test_Scalar_Quadratic_operate!()
    q = 1.0x + 1.0y + (1.0x) * z + (1.0w) * z
    @test q ≈ 1.0x + 1.0y + (1.0w) * z + (1.0x) * z
    # This calls
    aff = 1.0x + 1.0y
    # which tries to mutate `aff`, gets a quadratic expression
    # and mutate it with the remaining term
    @test MOI.Utilities.operate!(+, Float64, aff, (1.0x) * z, (1.0w) * z) ≈ q
    return
end

function test_Scalar_Quadratic_operate()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    @test f ≈ 7 + (x + 2y) * (1x + y) + 3x
    @test f ≈ -(-7 - 3x) + (x + 2y) * (1x + y)
    @test f ≈ -((x + 2y) * (MOI.Utilities.operate(-, Int, x) - y)) + 3x + 7
    @test f ≈ 7 + MOI.Utilities.operate(*, Int, x, x) + 3x * (y + 1) + 2y * y
    @test f ≈ (x + 2) * (x + 1) + (y + 1) * (2y + 3x) + (5 - 3x - 2y)
    @test f ≈ begin
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2], [x], [x]),
            [MOI.ScalarAffineTerm(3, x)],
            4,
        ) + MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([4, 3], [y, x], [y, y]),
            MOI.ScalarAffineTerm{Int}[],
            3,
        )
    end
    @test f ≈ begin
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2], [x], [x]),
            [MOI.ScalarAffineTerm(3, x)],
            10,
        ) - MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([-4, -3], [y, x], [y, y]),
            MOI.ScalarAffineTerm{Int}[],
            3,
        )
    end
    @test f ≈ begin
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) +
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]),
            MOI.ScalarAffineTerm{Int}[],
            2,
        )
    end
    @test f ≈ begin
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5) -
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([-2, -4, -3], [x, y, x], [x, y, y]),
            MOI.ScalarAffineTerm{Int}[],
            -2,
        )
    end
    @test f ≈ begin
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]),
            MOI.ScalarAffineTerm{Int}[],
            2,
        ) + MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 5)
    end
    @test f ≈ begin
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]),
            MOI.ScalarAffineTerm{Int}[],
            12,
        ) - MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-3, x)], 5)
    end
    @test f ≈ begin
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]),
            MOI.ScalarAffineTerm.([2], [x]),
            7,
        ) + x
    end
    @test f ≈
          MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.([2, 4, 3], [x, y, x], [x, y, y]),
        MOI.ScalarAffineTerm.([3], [x]),
        10,
    ) - 3
    @test f ≈
          2.0 * MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.([2.0, 4.0, 3.0], [x, y, x], [x, y, y]),
        MOI.ScalarAffineTerm.([3.0], [x]),
        7.0,
    ) / 2.0
    return
end

function test_Scalar_Quadratic_modification()
    f = 7 + 3x + 1x * x + 2y * y + 3x * y
    f = MOI.Utilities.modify_function(f, MOI.ScalarConstantChange(9))
    @test f.constant == 9
    f = MOI.Utilities.modify_function(f, MOI.ScalarCoefficientChange(y, 0))
    @test f.affine_terms == MOI.ScalarAffineTerm.([3], [x])
    g = deepcopy(f)
    @test f ≈ g
    f = MOI.Utilities.modify_function(f, MOI.ScalarCoefficientChange(y, 2))
    @test !(f ≈ g)
    @test g.affine_terms == MOI.ScalarAffineTerm.([3], [x])
    @test f.affine_terms == MOI.ScalarAffineTerm.([3, 2], [x, y])
    return
end

function test_Vector_Variable()
    f = MOI.VectorOfVariables([MOI.VariableIndex(1), MOI.VariableIndex(2)])
    @test real(f) === f
    @test MA.promote_operation(real, typeof(f)) == typeof(f)
    for T in [Int, Int32, Float64]
        z = MOI.Utilities.operate(imag, T, f)
        @test z isa MOI.VectorAffineFunction{T}
        @test isempty(z.terms)
        @test all(iszero, z.constants)
        @test MOI.output_dimension(z) == MOI.output_dimension(f)
        @test MOI.Utilities.promote_operation(imag, T, typeof(f)) == typeof(z)
    end
    @test conj(f) === f
    @test MA.promote_operation(conj, typeof(f)) == typeof(f)
end

function test_Vector_Affine_complex()
    x = MOI.VectorOfVariables([MOI.VariableIndex(1)])
    y = MOI.VectorOfVariables([MOI.VariableIndex(2)])
    r = 3x + 4y
    c = 2x + 5y
    f = (1 + 0im) * r + c * im
    @test real(f) ≈ r
    @test MOI.Utilities.operate(imag, Int, f) ≈ c
    @test imag(f) ≈ c
    @test conj(f) ≈ (1 + 0im) * r - c * im
    @test MA.promote_operation(real, typeof(f)) == typeof(r)
    @test MA.promote_operation(imag, typeof(f)) == typeof(c)
    @test MA.promote_operation(conj, typeof(f)) == typeof(f)
    return
end

function test_Vector_Affine()
    f = MOI.Utilities.canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2],
                MOI.ScalarAffineTerm.(
                    [3, 2, 3, -3, -1, -2, 3, -2, 1, 3, 5, -2, 0, -1],
                    [x, x, z, y, y, x, y, z, x, y, y, x, x, z],
                ),
            ),
            [5, 7],
        ),
    )
    @test MOI.output_dimension(f) == 2
    @test f.terms == MOI.VectorAffineTerm.(
        [1, 1, 2],
        MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]),
    )
    @test MOI.Utilities.constant_vector(f) == [5, 7]
    f = MOI.Utilities.modify_function(f, MOI.VectorConstantChange([6, 8]))
    @test MOI.Utilities.constant_vector(f) == [6, 8]
    g = deepcopy(f)
    @test f ≈ g
    f = MOI.Utilities.modify_function(f, MOI.MultirowChange(y, [(2, 9)]))
    @test !(f ≈ g)
    @test f.terms == MOI.VectorAffineTerm.(
        [1, 1, 2],
        MOI.ScalarAffineTerm.([2, 4, 9], [x, y, y]),
    )
    @test g.terms == MOI.VectorAffineTerm.(
        [1, 1, 2],
        MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]),
    )
    f = MOI.Utilities.modify_function(f, MOI.MultirowChange(y, [(1, 0)]))
    @test f.terms ==
          MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([2, 9], [x, y]))
    return
end

function test_Vector_Quadratic()
    f = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 2],
            MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y]),
        ),
        [7, 3, 4],
    )
    @test MOI.output_dimension(f) == 3
    f = MOI.Utilities.modify_function(f, MOI.VectorConstantChange([10, 11, 12]))
    @test MOI.Utilities.constant_vector(f) == [10, 11, 12]
    f = MOI.Utilities.modify_function(
        f,
        MOI.MultirowChange(y, [(2, 0), (1, 1)]),
    )
    @test f.affine_terms == MOI.VectorAffineTerm.(
        [1, 2, 1],
        MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]),
    )
    g = deepcopy(f)
    f = MOI.Utilities.modify_function(
        f,
        MOI.MultirowChange(x, [(1, 0), (3, 4)]),
    )
    @test f.affine_terms == MOI.VectorAffineTerm.(
        [2, 1, 3],
        MOI.ScalarAffineTerm.([1, 1, 4], [x, y, x]),
    )
    @test g.affine_terms == MOI.VectorAffineTerm.(
        [1, 2, 1],
        MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]),
    )
    return
end

function _isapprox_ordered(
    f1::T,
    f2::T,
) where {T<:Union{MOI.ScalarAffineFunction,MOI.VectorAffineFunction}}
    return (
        (MOI.term_indices.(f1.terms) == MOI.term_indices.(f2.terms)) &&
        (MOI.constant(f1) ≈ MOI.constant(f2)) &&
        (MOI.coefficient.(f1.terms) ≈ MOI.coefficient.(f2.terms))
    )
end

function _isapprox_ordered(
    f1::T,
    f2::T,
) where {T<:Union{MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction}}
    return (
        (
            MOI.term_indices.(f1.affine_terms) ==
            MOI.term_indices.(f2.affine_terms)
        ) &&
        (
            MOI.term_indices.(f1.quadratic_terms) ==
            MOI.term_indices.(f2.quadratic_terms)
        ) &&
        (MOI.constant(f1) ≈ MOI.constant(f2)) &&
        (
            MOI.coefficient.(f1.affine_terms) ≈
            MOI.coefficient.(f2.affine_terms)
        ) &&
        (
            MOI.coefficient.(f1.quadratic_terms) ≈
            MOI.coefficient.(f2.quadratic_terms)
        )
    )
end

function _test_canonicalization(
    f::T,
    expected::T,
) where {
    T<:Union{
        MOI.ScalarAffineFunction,
        MOI.VectorAffineFunction,
        MOI.ScalarQuadraticFunction,
        MOI.VectorQuadraticFunction,
    },
}
    @test MOI.Utilities.is_canonical(expected)
    g = @inferred(MOI.Utilities.canonical(f))
    @test _isapprox_ordered(g, expected)
    @test MOI.Utilities.is_canonical(g)
    @test MOI.Utilities.is_canonical(expected)
    @test _isapprox_ordered(MOI.Utilities.canonical(g), g)
    @test MOI.Utilities.canonical(g) !== g
    # There are some changes to sorting in Julia v1.9 that now mean sorting
    # allocates.
    if VERSION < v"1.9.0-DEV"
        @test @allocated(MOI.Utilities.canonicalize!(f)) == 0
    end
    return
end

function test_canonical_ScalarAffine()
    @test MOI.Utilities.is_canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            1.0,
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1])),
            1.0,
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([2, 1])),
            2.0,
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 0.0], MOI.VariableIndex.([1, 2])),
            2.0,
        ),
    )

    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]),
            1.5,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]),
            1.5,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.0], [MOI.VariableIndex(1)]),
            -2.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]),
            -2.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.0], [MOI.VariableIndex(2)]),
            -2.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex[]),
            -2.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0], [MOI.VariableIndex(2)]),
            1.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0], [MOI.VariableIndex(2)]),
            1.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0, 4.0], MOI.VariableIndex.([2, 1])),
            0.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([4.0, 3.0], MOI.VariableIndex.([1, 2])),
            0.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 0.1], MOI.VariableIndex.([1, 1])),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.1], MOI.VariableIndex.([1])),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [1.0, 0.5, 2.0],
                MOI.VariableIndex.([1, 3, 1]),
            ),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0, 0.5], MOI.VariableIndex.([1, 3])),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [1.0, 0.5, 2.0],
                MOI.VariableIndex.([1, 3, 1]),
            ),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0, 0.5], MOI.VariableIndex.([1, 3])),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [1.0, 2.0, 3.0, 4.0, 5.0],
                MOI.VariableIndex.([1, 3, 1, 3, 2]),
            ),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [4.0, 5.0, 6.0],
                MOI.VariableIndex.([1, 2, 3]),
            ),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, -1.0], MOI.VariableIndex.([7, 7])),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [1.0, -1.0, 0.5],
                MOI.VariableIndex.([7, 7, 2]),
            ),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.5], MOI.VariableIndex.([2])),
            5.0,
        ),
    )
    _test_canonicalization(
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [1.0, -0.5, 0.5, -0.5],
                MOI.VariableIndex.([7, 7, 2, 7]),
            ),
            5.0,
        ),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([0.5], MOI.VariableIndex.([2])),
            5.0,
        ),
    )
    return
end

function test_canonical_VectorAffine()
    @test MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                Int[],
                MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            ),
            Float64[],
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1],
                MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([1])),
            ),
            [1.0],
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2],
                MOI.ScalarAffineTerm.([0.0, 1.0], MOI.VariableIndex.([1, 2])),
            ),
            [1.0],
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 1],
                MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 2])),
            ),
            [1.0],
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1],
                MOI.ScalarAffineTerm.([1.0, -1.0], MOI.VariableIndex.([1, 1])),
            ),
            [1.0],
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1],
                MOI.ScalarAffineTerm.([1.0, 1.0], MOI.VariableIndex.([1, 1])),
            ),
            [1.0],
        ),
    )

    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                Int[],
                MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            ),
            Float64[],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                Int[],
                MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            ),
            Float64[],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3],
                MOI.ScalarAffineTerm.([0.0], MOI.VariableIndex.([5])),
            ),
            [1.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                Int[],
                MOI.ScalarAffineTerm.(Float64[], MOI.VariableIndex.(Int[])),
            ),
            [1.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3, 2],
                MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([5, 6])),
            ),
            [1.0, 2.0, 3.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 3],
                MOI.ScalarAffineTerm.([2.0, 1.0], MOI.VariableIndex.([6, 5])),
            ),
            [1.0, 2.0, 3.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3, 3],
                MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([6, 5])),
            ),
            [1.0, 2.0, 3.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3, 3],
                MOI.ScalarAffineTerm.([2.0, 1.0], MOI.VariableIndex.([5, 6])),
            ),
            [1.0, 2.0, 3.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3, 3],
                MOI.ScalarAffineTerm.([1.0, 2.0], MOI.VariableIndex.([1, 1])),
            ),
            [1.0, 2.0, 3.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [3],
                MOI.ScalarAffineTerm.([3.0], MOI.VariableIndex.([1])),
            ),
            [1.0, 2.0, 3.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 1],
                MOI.ScalarAffineTerm.(
                    [1.0, 2.0, 3.0],
                    MOI.VariableIndex.([1, 2, 1]),
                ),
            ),
            [4.0, 5.0, 6.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1],
                MOI.ScalarAffineTerm.([4.0, 2.0], MOI.VariableIndex.([1, 2])),
            ),
            [4.0, 5.0, 6.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 1],
                MOI.ScalarAffineTerm.(
                    [1.0, 2.0, 3.0],
                    MOI.VariableIndex.([2, 1, 1]),
                ),
            ),
            [4.0, 5.0, 6.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1],
                MOI.ScalarAffineTerm.([5.0, 1.0], MOI.VariableIndex.([1, 2])),
            ),
            [4.0, 5.0, 6.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 3, 3],
                MOI.ScalarAffineTerm.(
                    [1.0, 2.0, 3.0],
                    MOI.VariableIndex.([1, 1, 1]),
                ),
            ),
            [4.0, 5.0, 6.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 3],
                MOI.ScalarAffineTerm.([1.0, 5.0], MOI.VariableIndex.([1, 1])),
            ),
            [4.0, 5.0, 6.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 3, 3],
                MOI.ScalarAffineTerm.(
                    [1.0, -3.0, 3.0],
                    MOI.VariableIndex.([1, 1, 1]),
                ),
            ),
            [4.0, 5.0, 6.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2],
                MOI.ScalarAffineTerm.([1.0], MOI.VariableIndex.([1])),
            ),
            [4.0, 5.0, 6.0],
        ),
    )
    _test_canonicalization(
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2, 3, 3, 3],
                MOI.ScalarAffineTerm.(
                    [1.0, 3.0, -1.0, -2.0],
                    MOI.VariableIndex.([1, 1, 1, 1]),
                ),
            ),
            [4.0, 5.0, 6.0],
        ),
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [2],
                MOI.ScalarAffineTerm.([1.0], MOI.VariableIndex.([1])),
            ),
            [4.0, 5.0, 6.0],
        ),
    )
    return
end

function test_canonical_ScalarQuadratic()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test MOI.Utilities.is_canonical(
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.0),
    )
    @test !MOI.Utilities.is_canonical(1.0x * y + 2.0x * x + 2.0)
    @test !MOI.Utilities.is_canonical(1.0x * x + 0.0x * y + 2.0)

    _test_canonicalization(
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.5),
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.5),
    )
    _test_canonicalization(
        0.0x * y - 2.0,
        convert(MOI.ScalarQuadraticFunction{Float64}, -2.0),
    )
    _test_canonicalization(3.0x * y + 4.0x * x + 0.0, 4.0x * x + 3.0x * y + 0.0)
    _test_canonicalization(1.0x * y + 0.1x * y + 5.0, 1.1x * y + 5.0)
    return
end

function test_canonical_VectorQuadratic()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test MOI.Utilities.is_canonical(
        MOI.Utilities.operate(
            vcat,
            Float64,
            convert(MOI.ScalarQuadraticFunction{Float64}, 1.0),
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.Utilities.operate(vcat, Float64, 1.0x * y + 2.0x * x + 2.0),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.Utilities.operate(vcat, Float64, 1.0x * x + 0.0x * y + 2.0),
    )

    _test_canonicalization(
        MOI.Utilities.operate(
            vcat,
            Float64,
            1.5,
            0.0x * y - 2.0,
            3.0x * y + 4.0x * x + 0.0,
            1.0x * y + 0.1x * y + 5.0,
        ),
        MOI.Utilities.operate(
            vcat,
            Float64,
            1.5,
            -2.0,
            4.0x * x + 3.0x * y + 0.0,
            1.1x * y + 5.0,
        ),
    )
    return
end

function test_vector_terms()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    at = MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(10.0, x))
    qt = MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(6.0, x, y))
    @test MOI.Utilities.operate_term(*, 3.0, at) ==
          MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(30.0, x))
    @test MOI.Utilities.operate_term(*, at, at) ==
          MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(200.0, x, x))
    @test MOI.Utilities.operate_term(*, 3.0, qt) ==
          MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(18.0, x, y))
    @test MOI.Utilities.operate_term(/, at, 2.0) ==
          MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(5.0, x))
    @test MOI.Utilities.operate_term(/, qt, 3.0) ==
          MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, y))
end

function test_vector_promote()
    T = Float64
    for t1 in [MOI.VectorAffineFunction{T}, MOI.VectorOfVariables, Vector{T}]
        for t2 in [MOI.VectorAffineFunction{T}, MOI.VectorOfVariables]
            @test MOI.Utilities.promote_operation(+, T, t1, t2) ==
                  MOI.VectorAffineFunction{T}
            @test MOI.Utilities.promote_operation(-, T, t1, t2) ==
                  MOI.VectorAffineFunction{T}
        end
    end
    for t1 in [
        MOI.VectorQuadraticFunction{T},
        MOI.VectorAffineFunction{T},
        MOI.VectorOfVariables,
        Vector{T},
    ]
        for t2 in [MOI.VectorQuadraticFunction{T}]
            @test MOI.Utilities.promote_operation(+, T, t1, t2) ==
                  MOI.VectorQuadraticFunction{T}
            @test MOI.Utilities.promote_operation(-, T, t1, t2) ==
                  MOI.VectorQuadraticFunction{T}
        end
    end
    for t in [MOI.VectorOfVariables, MOI.VectorAffineFunction{T}]
        @test MOI.Utilities.promote_operation(-, T, t) ==
              MOI.VectorAffineFunction{T}
    end
    t = MOI.VectorQuadraticFunction{T}
    @test MOI.Utilities.promote_operation(-, T, t) ==
          MOI.VectorQuadraticFunction{T}
end

function test_vector_plus_and_neg()
    w = MOI.VariableIndex(0)
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    α = [1, 2, 3]
    v = MOI.VectorOfVariables([y, w, y])
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
        [3, 1, 4],
    )
    f = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 2],
            MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y]),
        ),
        [7, 3, 4],
    )
    v_plus_g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [3, 1, 1, 2, 3],
            MOI.ScalarAffineTerm.([5, 2, 1, 1, 1], [y, x, y, w, y]),
        ),
        [3, 1, 4],
    )
    g_plus_α = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.([3, 1], MOI.ScalarAffineTerm.([5, 2], [y, x])),
        [4, 3, 7],
    )
    α_minus_v = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 2, 3],
            MOI.ScalarAffineTerm.([-1, -1, -1], [y, w, y]),
        ),
        [1, 2, 3],
    )
    v_minus_v_plus_v = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 2, 3, 1, 2, 3, 1, 2, 3],
            MOI.ScalarAffineTerm.(
                [1, 1, 1, -1, -1, -1, 1, 1, 1],
                [y, w, y, y, w, y, y, w, y],
            ),
        ),
        [0, 0, 0],
    )
    f_plus_α = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 2],
            MOI.ScalarAffineTerm.([3, 1, 2], [x, x, y]),
        ),
        [8, 5, 7],
    )
    f_minus_g = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 2, 3, 1],
            MOI.ScalarAffineTerm.([3, 1, 2, -5, -2], [x, x, y, y, x]),
        ),
        [4, 2, 0],
    )
    @test v + g ≈ v_plus_g
    @test g + α ≈ g_plus_α
    @test α + g ≈ g_plus_α
    @test α - v ≈ α_minus_v
    @test MOI.Utilities.operate(
        +,
        Int,
        MOI.Utilities.operate(-, Int, v, v),
        v,
    ) ≈ v_minus_v_plus_v
    @test f + α ≈ f_plus_α
    @test f - g ≈ f_minus_g
    @test f - f + f - g ≈ f_minus_g
    @test v + f + α - v ≈ f_plus_α
    @test v - f - α - v ≈ -f_plus_α
    @test MOI.Utilities.operate!(-, Int, v, f) - v ≈ -f
    @test (g + v + g + v + f) - (v + g + v + g) ≈ f
    @test v - α ≈ -α_minus_v
    @test g - f ≈ -f_minus_g
    return
end

function test_vector_mult_and_div()
    w = MOI.VariableIndex(0)
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    v = MOI.VectorOfVariables([y, w, y])
    v2 = MOI.Utilities.operate(vcat, Float64, 2.0y, 2.0w, 2.0y)
    f = MOI.Utilities.operate(
        vcat,
        Float64,
        2.0x + 3.0y + 1.0,
        3.0x + 2.0y + 3.0,
    )
    f2 = MOI.Utilities.operate(
        vcat,
        Float64,
        4.0x + 6.0y + 2.0,
        6.0x + 4.0y + 6.0,
    )
    g = MOI.Utilities.operate(
        vcat,
        Float64,
        7.0x * y + 2.0x + 3.0y + 1.0,
        6.0x * x + 5.0y * y + 3.0x + 2.0y + 3.0,
    )
    g2 = MOI.Utilities.operate(
        vcat,
        Float64,
        14.0x * y + 4.0x + 6.0y + 2.0,
        12.0x * x + 10.0y * y + 6.0x + 4.0y + 6.0,
    )
    for (a, a2) in [(v, v2), (f, f2), (g, g2)]
        @test a * 2.0 ≈ a2
        @test 2.0 * a ≈ a2
        @test a / 0.5 ≈ a2
    end
    return
end

function test_modifycoefficient_duplicates()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, x)],
        0.0,
    )
    new_f =
        MOI.Utilities.modify_function(f, MOI.ScalarCoefficientChange(x, 3.0))
    @test new_f ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3.0, x)], 0.0)
    return
end

function test_modifycoefficients_duplicates()
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
        f,
        MOI.MultirowChange(x, [(1, 3.0), (2, 0.5)]),
    )
    @test new_f ≈ MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(3.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.5, x)),
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        ],
        [0.0, 0.0, 0.0],
    )
    return
end

function test_zero_with_output_dimension()
    f = MOI.Utilities.zero_with_output_dimension(Vector{Int}, 2)
    @test f == [0, 0]
    @test f isa Vector{Int}

    g = MOI.Utilities.zero_with_output_dimension(Vector{Float64}, 2)
    @test g == [0.0, 0.0]
    @test g isa Vector{Float64}

    h = MOI.Utilities.zero_with_output_dimension(
        MOI.VectorAffineFunction{Int},
        2,
    )
    @test MOI.output_dimension(h) == 2
    @test isapprox(
        h,
        MOI.VectorAffineFunction{Int}(MOI.VectorAffineTerm{Int}[], [0, 0]),
    )
    @test h isa MOI.VectorAffineFunction{Int}

    i = MOI.Utilities.zero_with_output_dimension(
        MOI.VectorQuadraticFunction{Int},
        2,
    )
    @test MOI.output_dimension(i) == 2
    @test isapprox(
        i,
        MOI.VectorQuadraticFunction{Int}(
            MOI.VectorQuadraticTerm{Int}[],
            MOI.VectorAffineTerm{Int}[],
            [0, 0],
        ),
    )
    @test i isa MOI.VectorQuadraticFunction{Int}
    return
end

"""
    test_SingleVariable_operators()

Test the three Base.:(op) fallbacks for pure SingleVariable operations.
"""
function test_SingleVariable_operators()
    x = MOI.VariableIndex(1)
    @test_throws ErrorException x + x
    @test_throws ErrorException x - x
    @test_throws ErrorException x * x
    return
end

function test_value_type()
    T = Float64
    @test MOI.Utilities.value_type(T, MOI.VariableIndex) == T
    @test MOI.Utilities.value_type(T, MOI.VectorOfVariables) == Vector{T}
    @test MOI.Utilities.value_type(T, MOI.ScalarAffineFunction{Int}) == T
    @test MOI.Utilities.value_type(T, MOI.ScalarAffineFunction{T}) == T
    @test MOI.Utilities.value_type(T, MOI.ScalarAffineFunction{Complex{Int}}) ==
          Complex{T}
    @test MOI.Utilities.value_type(T, MOI.ScalarAffineFunction{Complex{T}}) ==
          Complex{T}
    @test MOI.Utilities.value_type(T, MOI.ScalarQuadraticFunction{Int}) == T
    @test MOI.Utilities.value_type(T, MOI.ScalarQuadraticFunction{T}) == T
    @test MOI.Utilities.value_type(
        T,
        MOI.ScalarQuadraticFunction{Complex{Int}},
    ) == Complex{T}
    @test MOI.Utilities.value_type(T, MOI.ScalarNonlinearFunction) == T
    @test MOI.Utilities.value_type(
        T,
        MOI.ScalarQuadraticFunction{Complex{T}},
    ) == Complex{T}
    @test MOI.Utilities.value_type(T, MOI.VectorAffineFunction{Int}) ==
          Vector{T}
    @test MOI.Utilities.value_type(T, MOI.VectorAffineFunction{T}) == Vector{T}
    @test MOI.Utilities.value_type(T, MOI.VectorAffineFunction{Complex{Int}}) ==
          Vector{Complex{T}}
    @test MOI.Utilities.value_type(T, MOI.VectorAffineFunction{Complex{T}}) ==
          Vector{Complex{T}}
    @test MOI.Utilities.value_type(T, MOI.VectorQuadraticFunction{Int}) ==
          Vector{T}
    @test MOI.Utilities.value_type(T, MOI.VectorQuadraticFunction{T}) ==
          Vector{T}
    @test MOI.Utilities.value_type(
        T,
        MOI.VectorQuadraticFunction{Complex{Int}},
    ) == Vector{Complex{T}}
    @test MOI.Utilities.value_type(
        T,
        MOI.VectorQuadraticFunction{Complex{T}},
    ) == Vector{Complex{T}}
    return
end

function test_filter_variables_scalarnonlinearfunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.ScalarNonlinearFunction(:+, Any[x, 2, y])
    new_f = MOI.Utilities.filter_variables(xi -> xi != x, f)
    @test new_f ≈ MOI.ScalarNonlinearFunction(:+, Any[2, y])
    f = MOI.ScalarNonlinearFunction(:-, Any[x, 2, y])
    new_f = MOI.Utilities.filter_variables(xi -> xi != x, f)
    @test new_f ≈ MOI.ScalarNonlinearFunction(:-, Any[0, 2, y])
    f = MOI.ScalarNonlinearFunction(:-, Any[2, x, y])
    new_f = MOI.Utilities.filter_variables(xi -> xi != x, f)
    @test new_f ≈ MOI.ScalarNonlinearFunction(:-, Any[2, y])
    f = MOI.ScalarNonlinearFunction(:-, Any[2, x])
    new_f = MOI.Utilities.filter_variables(xi -> xi != x, f)
    @test new_f ≈ 2
    f = MOI.ScalarNonlinearFunction(:+, Any[x, 2, y])
    f2 = MOI.ScalarNonlinearFunction(:+, Any[2, y])
    g = MOI.ScalarNonlinearFunction(:-, Any[f, x, 2])
    new_g = MOI.Utilities.filter_variables(xi -> xi != x, g)
    @test new_g ≈ MOI.ScalarNonlinearFunction(:-, Any[f2, 2])
    return
end

function test_filter_variables_vectornonlinearfunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.ScalarNonlinearFunction(:+, Any[x, 2, y])
    g = MOI.ScalarNonlinearFunction(:-, Any[x, 2, y])
    fg = MOI.VectorNonlinearFunction([f, g])
    new_f = MOI.ScalarNonlinearFunction(:+, Any[2, y])
    new_g = MOI.ScalarNonlinearFunction(:-, Any[0, 2, y])
    new_fg = MOI.VectorNonlinearFunction([new_f, new_g])
    @test new_fg ≈ MOI.Utilities.filter_variables(xi -> xi != x, fg)
    return
end

function test_ScalarNonlinearFunction_count_map_indices_and_print()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Bool}(),
        MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Bool}()),
    )
    MOI.Utilities.attach_optimizer(model)
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:!, Any[x])
    g = MOI.ScalarNonlinearFunction(:count, Any[Any[x, f]])
    @test sprint(io -> show(io, MIME("text/plain"), g)) ==
          "count([MOI.VariableIndex(1), !(MOI.VariableIndex(1))])"
    c = MOI.add_constraint(model, g, MOI.EqualTo(true))
    @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ g
    return
end

function test_ScalarNonlinearFunction_map_indices()
    src = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(src)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    c = MOI.add_constraint(src, f, MOI.LessThan(1.0))
    dest = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    index_map = MOI.copy_to(dest, src)
    new_f = MOI.Utilities.map_indices(index_map, f)
    @test new_f ≈ MOI.ScalarNonlinearFunction(:log, Any[index_map[x]])
    return
end

function test_ScalarNonlinearFunction_map_indices_nested()
    src = MOI.Utilities.Model{Float64}()
    x = MOI.add_variables(src, 5)
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[
            x[1],
            MOI.ScalarNonlinearFunction(
                :+,
                Any[MOI.ScalarNonlinearFunction(:+, Any[x[2], x[3]]), x[4]],
            ),
            x[5],
        ],
    )
    c = MOI.add_constraint(src, f, MOI.LessThan(1.0))
    dest = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    index_map = MOI.copy_to(dest, src)
    new_f = MOI.Utilities.map_indices(index_map, f)
    y = [index_map[xi] for xi in x]
    @test new_f ≈ MOI.ScalarNonlinearFunction(
        :+,
        Any[
            y[1],
            MOI.ScalarNonlinearFunction(
                :+,
                Any[MOI.ScalarNonlinearFunction(:+, Any[y[2], y[3]]), y[4]],
            ),
            y[5],
        ],
    )
    return
end

function test_ScalarNonlinearFunction_map_indices_deep_recursion()
    src = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(src)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    for _ in 1:50_000
        f = MOI.ScalarNonlinearFunction(:log, Any[f])
    end
    c = MOI.add_constraint(src, f, MOI.LessThan(1.0))
    dest = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    index_map = MOI.copy_to(dest, src)
    new_f = MOI.Utilities.map_indices(index_map, f)
    g = new_f
    for _ in 1:50_000
        g = g.args[1]
    end
    @test g ≈ MOI.ScalarNonlinearFunction(:log, Any[index_map[x]])
    return
end

function test_ScalarNonlinearFunction_substitute_variables()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:log, Any[1.0*x])
    new_f = MOI.Utilities.substitute_variables(x -> -2.0 * x, f)
    @test new_f ≈ MOI.ScalarNonlinearFunction(:log, Any[-2.0*x])
    return
end

function test_ScalarNonlinearFunction_is_canonical()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:log, Any[1.0*x])
    @test MOI.Utilities.is_canonical(f)
    g = MOI.ScalarNonlinearFunction(:log, Any[1.0*x+1.0*x])
    @test !MOI.Utilities.is_canonical(g)
    MOI.Utilities.canonicalize!(g)
    @test MOI.Utilities.is_canonical(g)
    @test g.args[1] ≈ 2.0 * x
    f = MOI.ScalarNonlinearFunction(:^, Any[x, 2])
    @test MOI.Utilities.is_canonical(f)
    # Test deep recursion
    for _ in 1:100_000
        f = MOI.ScalarNonlinearFunction(:sin, Any[f])
    end
    @test MOI.Utilities.is_canonical(f)
    return
end

function test_vector_type()
    for T in (Int, Float64)
        @test MOI.Utilities.vector_type(T) == Vector{T}
        @test MOI.Utilities.vector_type(MOI.VariableIndex) ==
              MOI.VectorOfVariables
        @test MOI.Utilities.vector_type(MOI.ScalarAffineFunction{T}) ==
              MOI.VectorAffineFunction{T}
        @test MOI.Utilities.vector_type(MOI.ScalarQuadraticFunction{T}) ==
              MOI.VectorQuadraticFunction{T}
    end
    return
end

function _test_base_op_scalar(op)
    x = MOI.VariableIndex(1)
    F = (
        1,
        x,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2, x)], 3),
        MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(2, x, x)],
            [MOI.ScalarAffineTerm(2, x)],
            3,
        ),
    )
    for f in F, g in F
        if f == g == x
            @test_throws ErrorException op(f, g)
            continue
        end
        @test op(f, g) ≈ MOI.Utilities.operate(op, Int, f, g)
        if op === +
            for h in F
                @test op(f, g, h) ≈ MOI.Utilities.operate(op, Int, f, g, h)
            end
        end
    end
    return
end

function _test_base_op_vector(op)
    x = MOI.VariableIndex(1)
    F = (
        [1],
        MOI.VectorOfVariables([x]),
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2, x))],
            [3],
        ),
        MOI.VectorQuadraticFunction(
            [MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2, x, x))],
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2, x))],
            [3],
        ),
    )
    for f in F, g in F
        if f == g == MOI.VectorOfVariables([x])
            @test_throws ErrorException op(f, g)
            continue
        end
        @test op(f, g) ≈ MOI.Utilities.operate(op, Int, f, g)
        if op === +
            for h in F
                @test op(f, g, h) ≈ MOI.Utilities.operate(op, Int, f, g, h)
            end
        end
    end
    return
end

test_base_plus_scalar() = _test_base_op_scalar(+)

test_base_minus_scalar() = _test_base_op_scalar(-)

test_base_plus_vector() = _test_base_op_vector(+)

test_base_minus_vector() = _test_base_op_vector(-)

function test_base_divide()
    x = MOI.VariableIndex(1)
    F = (
        x,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 3.0),
        MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(2.0, x, x)],
            [MOI.ScalarAffineTerm(2.0, x)],
            3.0,
        ),
        MOI.VectorOfVariables([x]),
        MOI.VectorAffineFunction(
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
            [3.0],
        ),
        MOI.VectorQuadraticFunction(
            [MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2.0, x, x))],
            [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
            [3.0],
        ),
    )
    for f in F
        @test f / 2.0 ≈ MOI.Utilities.operate(/, Float64, f, 2.0)
    end
    return
end

function test_deprecated_constant_vector()
    x = MOI.VariableIndex(1)
    f, g = 1.0 * x + 2.0, 1.0 * x * x + 2.0
    @test MOI.Utilities.constant_vector(f) == [2.0]
    @test MOI.Utilities.constant_vector(g) == [2.0]
    @test MOI.Utilities.constant_vector(MOI.Utilities.vectorize([f])) == [2.0]
    @test MOI.Utilities.constant_vector(MOI.Utilities.vectorize([g])) == [2.0]
    return
end

function test_deprecated_eval_term()
    x = MOI.VariableIndex(1)
    term = MOI.ScalarAffineTerm(2.0, x)
    @test MOI.Utilities.eval_term(x -> 1.4, term) ≈ 2.0 * 1.4
    return
end

function test_VectorAffineFunction_row_constructor()
    x = MOI.VariableIndex(1)
    f = MOI.VectorAffineFunction([1.0 * x + 2.0, 3.0 * x + 4.0])
    g = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm{Float64}[
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(3.0, x)),
        ],
        [2.0, 4.0],
    )
    @test f ≈ g
    return
end

function test_VectorQuadraticFunction_row_constructor()
    x, y = MOI.VariableIndex.(1:2)
    row1 = 1.0 * x * x + 1.0 * y * y + 2.0 * x + 3.0 * y + 4.2
    row2 = 3.0 * x * x + 3.0 * y * y + 1.0 * x + 3.0 * y + 1.2
    f = MOI.VectorQuadraticFunction([row1, row2])
    @test f ≈ MOI.Utilities.vectorize([row1, row2])
    @test f ≈ MOI.VectorQuadraticFunction(
        [
            MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2.0, x, x)),
            MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2.0, y, y)),
            MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(6.0, x, x)),
            MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(6.0, y, y)),
        ],
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x)),
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(3.0, y)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(3.0, y)),
        ],
        [4.2, 1.2],
    )
    return
end

function test_cannonicalize_Variable()
    x = MOI.VariableIndex.(1:2)
    f = MOI.VectorOfVariables(x)
    @test MOI.Utilities.canonicalize!(x[1]) === x[1]
    @test MOI.Utilities.canonicalize!(f) === f
    return
end

function test_filter_variables_variable_index()
    f = MOI.VariableIndex(1)
    @test MOI.Utilities.filter_variables(x -> x.value == 1, f) === f
    @test_throws(
        ErrorException(
            "Cannot remove variable from a `VariableIndex` function of the " *
            "same variable.",
        ),
        MOI.Utilities.filter_variables(x -> x.value == 0, f),
    )
    return
end

function test_filter_variables_vector_of_variables()
    f = MOI.VectorOfVariables(MOI.VariableIndex.(1:2))
    s = MOI.Zeros(2)
    g, new_s = MOI.Utilities.filter_variables(x -> x.value != 1, f, s)
    @test g == MOI.VectorOfVariables([MOI.VariableIndex(2)])
    @test new_s == MOI.Zeros(1)
    return
end

function test_mult_bool()
    f = 1.0 * MOI.VariableIndex(1) + 2.0
    @test *(f, false) ≈ zero(MOI.ScalarAffineFunction{Float64})
    return
end

function test_is_complex()
    @test !MOI.Utilities.is_complex(MOI.ScalarAffineFunction{Float64})
    @test !MOI.Utilities.is_complex(MOI.ScalarQuadraticFunction{Float64})
    @test !MOI.Utilities.is_complex(MOI.VectorAffineFunction{Float64})
    @test !MOI.Utilities.is_complex(MOI.VectorQuadraticFunction{Float64})
    @test MOI.Utilities.is_complex(MOI.ScalarAffineFunction{ComplexF64})
    @test MOI.Utilities.is_complex(MOI.ScalarQuadraticFunction{ComplexF64})
    @test MOI.Utilities.is_complex(MOI.VectorAffineFunction{ComplexF64})
    @test MOI.Utilities.is_complex(MOI.VectorQuadraticFunction{ComplexF64})
    return
end

function test_ndims()
    x = MOI.VariableIndex(1)
    @test ndims(1.0 * x) == 0
    @test ndims(MOI.ScalarAffineFunction{Float64}) == 0
    return
end

end  # module

TestUtilitiesFunctions.runtests()
