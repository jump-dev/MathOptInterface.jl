# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFunctions

using Test
import MathOptInterface as MOI

"""
    test_isbits()

Test isbit-ness of VariableIndex and terms.

It is important that these struct remain isbits as otherwise, the performance of
the corresponding function will be deteriored. These tests explicit this design
choice and makes sure that it remains the case.
"""
function test_isbits()
    x = @inferred MOI.VariableIndex(1)
    @test isbits(x)
    at = @inferred MOI.ScalarAffineTerm(1.0, x)
    @test isbits(at)
    @test isbits(@inferred MOI.VectorAffineTerm(1, at))
    qt = @inferred MOI.ScalarQuadraticTerm(1.0, x, x)
    @test isbits(qt)
    @test isbits(@inferred MOI.VectorQuadraticTerm(1, qt))
end

function test_functions_broadcast()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    function sum_indices(sv1::MOI.VariableIndex, sv2::MOI.VariableIndex)
        return sv1.value + sv2.value
    end
    @test sum_indices.(x, [y, z]) == [3, 4]
end

function test_functions_copy_VectorOfVariables()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    f = MOI.VectorOfVariables([x, y])
    f_copy = copy(f)
    f_copy.variables[2] = z
    @test f.variables[2] == y
end

function test_functions_convert_to_variable_index()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test !isfinite(
        MOI.conversion_cost(
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{Float64},
        ),
    )
    for f in (
        1.0 * x,
        1.0 * x + 0.0,
        1.0 * x + 0.0 * y + 0.0,
        0.0 * y + 1.0 * x + 0.0,
    )
        @test convert(MOI.VariableIndex, f) === x
    end
    for f in (
        1.0 * x + 0.5,
        0.5 * x + 0.0,
        1.0 * x + 1.0 * y + 0.0,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
    )
        @test_throws InexactError convert(MOI.VariableIndex, f)
    end
    return
end

function test_functions_convert_VariableIndex()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f_vov = convert(MOI.VectorOfVariables, x)
    @test f_vov ≈ MOI.VectorOfVariables([x])
    f_vaf = convert(MOI.VectorAffineFunction{Float64}, x)
    @test f_vaf ≈ MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
        [0.0],
    )
    f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, x)
    @test f_vqf ≈ MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm{Float64}[],
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
        [0.0],
    )
end

function test_functions_convert_ScalarAffineFunction()
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
        MOI.VectorQuadraticTerm{Float64}[],
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
        [1.0],
    )
end

function test_functions_convert_ScalarQuadraticFunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(3.0, x, x)],
        [MOI.ScalarAffineTerm(2.0, x)],
        1.0,
    )
    @test_throws(MethodError, convert(MOI.VectorOfVariables, f))
    @test_throws(MethodError, convert(MOI.VectorAffineFunction{Float64}, f))
    f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
    @test f_vqf ≈ MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm{Float64}[MOI.VectorQuadraticTerm(
            1,
            MOI.ScalarQuadraticTerm(3.0, x, x),
        )],
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
        [1.0],
    )
end

function test_isapprox_VectorOfVariables()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test MOI.VectorOfVariables([x, y]) == MOI.VectorOfVariables([x, y])
    @test MOI.VectorOfVariables([y, x]) != MOI.VectorOfVariables([x, y])
    @test MOI.VectorOfVariables([x, x]) != MOI.VectorOfVariables([x])
    @test MOI.VectorOfVariables([x]) != MOI.VectorOfVariables([y])
    return
end

function test_isapprox_ScalarAffineFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    @test MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, 1], [x, z]), 1) ≈
          MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 1e-7, 1.0], [x, y, z]),
        1.0,
    ) atol = 1e-6
    @test MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 1e-7], [x, y]),
        1.0,
    ) ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1, x)], 1) atol = 1e-6
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, 4], [x, y]), 6)
    g = deepcopy(f)
    @test g ≈ f
    @test f ≈ g
    f.terms[2] = MOI.ScalarAffineTerm(3, y)
    @test !(g ≈ f)
    @test !(f ≈ g)
    return
end

function test_isapprox_VectorAffineFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 1, 2],
            MOI.ScalarAffineTerm.([2, 4, 3], [x, y, y]),
        ),
        [6, 8],
    )
    g = deepcopy(f)
    @test f ≈ g
    f.terms[3] = MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(9, y))
    @test !(f ≈ g)
    @test !(g ≈ f)
    push!(f.terms, MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(-6, y)))
    @test f ≈ g
    @test g ≈ f
    return
end

function test_isapprox_ScalarQuadraticFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        [MOI.ScalarAffineTerm(3, x)],
        8,
    )
    g = deepcopy(f)
    @test f ≈ g
    push!(f.affine_terms, MOI.ScalarAffineTerm(2, y))
    @test !(f ≈ g)
    g = deepcopy(f)
    push!(f.quadratic_terms, MOI.ScalarQuadraticTerm(2, y, x))
    @test !(f ≈ g)
    @test !(g ≈ f)
    push!(f.quadratic_terms, MOI.ScalarQuadraticTerm(-2, y, x))
    @test f ≈ g
    @test g ≈ f
    return
end

function test_isapprox_VectorQuadraticFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    f = MOI.VectorQuadraticFunction(
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
        ),
        MOI.VectorAffineTerm.(
            [1, 2, 1],
            MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]),
        ),
        [10, 11, 12],
    )
    g = deepcopy(f)
    @test f ≈ g
    @test g ≈ f
    f.affine_terms[1] = MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(4, x))
    @test !(f ≈ g)
    @test !(g ≈ f)
    push!(g.affine_terms, MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(-3, x)))
    push!(g.affine_terms, MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(4, x)))
    @test f ≈ g
    @test g ≈ f
    f.quadratic_terms[1] =
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1, x, x))
    @test !(f ≈ g)
    @test !(g ≈ f)
    return
end

function test_isapprox_issue_1483()
    x = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float16}[],
        MOI.ScalarAffineTerm{Float16}[],
        Float16(0.0),
    )
    y = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float16}[],
        MOI.ScalarAffineTerm{Float16}[MOI.ScalarAffineTerm(
            Float16(1.0),
            MOI.VariableIndex(1234),
        )],
        Float16(0.0),
    )
    z = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float16}[],
        MOI.ScalarAffineTerm{Float16}[],
        Float16(0.0),
    )
    @test !(x ≈ y)
    @test !(y ≈ x)
    @test x ≈ z
    @test z ≈ x
    @test !(y ≈ z)
    @test !(z ≈ y)
    return
end

function test_convert_vectorofvariables()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.VectorOfVariables([x, y])
    g = MOI.Utilities.operate(vcat, Float64, 1.0 * x, 1.0 * y)
    @test !isfinite(MOI.conversion_cost(typeof(f), typeof(g)))
    @test convert(MOI.VectorOfVariables, g) == f
    for g in (
        MOI.Utilities.operate(vcat, Float64, 1.2 * x, 1.0 * y),
        MOI.Utilities.operate(vcat, Float64, 1.0 * y, 1.2 * x),
        MOI.Utilities.operate(vcat, Float64, 1.0 * x, 0.0),
        MOI.Utilities.operate(vcat, Float64, 0.0, 1.0 * x),
        MOI.Utilities.operate(vcat, Float64, 1.0 * x, 1.0 * y + 1.0),
        MOI.Utilities.operate(vcat, Float64, 1.0 * x + 1.0, 1.0 * y),
    )
        @test_throws InexactError convert(MOI.VectorOfVariables, g)
    end
    return
end

function test_ScalarNonlinearFunction_constant()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    @test MOI.constant(f, Float64) === Float64(0)
    @test MOI.constant(f, Int32) === Int32(0)
    return
end

function test_ScalarNonlinearFunction_isapprox()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.ScalarNonlinearFunction(:+, Any[f, 0.0])
    h = MOI.ScalarNonlinearFunction(:+, Any[f, 1.0])
    @test f ≈ f
    @test !(f ≈ g)
    @test !(g ≈ h)
    return
end

function test_convert_ScalarNonlinearFunction_ScalarAffineTerm()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarAffineTerm(1.0, x)
    g = MOI.ScalarNonlinearFunction(:*, Any[1.0, x])
    h = MOI.ScalarNonlinearFunction(:*, Any[x, 1.0])
    @test convert(MOI.ScalarNonlinearFunction, f) ≈ g
    @test convert(MOI.ScalarAffineTerm{Float64}, g) == f
    @test convert(MOI.ScalarAffineTerm{Float64}, h) == f
    f = MOI.ScalarAffineTerm(1, x)
    g = MOI.ScalarNonlinearFunction(:*, Any[1, x])
    h = MOI.ScalarNonlinearFunction(:*, Any[x, 1])
    @test convert(MOI.ScalarNonlinearFunction, f) ≈ g
    @test convert(MOI.ScalarAffineTerm{Int}, h) == f
    for f_error in (
        MOI.ScalarNonlinearFunction(:*, Any[1.0, x, 2.0]),
        MOI.ScalarNonlinearFunction(:+, Any[1.0, x]),
        MOI.ScalarNonlinearFunction(:*, Any[x, x]),
    )
        @test_throws(
            InexactError,
            convert(MOI.ScalarAffineTerm{Float64}, f_error),
        )
    end
    return
end

function test_convert_ScalarNonlinearFunction_ScalarAffineFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    for f in (
        1.0 * x,
        1.0 * x + 2.0,
        1.0 * x + 1.0 * x + 2.0,
        1.0 * x + 2.0 * y + 2.0,
        2.0 * y + 2.0 + 2.0 * x,
    )
        g = convert(MOI.ScalarNonlinearFunction, f)
        @test convert(MOI.ScalarAffineFunction{Float64}, g) ≈ f
    end
    f_add = MOI.ScalarNonlinearFunction(:+, Any[1.0, x])
    @test !isfinite(
        MOI.conversion_cost(
            MOI.ScalarQuadraticFunction{Float64},
            typeof(f_add),
        ),
    )
    for (f, g) in (
        MOI.ScalarNonlinearFunction(:*, Any[1.0, x]) => 1.0 * x,
        MOI.ScalarNonlinearFunction(:+, Any[1.0, x]) => 1.0 + x,
        MOI.ScalarNonlinearFunction(:+, Any[f_add, f_add]) => 2.0 + 2.0 * x,
    )
        @test convert(MOI.ScalarAffineFunction{Float64}, f) ≈ g
    end
    for f_error in (
        MOI.ScalarNonlinearFunction(:/, Any[1.0, x]),
        MOI.ScalarNonlinearFunction(:+, Any[1.0, 1.0*x*x]),
    )
        @test_throws(
            InexactError,
            convert(MOI.ScalarAffineFunction{Float64}, f_error),
        )
    end
    return
end

function test_convert_ScalarNonlinearFunction_ScalarQuadraticTerm()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.ScalarQuadraticTerm(1, x, y)
    g = MOI.ScalarNonlinearFunction(:*, Any[1, x, y])
    h = MOI.ScalarNonlinearFunction(:*, Any[x, 1, y])
    i = MOI.ScalarNonlinearFunction(:*, Any[x, y, 1])
    @test convert(MOI.ScalarNonlinearFunction, f) ≈ g
    @test convert(MOI.ScalarQuadraticTerm{Int}, g) == f
    @test convert(MOI.ScalarQuadraticTerm{Int}, h) == f
    @test convert(MOI.ScalarQuadraticTerm{Int}, i) == f
    f = MOI.ScalarQuadraticTerm(2.0, x, x)
    g = MOI.ScalarNonlinearFunction(:*, Any[1.0, x, x])
    h = MOI.ScalarNonlinearFunction(:*, Any[x, 1.0, x])
    i = MOI.ScalarNonlinearFunction(:*, Any[x, x, 1.0])
    @test convert(MOI.ScalarNonlinearFunction, f) ≈ g
    @test convert(MOI.ScalarQuadraticTerm{Float64}, g) == f
    @test convert(MOI.ScalarQuadraticTerm{Float64}, h) == f
    @test convert(MOI.ScalarQuadraticTerm{Float64}, i) == f
    for f_error in (
        MOI.ScalarNonlinearFunction(:*, Any[1.0, x, 2.0]),
        MOI.ScalarNonlinearFunction(:+, Any[1.0, x, x]),
        MOI.ScalarNonlinearFunction(:*, Any[x, x]),
    )
        @test_throws(
            InexactError,
            convert(MOI.ScalarQuadraticTerm{Float64}, f_error),
        )
    end
    return
end

function test_convert_ScalarNonlinearFunction_ScalarQuadraticFunction()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    for f in (
        1.0 * x * x,
        1.0 * x * x + 2.0,
        1.0 * x * x + 1.0 * x * y + 2.0,
        1.0 * x + 2.0 * y * y + 2.0,
        2.0 * y + 2.0 + 2.0 * x * x,
    )
        g = convert(MOI.ScalarNonlinearFunction, f)
        @test convert(MOI.ScalarQuadraticFunction{Float64}, g) ≈ f
    end
    f_add = MOI.ScalarNonlinearFunction(:+, Any[1.0, x])
    @test !isfinite(
        MOI.conversion_cost(
            MOI.ScalarQuadraticFunction{Float64},
            typeof(f_add),
        ),
    )
    for (f, g) in (
        MOI.ScalarNonlinearFunction(:*, Any[1.0, x]) => 0.0 * x * x + 1.0 * x,
        MOI.ScalarNonlinearFunction(:*, Any[x, 1.0, x]) => 1.0 * x * x,
        MOI.ScalarNonlinearFunction(:+, Any[1.0, x]) => 0.0 * x * x + 1.0 + x,
        MOI.ScalarNonlinearFunction(:+, Any[f_add, f_add]) =>
            0.0 * x * x + 2.0 + 2.0 * x,
        MOI.ScalarNonlinearFunction(:^, Any[x, 2]) => 1.0 * x * x,
    )
        @test convert(MOI.ScalarQuadraticFunction{Float64}, f) ≈ g
    end
    for f_error in (MOI.ScalarNonlinearFunction(:/, Any[1.0, x]),)
        @test_throws(
            InexactError,
            convert(MOI.ScalarQuadraticFunction{Float64}, f_error),
        )
    end
    return
end

function test_convert_VectorQuadraticFunction_VectorAffineFunction()
    x = MOI.VariableIndex(1)
    quadratic = MOI.VectorQuadraticTerm{Float64}[]
    affine = [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))]
    constants = [2.0]
    f = MOI.VectorQuadraticFunction(quadratic, affine, constants)
    g = MOI.VectorAffineFunction(affine, constants)
    @test !isfinite(MOI.conversion_cost(typeof(g), typeof(f)))
    @test convert(MOI.VectorAffineFunction{Float64}, f) ≈ g
    push!(
        quadratic,
        MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(1.0, x, x)),
    )
    @test_throws InexactError convert(MOI.VectorAffineFunction{Float64}, f)
    return
end

function test_convert_VectorAffineFunction_VectorQuadraticFunction()
    x = MOI.VariableIndex(1)
    quadratic = MOI.VectorQuadraticTerm{Float64}[]
    affine = [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))]
    constants = [2.0]
    f = MOI.VectorAffineFunction(affine, constants)
    g = MOI.VectorQuadraticFunction(quadratic, affine, constants)
    @test MOI.conversion_cost(typeof(g), typeof(f)) == 10
    @test convert(MOI.VectorQuadraticFunction{Float64}, f) ≈ g
    return
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end

TestFunctions.runtests()
