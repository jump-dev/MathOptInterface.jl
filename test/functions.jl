module TestFunctions

using Test
using MathOptInterface
const MOI = MathOptInterface

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
    xf = MOI.SingleVariable(x)
    yf = MOI.SingleVariable(y)
    zf = MOI.SingleVariable(z)
    function sum_indices(sv1::MOI.SingleVariable, sv2::MOI.SingleVariable)
        return sv1.variable.value + sv2.variable.value
    end
    @test sum_indices.(xf, [yf, zf]) == [3, 4]
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

function test_functions_convert_SingleVariable()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.SingleVariable(x)
    f_vov = convert(MOI.VectorOfVariables, f)
    @test f_vov ≈ MOI.VectorOfVariables([x])
    f_vaf = convert(MOI.VectorAffineFunction{Float64}, f)
    @test f_vaf ≈ MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
        [0.0],
    )
    f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
    @test f_vqf ≈ MOI.VectorQuadraticFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x))],
        MOI.VectorQuadraticTerm{Float64}[],
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
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
        MOI.VectorQuadraticTerm{Float64}[],
        [1.0],
    )
end

function test_functions_convert_ScalarQuadraticFunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarAffineTerm(2.0, x)],
        [MOI.ScalarQuadraticTerm(3.0, x, x)],
        1.0,
    )
    @test_throws(MethodError, convert(MOI.VectorOfVariables, f))
    @test_throws(MethodError, convert(MOI.VectorAffineFunction{Float64}, f))
    f_vqf = convert(MOI.VectorQuadraticFunction{Float64}, f)
    @test f_vqf ≈ MOI.VectorQuadraticFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))],
        MOI.VectorQuadraticTerm{Float64}[MOI.VectorQuadraticTerm(
            1,
            MOI.ScalarQuadraticTerm(3.0, x, x),
        )],
        [1.0],
    )
end

function test_isapprox_SingleVariable()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    @test MOI.SingleVariable(x) == MOI.SingleVariable(x)
    @test MOI.SingleVariable(x) != MOI.SingleVariable(y)
    return
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
        [MOI.ScalarAffineTerm(3, x)],
        MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
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
        MOI.VectorAffineTerm.(
            [1, 2, 1],
            MOI.ScalarAffineTerm.([3, 1, 1], [x, x, y]),
        ),
        MOI.VectorQuadraticTerm.(
            [1, 1, 2],
            MOI.ScalarQuadraticTerm.([1, 2, 3], [x, y, x], [x, y, y]),
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
        MOI.ScalarAffineTerm{Float16}[],
        MOI.ScalarQuadraticTerm{Float16}[],
        Float16(0.0),
    )
    y = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm{Float16}[MOI.ScalarAffineTerm(
            Float16(1.0),
            MOI.VariableIndex(1234),
        )],
        MOI.ScalarQuadraticTerm{Float16}[],
        Float16(0.0),
    )
    z = MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm{Float16}[],
        MOI.ScalarQuadraticTerm{Float16}[],
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
