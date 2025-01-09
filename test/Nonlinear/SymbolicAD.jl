# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestSymbolicAD

using Test
import MathOptInterface as MOI
import MathOptInterface.Nonlinear: SymbolicAD

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

function test_simplify()
    x = MOI.VariableIndex(1)
    @test SymbolicAD.simplify(x) === x
    @test SymbolicAD.simplify(1.0) === 1.0
    return
end

function test_simplify_ScalarAffineFunction()
    f = zero(MOI.ScalarAffineFunction{Float64})
    @test SymbolicAD.simplify(f) == 0.0
    f = MOI.ScalarAffineFunction{Float64}(MOI.ScalarAffineTerm{Float64}[], 2.0)
    @test SymbolicAD.simplify(f) == 2.0
    x = MOI.VariableIndex(1)
    @test SymbolicAD.simplify(1.0 * x + 1.0) ≈ 1.0 * x + 1.0
    @test SymbolicAD.simplify(1.0 * x + 2.0 * x + 1.0) ≈ 3.0 * x + 1.0
    return
end

function test_simplify_ScalarQuadraticFunction()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float64}[],
        [MOI.ScalarAffineTerm{Float64}(1.0, x)],
        1.0,
    )
    @test SymbolicAD.simplify(f) ≈ 1.0 * x + 1.0
    @test SymbolicAD.simplify(1.0 * x * x + 1.0) ≈ 1.0 * x * x + 1.0
    g = 1.0 * x * x + 2.0 * x * x + 1.0
    @test SymbolicAD.simplify(g) ≈ 3.0 * x * x + 1.0
    return
end

function test_simplify_ScalarNonlinearFunction()
    x = MOI.VariableIndex(1)
    # sin(3 * (x^0)) -> sin(3)
    f = MOI.ScalarNonlinearFunction(:^, Any[x, 0])
    g = MOI.ScalarNonlinearFunction(:*, Any[3, f])
    h = MOI.ScalarNonlinearFunction(:sin, Any[g])
    @test SymbolicAD.simplify(h) ≈ sin(3)
    # sin(log(x)) -> sin(log(x))
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.ScalarNonlinearFunction(:sin, Any[f])
    @test SymbolicAD.simplify(g) ≈ g
    return
end

# simplify(::Val{:*}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_multiplication()
    x, y, z = MOI.VariableIndex.(1:3)
    # *(x, *(y, z)) -> *(x, y, z)
    @test ≈(
        SymbolicAD.simplify(
            MOI.ScalarNonlinearFunction(
                :*,
                Any[x, MOI.ScalarNonlinearFunction(:*, Any[y, z])],
            ),
        ),
        MOI.ScalarNonlinearFunction(:*, Any[x, y, z]),
    )
    # *(x, *(y, z, *(x, 2))) -> *(x, y, z, x, 2)
    f = MOI.ScalarNonlinearFunction(:*, Any[x, 2])
    @test ≈(
        SymbolicAD.simplify(
            MOI.ScalarNonlinearFunction(
                :*,
                Any[x, MOI.ScalarNonlinearFunction(:*, Any[y, z, f])],
            ),
        ),
        MOI.ScalarNonlinearFunction(:*, Any[x, y, z, x, 2]),
    )
    # *(x, 3, 2) -> *(x, 6)
    @test ≈(
        SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[x, 3, 2])),
        MOI.ScalarNonlinearFunction(:*, Any[x, 6]),
    )
    # *(3, x, 2) -> *(6, x)
    @test ≈(
        SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[3, x, 2])),
        MOI.ScalarNonlinearFunction(:*, Any[6, x]),
    )
    # *(x, 1) -> x
    @test ≈(SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[x, 1])), x)
    # *(x, 0) -> 0
    @test ≈(SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[x, 0])), 0)
    # *(-(x, x), 1) -> 0
    f = MOI.ScalarNonlinearFunction(:-, Any[x, x])
    @test ≈(SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[f, 1])), 0)
    # *() -> true
    @test ≈(SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:*, Any[])), 1)
    return
end

# simplify(::Val{:+}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_addition()
    x, y, z = MOI.VariableIndex.(1:3)
    # (+(x, +(y, z)))=>(+(x, y, z)),
    @test ≈(
        SymbolicAD.simplify(
            MOI.ScalarNonlinearFunction(
                :+,
                Any[x, MOI.ScalarNonlinearFunction(:+, Any[y, z])],
            ),
        ),
        MOI.ScalarNonlinearFunction(:+, Any[x, y, z]),
    )
    # +(sin(x), -cos(x))=>sin(x)-cos(x),
    sinx = MOI.ScalarNonlinearFunction(:sin, Any[x])
    cosx = MOI.ScalarNonlinearFunction(:cos, Any[x])
    @test ≈(
        SymbolicAD.simplify(
            MOI.ScalarNonlinearFunction(
                :+,
                Any[sinx, MOI.ScalarNonlinearFunction(:-, Any[cosx])],
            ),
        ),
        MOI.ScalarNonlinearFunction(:-, Any[sinx, cosx]),
    )
    # (+(x, 1, 2))=>(+(x, 3)),
    @test ≈(
        SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:+, Any[x, 1, 2])),
        MOI.ScalarNonlinearFunction(:+, Any[x, 3]),
    )
    # (+(1, x, 2))=>(+(3, x)),
    @test ≈(
        SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:+, Any[1, x, 2])),
        MOI.ScalarNonlinearFunction(:+, Any[3, x]),
    )
    # +(x, 0) -> x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:+, Any[x, 0])) ≈ x
    # +(0, x) -> x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:+, Any[0, x])) ≈ x
    # +(-(x, x), 0) -> 0
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[MOI.ScalarNonlinearFunction(:-, Any[x, x]), 0],
    )
    @test SymbolicAD.simplify(f) === false
    return
end

# simplify(::Val{:-}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_subtraction()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    f = MOI.ScalarNonlinearFunction(:-, Any[x])
    # -x -> -x
    @test SymbolicAD.simplify(f) ≈ f
    # -(-(x)) -> x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:-, Any[f])) ≈ x
    # -(x, 0) -> x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:-, Any[x, 0])) ≈ x
    # -(0, x) -> -x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:-, Any[0, x])) ≈ f
    # -(x, x) -> 0
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:-, Any[x, x])) ≈ 0
    # -(x, -y) -> +(x, y)
    f = MOI.ScalarNonlinearFunction(
        :-,
        Any[x, MOI.ScalarNonlinearFunction(:-, Any[y])],
    )
    @test SymbolicAD.simplify(f) ≈ MOI.ScalarNonlinearFunction(:+, Any[x, y])
    # -(x, y) -> -(x, y)
    f = MOI.ScalarNonlinearFunction(:-, Any[x, y])
    @test SymbolicAD.simplify(f) ≈ f
    return
end

# simplify(::Val{:^}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_power()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    # x^0 -> 1
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:^, Any[x, 0])) == 1
    # x^1 -> x
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:^, Any[x, 1])) == x
    # 0^x -> 0
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:^, Any[0, x])) == 0
    # 1^x -> 1
    @test SymbolicAD.simplify(MOI.ScalarNonlinearFunction(:^, Any[1, x])) == 1
    # x^y -> x^y
    f = MOI.ScalarNonlinearFunction(:^, Any[x, y])
    @test SymbolicAD.simplify(f) ≈ f
    return
end

function test_simplify_VectorAffineFunction()
    f = MOI.VectorAffineFunction{Float64}(
        MOI.VectorAffineTerm{Float64}[],
        [0.0, 1.0, 2.0],
    )
    @test SymbolicAD.simplify(f) == [0.0, 1.0, 2.0]
    x = MOI.VariableIndex(1)
    f = MOI.Utilities.operate(vcat, Float64, 1.0, x, 2.0 * x + 1.0 * x)
    @test SymbolicAD.simplify(f) ≈ f
    return
end

function test_simplify_VectorQuadraticFunction()
    f = MOI.VectorQuadraticFunction{Float64}(
        MOI.VectorQuadraticTerm{Float64}[],
        MOI.VectorAffineTerm{Float64}[],
        [0.0, 1.0, 2.0],
    )
    @test SymbolicAD.simplify(f) == [0.0, 1.0, 2.0]
    x = MOI.VariableIndex(1)
    f = MOI.VectorQuadraticFunction{Float64}(
        MOI.VectorQuadraticTerm{Float64}[],
        MOI.VectorAffineTerm{Float64}[
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(3.0, x))
        ],
        [1.0, 0.0],
    )
    g = MOI.Utilities.operate(vcat, Float64, 1.0, 3.0 * x)
    @test SymbolicAD.simplify(f) ≈ g
    f = MOI.Utilities.operate(vcat, Float64, 1.0, 2.0 * x * x)
    @test SymbolicAD.simplify(f) ≈ f
    return
end

function test_simplify_VectorNonlinearFunction()
    x = MOI.VariableIndex.(1:3)
    y = MOI.ScalarNonlinearFunction(
        :+,
        Any[MOI.ScalarNonlinearFunction(:^, Any[xi, 2]) for xi in x],
    )
    x_plus = [MOI.ScalarNonlinearFunction(:+, Any[xi]) for xi in x]
    function wrap(f)
        return MOI.ScalarNonlinearFunction(
            :+,
            Any[MOI.ScalarNonlinearFunction(:-, Any[f, 0.0]), 0.0],
        )
    end
    f = MOI.VectorNonlinearFunction(wrap.([y; x_plus]))
    g = MOI.VectorNonlinearFunction([y; x_plus])
    @test SymbolicAD.simplify(f) ≈ g
    return
end

end  # module

TestSymbolicAD.runtests()
