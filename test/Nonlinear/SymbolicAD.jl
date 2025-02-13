# Copyright (c) 2017: Miles Lubin and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMathOptSymbolicAD

using Test

import MathOptInterface as MOI
import MathOptInterface.Nonlinear: SymbolicAD

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            Test.@testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

op(head, args...) = MOI.ScalarNonlinearFunction(head, Any[args...])

function test_derivative()
    x, y, z = MOI.VariableIndex.(1:3)
    sin_x = op(:sin, x)
    cos_x = op(:cos, x)
    @testset "$f" for (f, fp) in Any[
        # derivative(::Real, ::MOI.VariableIndex)
        1.0=>0.0,
        1.23=>0.0,
        # derivative(f::MOI.VariableIndex, x::MOI.VariableIndex)
        x=>1.0,
        y=>0.0,
        # derivative(f::MOI.ScalarAffineFunction{T}, x::MOI.VariableIndex)
        1.0*x=>1.0,
        1.0*x+2.0=>1.0,
        2.0*x+2.0=>2.0,
        2.0*x+y+2.0=>2.0,
        2.0*x+y+z+2.0=>2.0,
        # derivative(f::MOI.ScalarQuadraticFunction{T}, x::MOI.VariableIndex)
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.0 * x)=>1.0,
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.0 * x + 0.0 * y)=>1.0,
        1.0*x*y=>1.0*y,
        1.0*y*x=>1.0*y,
        1.0*x*x=>2.0*x,
        1.0*x*x+3.0*x+4.0=>2.0*x+3.0,
        2.0*x*x-2.0*x+1.0=>4.0*x-2.0,
        9.0*x*x+6.0*x+1.0=>18.0*x+6.0,
        # Univariate
        #   f.head == :+
        op(:+, x)=>1,
        op(:+, sin_x)=>cos_x,
        #   f.head == :-
        op(:-, sin_x)=>op(:-, cos_x),
        #   f.head == :abs
        op(:abs, sin_x)=>op(:*, op(:ifelse, op(:(>=), sin_x, 0), 1, -1), cos_x),
        #   f.head == :sign
        op(:sign, x)=>false,
        #   f.head == :deg2rad
        op(:deg2rad, x)=>deg2rad(1),
        #   f.head == :rad2deg
        op(:rad2deg, x)=>rad2deg(1),
        # SYMBOLIC_UNIVARIATE_EXPRESSIONS
        sin_x=>cos_x,
        cos_x=>op(:-, sin_x),
        op(:log, x)=>op(:/, 1, x),
        op(:log, 2.0 * x)=>op(:*, op(:/, 1, 2.0 * x), 2.0),
        # f.head == :+
        op(:+, sin_x, cos_x)=>op(:-, cos_x, sin_x),
        # f.head == :-
        op(:-, sin_x, cos_x)=>op(:+, cos_x, sin_x),
        # f.head == :*
        op(:*, x, y, z)=>op(:*, y, z),
        op(:*, y, x, z)=>op(:*, y, z),
        op(:*, y, z, x)=>op(:*, y, z),
        # :^
        op(:^, sin_x, 2)=>op(:*, 2.0, sin_x, cos_x),
        op(:^, sin_x, 1)=>cos_x,
        op(
            :^,
            x,
            x,
        )=>op(
            :+,
            op(:*, x, op(:^, x, x - 1)),
            op(:*, op(:^, x, x), op(:log, x)),
        ),
        # :/
        op(:/, x, 2)=>0.5,
        op(
            :/,
            op(:^, x, 2),
            op(:+, x, 1),
        )=>op(
            :/,
            op(:-, op(:*, 2, x, op(:+, x, 1)), op(:^, x, 2)),
            op(:^, op(:+, x, 1), 2),
        ),
        # :ifelse
        op(:ifelse, z, 1.0 * x * x, x)=>op(:ifelse, z, 2.0 * x, 1.0),
        # :atan
        op(
            :atan,
            x,
            sin_x,
        )=>op(
            :/,
            op(:+, op(:*, x, cos_x), sin_x),
            op(:+, op(:^, x, 2), op(:^, sin_x, 2)),
        ),
        # :min
        op(
            :min,
            x,
            1.0 * x * x,
        )=>op(:ifelse, op(:(<=), x, op(:min, x, 1.0 * x * x)), 1.0, 2.0 * x),
        # :max
        op(
            :max,
            x,
            1.0 * x * x,
        )=>op(:ifelse, op(:(>=), x, op(:max, x, 1.0 * x * x)), 1.0, 2.0 * x),
        # comparisons
        op(:(>=), x, y)=>false,
        op(:(==), x, y)=>false,
    ]
        @test SymbolicAD.simplify!(SymbolicAD.derivative(f, x)) ≈ fp
    end
    return
end

function test_derivative_error()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:foo, Any[x, x])
    @test_throws(MOI.UnsupportedNonlinearOperator, SymbolicAD.derivative(f, x),)
    return
end

function test_variable()
    x, y, z = MOI.VariableIndex.(1:3)
    @testset "$f" for (f, fp) in Any[
        # ::Real
        1.0=>[],
        # ::VariableRef,
        x=>[x],
        # ::AffExpr
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 2.0)=>[],
        x+1.0=>[x],
        2.0*x+1.0=>[x],
        2.0*x+y+1.0=>[x, y],
        y+1.0+z=>[y, z],
        # ::QuadExpr
        zero(MOI.ScalarQuadraticFunction{Float64})=>[],
        convert(MOI.ScalarQuadraticFunction{Float64}, 1.0 * x + 1.0)=>[x],
        convert(
            MOI.ScalarQuadraticFunction{Float64},
            1.0 * x + 1.0 + y,
        )=>[x, y],
        1.0*x*x=>[x],
        1.0*x*x+x=>[x],
        1.0*x*x+y=>[y, x],
        1.0*x*y=>[x, y],
        1.0*y*x=>[y, x],
        # ::NonlinearExpr
        MOI.ScalarNonlinearFunction(:sin, Any[x])=>[x],
        MOI.ScalarNonlinearFunction(:sin, Any[1.0*x+y])=>[x, y],
        MOI.ScalarNonlinearFunction(
            :*,
            Any[
                MOI.ScalarNonlinearFunction(:sin, Any[x]),
                MOI.ScalarNonlinearFunction(:sin, Any[y]),
            ],
        )=>[x, y],
        MOI.ScalarNonlinearFunction(
            :^,
            Any[MOI.ScalarNonlinearFunction(:log, Any[x]), 2],
        )=>[x],
    ]
        @test SymbolicAD.variables(f) == fp
    end
    return
end

function test_simplify_VariableIndex()
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
    f = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{Float64}[],
        MOI.ScalarAffineTerm{Float64}[],
        2.0,
    )
    @test SymbolicAD.simplify(f) === 2.0
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
    ret = MOI.ScalarNonlinearFunction(:*, Any[x, 3, 2])
    @test ≈(
        SymbolicAD.simplify(ret),
        MOI.ScalarNonlinearFunction(:*, Any[x, 6]),
    )
    # *(3, x, 2) -> *(6, x)
    ret = MOI.ScalarNonlinearFunction(:*, Any[3, x, 2])
    @test ≈(
        SymbolicAD.simplify(ret),
        MOI.ScalarNonlinearFunction(:*, Any[6, x]),
    )
    # *(x, 1) -> x
    ret = MOI.ScalarNonlinearFunction(:*, Any[x, 1])
    @test ≈(SymbolicAD.simplify(ret), x)
    # *(x, 0) -> 0
    ret = MOI.ScalarNonlinearFunction(:*, Any[x, 0])
    @test ≈(SymbolicAD.simplify(ret), 0)
    # *(-(x, x), 1) -> 0
    f = MOI.ScalarNonlinearFunction(:-, Any[x, x])
    ret = MOI.ScalarNonlinearFunction(:*, Any[f, 1])
    @test ≈(SymbolicAD.simplify(ret), 0)
    # *() -> true
    ret = MOI.ScalarNonlinearFunction(:*, Any[])
    @test ≈(SymbolicAD.simplify(ret), 1)
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
    ret = MOI.ScalarNonlinearFunction(:+, Any[x, 1, 2])
    @test ≈(
        SymbolicAD.simplify(ret),
        MOI.ScalarNonlinearFunction(:+, Any[x, 3]),
    )
    # (+(1, x, 2))=>(+(3, x)),
    ret = MOI.ScalarNonlinearFunction(:+, Any[1, x, 2])
    @test ≈(
        SymbolicAD.simplify(ret),
        MOI.ScalarNonlinearFunction(:+, Any[3, x]),
    )
    # +(x, 0) -> x
    ret = MOI.ScalarNonlinearFunction(:+, Any[x, 0])
    @test SymbolicAD.simplify(ret) ≈ x
    # +(0, x) -> x
    ret = MOI.ScalarNonlinearFunction(:+, Any[0, x])
    @test SymbolicAD.simplify(ret) ≈ x
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
    ret = MOI.ScalarNonlinearFunction(:-, Any[f])
    @test SymbolicAD.simplify(ret) ≈ x
    # -(x, 0) -> x
    ret = MOI.ScalarNonlinearFunction(:-, Any[x, 0])
    @test SymbolicAD.simplify(ret) ≈ x
    # -(0, x) -> -x
    ret = MOI.ScalarNonlinearFunction(:-, Any[0, x])
    @test SymbolicAD.simplify(ret) ≈ f
    # -(x, x) -> 0
    ret = MOI.ScalarNonlinearFunction(:-, Any[x, x])
    @test SymbolicAD.simplify(ret) ≈ 0
    # -(x, -y) -> +(x, y)
    f = MOI.ScalarNonlinearFunction(
        :-,
        Any[x, MOI.ScalarNonlinearFunction(:-, Any[y])],
    )
    target = MOI.ScalarNonlinearFunction(:+, Any[x, y])
    @test SymbolicAD.simplify(f) ≈ target
    # -(x, y) -> -(x, y)
    f = MOI.ScalarNonlinearFunction(:-, Any[x, y])
    @test SymbolicAD.simplify(f) ≈ f
    return
end

# simplify(::Val{:^}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_power()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    # x^0 -> 1
    f = MOI.ScalarNonlinearFunction(:^, Any[x, 0])
    @test SymbolicAD.simplify(f) == 1
    # x^1 -> x
    f = MOI.ScalarNonlinearFunction(:^, Any[x, 1])
    @test SymbolicAD.simplify(f) == x
    # 0^x -> 0
    f = MOI.ScalarNonlinearFunction(:^, Any[0, x])
    @test SymbolicAD.simplify(f) == 0
    # 1^x -> 1
    f = MOI.ScalarNonlinearFunction(:^, Any[1, x])
    @test SymbolicAD.simplify(f) == 1
    # x^y -> x^y
    f = MOI.ScalarNonlinearFunction(:^, Any[x, y])
    @test SymbolicAD.simplify(f) ≈ f
    return
end

# simplify(::Val{:ifelse}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_ifelse()
    x, y, z = MOI.VariableIndex(1), MOI.VariableIndex(2), MOI.VariableIndex(3)
    f = MOI.ScalarNonlinearFunction(:ifelse, Any[false, x, y])
    @test SymbolicAD.simplify(f) == y
    f = MOI.ScalarNonlinearFunction(:ifelse, Any[true, x, y])
    @test SymbolicAD.simplify(f) == x
    f = MOI.ScalarNonlinearFunction(:ifelse, Any[y, x, x])
    @test SymbolicAD.simplify(f) == x
    f = MOI.ScalarNonlinearFunction(:ifelse, Any[x, y, z])
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
        [MOI.VectorAffineTerm{Float64}(2, MOI.ScalarAffineTerm(3.0, x))],
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

function test_simplify_deep()
    N = 10_000
    x = MOI.VariableIndex.(1:N)
    f = MOI.ScalarNonlinearFunction(:^, Any[x[1], 1])
    for i in 2:N
        g = MOI.ScalarNonlinearFunction(:^, Any[x[i], 1])
        f = MOI.ScalarNonlinearFunction(:+, Any[f, g])
    end
    @test ≈(SymbolicAD.simplify(f), MOI.ScalarNonlinearFunction(:+, x))
    return
end

function test_simplify_shared_false()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:^, Any[x, 1])
    g = MOI.ScalarNonlinearFunction(:+, Any[f, f])
    h = MOI.ScalarNonlinearFunction(:-, Any[g, g])
    @test ≈(SymbolicAD.simplify!(h), false)
    return
end

function test_simplify_shared_node()
    x = MOI.VariableIndex(1)
    f1 = MOI.ScalarNonlinearFunction(:^, Any[x, 1])
    f2 = MOI.ScalarNonlinearFunction(:sin, Any[f1])
    f3 = MOI.ScalarNonlinearFunction(:cos, Any[f1])
    f4 = MOI.ScalarNonlinearFunction(:+, Any[f2, f3])
    g1 = MOI.ScalarNonlinearFunction(:sin, Any[x])
    g2 = MOI.ScalarNonlinearFunction(:cos, Any[x])
    g3 = MOI.ScalarNonlinearFunction(:+, Any[g1, g2])
    @test ≈(SymbolicAD.simplify!(f4), g3)
    return
end

function test_SymbolicAD_Evaluator()
    x = MOI.VariableIndex.(1:4)
    model = MOI.Nonlinear.Model()
    for i in 1:3
        MOI.Nonlinear.add_constraint(
            model,
            :($(x[4]) * log($(x[i]) + $i)),
            MOI.LessThan(2.0),
        )
    end
    MOI.Nonlinear.add_constraint(model, :($(x[1]) * $(x[2])), MOI.LessThan(2.0))
    MOI.Nonlinear.set_objective(model, :($(x[1]) * $(x[2])))
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), x)
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :Hess, :ExprGraph]
    @test_throws AssertionError MOI.initialize(evaluator, [:Foo])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [1.0, 2.0, 3.0, 4.0]
    f = MOI.eval_objective(evaluator, x)
    @test f == 2.0
    ∇f = zeros(4)
    MOI.eval_objective_gradient(evaluator, ∇f, x)
    @test ∇f == [2.0, 1.0, 0.0, 0.0]
    g = zeros(4)
    MOI.eval_constraint(evaluator, g, x)
    @test g == [4 * log(1 + 1), 4 * log(2 + 2), 4 * log(3 + 3), 1 * 2]
    J_s = MOI.jacobian_structure(evaluator)
    J = zeros(length(J_s))
    p = sortperm(J_s)
    MOI.eval_constraint_jacobian(evaluator, J, x)
    @test J[p] == [2.0, log(2), 1.0, log(4), 2 / 3, log(6), 2.0, 1.0]
    H_s = MOI.hessian_lagrangian_structure(evaluator)
    H = zeros(length(H_s))
    MOI.eval_hessian_lagrangian(evaluator, H, x, 1.5, [1.1, 1.2, 1.3, 1.4])
    p = sortperm(H_s)
    @test H[p] == [
        -1.1,                   # dg(1) / d(1, 1)
        1.5,                    # df    / d(1, 2)
        1.4,                    # dg(4) / d(1, 2)
        -0.3,                   # dg(2) / d(2, 2)
        -0.14444444444444443,   # dg(3) / d(3, 3)
        0.55,                   # dg(1) / d(4, 1)
        0.3,                    # dg(2) / d(4, 2)
        0.21666666666666667,    # dg(3) / d(4, 3)
    ]
    return
end

function test_SymbolicAD_Evaluator_squared()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, :($x^2))
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), [x])
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :Hess, :ExprGraph]
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [3.0]
    f = MOI.eval_objective(evaluator, x)
    @test f == 3.0^2
    ∇f = zeros(1)
    MOI.eval_objective_gradient(evaluator, ∇f, x)
    @test ∇f == [6.0]
    H_s = MOI.hessian_lagrangian_structure(evaluator)
    @test H_s == [(1, 1)]
    H = zeros(1)
    MOI.eval_hessian_lagrangian(evaluator, H, x, 1.25, [])
    @test H == [2.5]
    return
end

function test_SymbolicAD_Evaluator_ifelse()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, :(ifelse($x > 0, $x, -$x)))
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), [x])
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :Hess, :ExprGraph]
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [3.0]
    f = MOI.eval_objective(evaluator, x)
    @test f == 3.0
    x = [-4.0]
    f = MOI.eval_objective(evaluator, x)
    @test f == 4.0
    ∇f = zeros(1)
    MOI.eval_objective_gradient(evaluator, ∇f, x)
    @test ∇f == [-1.0]
    H_s = MOI.hessian_lagrangian_structure(evaluator)
    @test isempty(H_s)
    return
end

function test_SymbolicAD_Evaluator_comparison()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, :(ifelse(($x < -1) || ($x > 1), $x, 0)))
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), [x])
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :Hess, :ExprGraph]
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = zeros(1)
    ∇f = zeros(1)
    for (xi, f, df) in [
        (-2.0, -2.0, 1.0),
        (-1.01, -1.01, 1.0),
        (-1.0, 0.0, 0.0),
        (1.0, 0.0, 0.0),
        (1.01, 1.01, 1.0),
        (2.0, 2.0, 1.0),
    ]
        x[1] = xi
        @test MOI.eval_objective(evaluator, x) == f
        MOI.eval_objective_gradient(evaluator, ∇f, x)
        @test only(∇f) == df
    end
    return
end

function test_packed_operator_bit_fiddling()
    registry = MOI.Nonlinear.OperatorRegistry()
    for op in registry.univariate_operators
        operator = SymbolicAD._op_nargs_to_operator(registry, op, 1)
        type, id, nargs = SymbolicAD._operator_to_type_id_nargs(operator)
        @test type == :univariate
        @test id == registry.univariate_operator_to_id[op]
        @test nargs == 1
    end
    for op in registry.multivariate_operators, n in 2:5
        operator = SymbolicAD._op_nargs_to_operator(registry, op, n)
        type, id, nargs = SymbolicAD._operator_to_type_id_nargs(operator)
        @test type == :multivariate
        @test id == registry.multivariate_operator_to_id[op]
        @test nargs == n
    end
    for op in registry.logic_operators
        operator = SymbolicAD._op_nargs_to_operator(registry, op, 2)
        type, id, nargs = SymbolicAD._operator_to_type_id_nargs(operator)
        @test type == :logic
        @test id == registry.logic_operator_to_id[op]
        @test nargs == 2
    end
    for op in registry.comparison_operators
        operator = SymbolicAD._op_nargs_to_operator(registry, op, 2)
        type, id, nargs = SymbolicAD._operator_to_type_id_nargs(operator)
        @test type == :comparison
        @test id == registry.comparison_operator_to_id[op]
        @test nargs == 2
    end
    MOI.Nonlinear.register_operator(registry, :op_foo, 1, x -> x^2)
    D = SymbolicAD.__DERIVATIVE__
    for (prefix, ret_type) in (
        "" => :univariate,
        D => :univariate_derivative,
        (D * D) => :univariate_second_derivative,
    )
        op = Symbol("$(prefix)op_foo")
        operator = SymbolicAD._op_nargs_to_operator(registry, op, 1)
        type, id, nargs = SymbolicAD._operator_to_type_id_nargs(operator)
        @test type == ret_type
        @test id == registry.univariate_operator_to_id[:op_foo]
        @test id >= registry.univariate_user_operator_start
        @test nargs == 1
    end
    return
end

function test_SymbolicAD_show()
    x = MOI.VariableIndex.(1:4)
    model = MOI.Nonlinear.Model()
    for i in 1:3
        MOI.Nonlinear.add_constraint(
            model,
            :($(x[4]) * log($(x[i]) + $i)),
            MOI.LessThan(2.0),
        )
    end
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), x)
    _, dag = only(evaluator.backend.dag)
    contents = sprint(show, dag)
    @test occursin("# x[1]", contents)
    @test occursin("# p[1]", contents)
    @test occursin("# multivariate(op = 1, children = 1:2)", contents)
    @test occursin("children", contents)
    return
end

function test_SymbolicAD_subexpressions()
    x = MOI.VariableIndex.(1:2)
    model = MOI.Nonlinear.Model()
    expr = MOI.Nonlinear.add_expression(model, :($(x[1]) * $(x[2])))
    MOI.Nonlinear.add_constraint(model, expr, MOI.LessThan(2.0))
    @test_throws(
        ErrorException("Subexpressions not supported"),
        MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), x),
    )
    return
end

function test_SymbolicAD_parameters()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    p = MOI.Nonlinear.add_parameter(model, 1.0)
    MOI.Nonlinear.add_constraint(model, :($x * $p), MOI.LessThan(2.0))
    @test_throws(
        ErrorException("Parameters not supported"),
        MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), [x]),
    )
    return
end

function test_gradient_and_hessian()
    x = MOI.VariableIndex.(1:2)
    sin_x = op(:sin, x[1])
    for (f, ret) in Any[
        (2.0*x[1]*x[2])=>(x, Any[2.0*x[2], 2.0*x[1]], [(1, 2)], [2.0]),
        op(:*, x[1], x[2])=>(x, Any[x[2], x[1]], [(1, 2)], [true]),
        sin_x=>([x[1]], Any[op(:cos, x[1])], [(1, 1)], Any[op(:-, sin_x)]),
    ]
        x, ∇f, H, ∇²f = SymbolicAD.gradient_and_hessian(f)
        @test ret[1] == x
        @test all(ret[2] .≈ ∇f)
        @test ret[3] == H
        @test all(ret[4] .≈ ∇²f)
    end
    return
end

function test_gradient_and_hessian_filter()
    x = MOI.VariableIndex.(1:2)
    sin_x = op(:sin, x[1])
    for (f, ret) in Any[
        op(:*, x[1], x[2])=>([x[1]], Any[x[2]], [], []),
        sin_x=>([x[1]], Any[op(:cos, x[1])], [(1, 1)], Any[op(:-, sin_x)]),
    ]
        x, ∇f, H, ∇²f = SymbolicAD.gradient_and_hessian(x -> x.value == 1, f)
        @test ret[1] == x
        @test all(ret[2] .≈ ∇f)
        @test ret[3] == H
        @test all(ret[4] .≈ ∇²f)
    end
    return
end

function test_SymbolicAD_DAG()
    registry = MOI.Nonlinear.OperatorRegistry()
    x = MOI.VariableIndex(1)
    f = Any[3.0, x, 1.0*x+1.0, 2.0*x*x+3.0*x+4.0, op(:sin, x)]
    dag = SymbolicAD._DAG(registry, f)
    @test SymbolicAD._evaluate!(dag, [2.5], Float64[]) === nothing
    @test dag.result[dag.indices] == [3.0, 2.5, 3.5, 24.0, sin(2.5)]
    return
end

function test_SymbolicAD_univariate_registered()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.register_operator(model, :op_foo, 1, x -> x^2)
    MOI.Nonlinear.set_objective(model, :(op_foo($x - 0.5)))
    evaluator = MOI.Nonlinear.Evaluator(model, SymbolicAD.SymbolicMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [1.0]
    @test MOI.eval_objective(evaluator, x) == 0.25
    return
end

end  # module

TestMathOptSymbolicAD.runtests()
