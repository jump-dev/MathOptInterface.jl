# Copyright (c) 2017: Miles Lubin and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMathOptSymbolicAD

using Test

import MathOptInterface as MOI
import MathOptInterface: Nonlinear

const SymbolicAD = Nonlinear.SymbolicAD

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
op(head, arg::Vector{Any}) = MOI.ScalarNonlinearFunction(head, arg)

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
        1.0*x*y=>y,
        1.0*y*x=>y,
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
        op(:*, x, y, z)=>1.0*y*z,
        op(:*, y, x, z)=>1.0*y*z,
        op(:*, y, z, x)=>1.0*y*z,
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
        op(:^, x, 3)=>3.0*x*x,
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

function test_derivative_univariate_simplification()
    x = MOI.VariableIndex(1)
    @test SymbolicAD.derivative(op(:sin, x), x) ≈ op(:cos, x)
    return
end

function test_derivative_error()
    x = MOI.VariableIndex(1)
    f = op(:foo, x, x)
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
        1.0*x*x+y=>[x, y],
        1.0*x*y=>[x, y],
        1.0*y*x=>[x, y],
        # ::NonlinearExpr
        op(:sin, x)=>[x],
        op(:sin, 1.0 * x + y)=>[x, y],
        op(:*, op(:sin, x), op(:sin, y))=>[x, y],
        op(:^, op(:log, x), 2)=>[x],
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
    @test SymbolicAD.simplify(1.0 * x) == x
    @test SymbolicAD.simplify(2.0 * x) ≈ 2.0 * x
    return
end

function test_simplify_ScalarQuadraticFunction()
    x = MOI.VariableIndex(1)
    terms = MOI.ScalarQuadraticTerm{Float64}[]
    f = MOI.ScalarQuadraticFunction(terms, [MOI.ScalarAffineTerm(1.0, x)], 1.0)
    @test SymbolicAD.simplify(f) ≈ 1.0 * x + 1.0
    f = MOI.ScalarQuadraticFunction(terms, [MOI.ScalarAffineTerm(2.0, x)], 0.0)
    @test SymbolicAD.simplify(f) ≈ 2.0 * x + 0.0
    f = MOI.ScalarQuadraticFunction(terms, [MOI.ScalarAffineTerm(1.0, x)], 0.0)
    @test SymbolicAD.simplify(f) == x
    f = MOI.ScalarQuadraticFunction(terms, MOI.ScalarAffineTerm{Float64}[], 2.0)
    @test SymbolicAD.simplify(f) === 2.0
    @test SymbolicAD.simplify(1.0 * x * x + 1.0) ≈ 1.0 * x * x + 1.0
    g = 1.0 * x * x + 2.0 * x * x + 1.0
    @test SymbolicAD.simplify(g) ≈ 3.0 * x * x + 1.0
    return
end

function test_simplify_ScalarNonlinearFunction()
    x = MOI.VariableIndex(1)
    # sin(3 * (x^0)) -> sin(3)
    f = op(:^, x, 0)
    g = op(:*, 3, f)
    h = op(:sin, g)
    @test SymbolicAD.simplify(h) ≈ sin(3)
    # sin(log(x)) -> sin(log(x))
    f = op(:log, x)
    g = op(:sin, f)
    @test SymbolicAD.simplify(g) ≈ g
    return
end

# simplify(::Val{:*}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_multiplication()
    x, y, z = MOI.VariableIndex.(1:3)
    sinx = op(:sin, x)
    # *(x, *(y, z)) -> *(x, y, z)
    @test ≈(SymbolicAD.simplify(op(:*, x, op(:*, y, z))), op(:*, x, y, z))
    # *(x, *(y, z, *(x, 2))) -> *(x, y, z, x, 2)
    f = op(:*, x, 2)
    @test ≈(
        SymbolicAD.simplify(op(:*, x, op(:*, y, z, f))),
        op(:*, x, y, z, x, 2),
    )
    # *(x, 3, 2) -> *(x, 6)
    @test ≈(SymbolicAD.simplify(op(:*, x, 3, 2)), 6.0 * x)
    @test ≈(SymbolicAD.simplify(op(:*, sinx, 3, 2)), op(:*, sinx, 6))
    # *(3, x, 2) -> *(6, x)
    @test ≈(SymbolicAD.simplify(op(:*, 3, x, 2)), 6.0 * x)
    @test ≈(SymbolicAD.simplify(op(:*, 3, sinx, 2)), op(:*, 6, sinx))
    # *(x, 1) -> x
    ret = op(:*, x, 1)
    @test ≈(SymbolicAD.simplify(ret), x)
    # *(x, 0) -> 0
    ret = op(:*, x, 0)
    @test ≈(SymbolicAD.simplify(ret), 0)
    # *(-(x, x), 1) -> 0
    f = op(:-, x, x)
    ret = op(:*, f, 1)
    @test ≈(SymbolicAD.simplify(ret), 0)
    # *() -> true
    ret = op(:*, Any[])
    @test ≈(SymbolicAD.simplify(ret), 1)
    return
end

# simplify(::Val{:+}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_addition()
    x, y, z = MOI.VariableIndex.(1:3)
    sinx = op(:sin, x)
    cosx = op(:cos, x)
    # (+(x, +(y, z)))=>(+(x, y, z)),
    @test ≈(SymbolicAD.simplify(op(:+, sinx, op(:+, y, z))), op(:+, sinx, y, z))
    @test ≈(
        SymbolicAD.simplify(op(:+, x, op(:+, y, z))),
        1.0 * x + 1.0 * y + 1.0 * z,
    )
    # +(sin(x), -cos(x))=>sin(x)-cos(x),
    @test ≈(SymbolicAD.simplify(op(:+, sinx, op(:-, cosx))), op(:-, sinx, cosx))
    # (+(x, 1, 2))=>(+(x, 3)),
    @test ≈(SymbolicAD.simplify(op(:+, x, 1, 2)), x + 3.0)
    @test ≈(SymbolicAD.simplify(op(:+, sinx, 1, 2)), op(:+, sinx, 3))
    # (+(1, x, 2))=>(+(3, x)),ret =
    @test ≈(SymbolicAD.simplify(op(:+, 1, x, 2)), x + 3.0)
    @test ≈(SymbolicAD.simplify(op(:+, 1, sinx, 2)), op(:+, 3, sinx))
    # +(x, 0) -> x
    @test SymbolicAD.simplify(op(:+, x, 0)) ≈ x
    # +(0, x) -> x
    @test SymbolicAD.simplify(op(:+, 0, x)) ≈ x
    # +(-(x, x), 0) -> 0
    @test SymbolicAD.simplify(op(:+, op(:-, x, x), 0)) === false
    return
end

# simplify(::Val{:-}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_subtraction()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    sinx = op(:sin, x)
    f = op(:-, x)
    # -x -> -x
    @test SymbolicAD.simplify(op(:-, x)) ≈ -1.0 * x
    @test SymbolicAD.simplify(op(:-, sinx)) ≈ op(:-, sinx)
    # -(-(x)) -> x
    ret = op(:-, f)
    @test SymbolicAD.simplify(ret) ≈ x
    # -(x, 0) -> x
    ret = op(:-, x, 0)
    @test SymbolicAD.simplify(ret) ≈ x
    # -(0, x) -> -x
    @test SymbolicAD.simplify(op(:-, 0, sinx)) ≈ op(:-, sinx)
    # -(x, x) -> 0
    ret = op(:-, x, x)
    @test SymbolicAD.simplify(ret) ≈ 0
    # -(x, -y) -> +(x, y)
    @test SymbolicAD.simplify(op(:-, x, op(:-, y))) ≈ 1.0 * x + 1.0 * y
    @test SymbolicAD.simplify(op(:-, sinx, op(:-, y))) ≈ op(:+, sinx, y)
    # -(x, y) -> -(x, y)
    @test SymbolicAD.simplify(op(:-, sinx, y)) ≈ op(:-, sinx, y)
    @test SymbolicAD.simplify(op(:-, x, y)) ≈ 1.0 * x - 1.0 * y
    return
end

# simplify(::Val{:^}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_power()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    # x^0 -> 1
    f = op(:^, x, 0)
    @test SymbolicAD.simplify(f) == 1
    # x^1 -> x
    f = op(:^, x, 1)
    @test SymbolicAD.simplify(f) == x
    # 0^x -> 0
    f = op(:^, 0, x)
    @test SymbolicAD.simplify(f) == 0
    # 1^x -> 1
    f = op(:^, 1, x)
    @test SymbolicAD.simplify(f) == 1
    # x^y -> x^y
    f = op(:^, x, y)
    @test SymbolicAD.simplify(f) ≈ f
    return
end

# simplify(::Val{:ifelse}, f::MOI.ScalarNonlinearFunction)
function test_simplify_ScalarNonlinearFunction_ifelse()
    x, y, z = MOI.VariableIndex(1), MOI.VariableIndex(2), MOI.VariableIndex(3)
    f = op(:ifelse, false, x, y)
    @test SymbolicAD.simplify(f) == y
    f = op(:ifelse, true, x, y)
    @test SymbolicAD.simplify(f) == x
    f = op(:ifelse, y, x, x)
    @test SymbolicAD.simplify(f) == x
    f = op(:ifelse, x, y, z)
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
    y = op(:+, Any[op(:^, xi, 2) for xi in x])
    x_plus = [op(:+, xi) for xi in x]
    function wrap(f)
        return op(:+, op(:-, f, 0.0), 0.0)
    end
    f = MOI.VectorNonlinearFunction(wrap.([y; x_plus]))
    g = MOI.Utilities.vectorize([sum(1.0 .* x .* x); x])
    @test SymbolicAD.simplify(f) ≈ g
    return
end

function test_simplify_VectorNonlinearFunction_abstract()
    x = MOI.VariableIndex(1)
    f = MOI.VectorNonlinearFunction([x, 1.0 * x, op(:sin, x)])
    g = MOI.VectorNonlinearFunction([x, x, op(:sin, x)])
    @test SymbolicAD.simplify(f) ≈ g
    return
end

function test_simplify_deep()
    N = 10_000
    x = MOI.VariableIndex.(1:N)
    f = op(:^, x[1], 1)
    for i in 2:N
        g = op(:^, x[i], 1)
        f = op(:+, f, g)
    end
    ret = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0)
    @test ≈(SymbolicAD.simplify(f), ret)
    return
end

function test_simplify_shared_false()
    x = MOI.VariableIndex(1)
    f = op(:^, x, 1)
    g = op(:+, f, f)
    h = op(:-, g, g)
    @test ≈(SymbolicAD.simplify!(h), false)
    return
end

function test_simplify_shared_node()
    x = MOI.VariableIndex(1)
    f1 = op(:^, x, 1)
    f2 = op(:sin, f1)
    f3 = op(:cos, f1)
    f4 = op(:+, f2, f3)
    g1 = op(:sin, x)
    g2 = op(:cos, x)
    g3 = op(:+, g1, g2)
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), x)
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), [x])
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), [x])
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), [x])
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), x)
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
        MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), x),
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
        MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), [x]),
    )
    return
end

function test_gradient_and_hessian()
    x = MOI.VariableIndex.(1:2)
    sin_x = op(:sin, x[1])
    for (f, ret) in Any[
        (2.0*x[1]*x[2])=>(x, Any[2.0*x[2], 2.0*x[1]], [(1, 2)], [2.0]),
        (2.0*x[2]*x[1])=>(x, Any[2.0*x[2], 2.0*x[1]], [(1, 2)], [2.0]),
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
    evaluator = MOI.Nonlinear.Evaluator(model, Nonlinear.SymbolicMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [1.0]
    @test MOI.eval_objective(evaluator, x) == 0.25
    return
end

function test_simplify_if_quadratic()
    x = MOI.VariableIndex(1)
    for (f, ret) in Any[
        # Constants
        op(:*, 2)=>2,
        op(:*, 2 // 3)=>2/3,
        op(:*, 2, 3)=>6,
        op(:+, 2, 3)=>5,
        op(:-, 2)=>-2,
        op(:-, 2, 3)=>-1,
        # Affine
        op(:*, 2, x, 3)=>6*x,
        op(:-, x)=>-1*x,
        op(:-, x, 2)=>1.0*x-2.0,
        op(:-, 2, x)=>2.0+-1.0*x,
        op(:+, 2, x)=>2+x,
        op(:+, x, x)=>2.0*x,
        op(:+, x, 2, x)=>2.0*x+2.0,
        op(:+, x, 2, op(:+, x))=>2.0*x+2.0,
        op(:+, 1.0 * x, 1.0 * x + 2.0)=>2.0*x+2.0,
        op(:-, 1.0 * x + 2.0)=>-1.0*x-2.0,
        op(:*, 2, 1.0 * x + 2.0, 3)=>6.0*x+12.0,
        # Quadratic
        op(:+, 1.0 * x * x)=>1.0*x*x,
        op(:*, 2, x, 3, x)=>6.0*x*x,
        op(:*, 2, x, 3, 1.0 * x)=>6.0*x*x,
        op(:*, 2, x, 3, 1.0 * x + 2.0)=>6.0*x*x+12.0*x,
        op(:*, 2, 1.0 * x + 2.0, 3, x)=>6.0*x*x+12.0*x,
        op(:*, 1.0 * x + 2.0, 1.0 * x)=>1.0*x*x+2.0*x,
        op(:*, 1.0 * x + 2.0, 1.0 * x + 1.0)=>1.0*x*x+3.0*x+2.0,
        # Power
        op(:^, x, 2)=>1.0*x*x,
        op(:^, 1.0 * x, 2)=>1.0*x*x,
        op(:^, 1.0 * x + 1.0, 2)=>1.0*x*x+2.0*x+1.0,
        op(:^, -2.0 * x - 2.0, 2)=>4.0*x*x+8.0*x+4.0,
        # Divide
        op(:/, x, 2)=>0.5*x,
        op(:/, 1.0 * x, 2)=>0.5*x,
        op(:/, 2.0 * x, 2)=>x,
        op(:/, 2.0 * x * x, 2)=>1.0*x*x,
        # Early termination because not affine
        op(:+, op(:sin, x))=>nothing,
        op(:-, op(:sin, x))=>nothing,
        op(:-, op(:sin, x), 1)=>nothing,
        op(:-, x, op(:sin, x))=>nothing,
        op(:*, 2, 3, op(:sin, x))=>nothing,
        op(:log, x)=>nothing,
        op(:+, big(1) * x * x, big(2))=>nothing,
        op(:+, big(1) * x, big(2))=>nothing,
        op(:+, x, big(2))=>nothing,
        op(:+, x, 1+2im)=>nothing,
    ]
        @test SymbolicAD._simplify_if_quadratic!(f) ≈ something(ret, f)
    end
    return
end

end  # module

TestMathOptSymbolicAD.runtests()
