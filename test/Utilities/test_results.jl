# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestResults

using Test

import MathOptInterface as MOI

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

test_hyperrectangle_Int() = _test_hyperrectangle(Int)

test_hyperrectangle_Float64() = _test_hyperrectangle(Float64)

function _test_hyperrectangle(T)
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        T,
    )
    x = MOI.add_variables(model, 2)
    c1 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.HyperRectangle(T[3, -7], T[5, -2]),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.Utilities.vectorize(x .+ T[11, 13]),
        MOI.HyperRectangle(T[-T(6), -T(4)], [T(3), T(2)]),
    )
    MOI.set(model, MOI.ConstraintDual(), c1, T[4, -3])
    MOI.set(model, MOI.ConstraintDual(), c2, T[-2, 5])
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    @test -53 == @inferred MOI.Utilities.get_fallback(
        model,
        MOI.DualObjectiveValue(),
        T,
    )
    return
end

function test_dual_objective_value_open_interval_Interval_variable_index()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(
        inner;
        eval_variable_constraint_dual = false,
    )
    # -Inf <= x[1] <= Inf
    # -Inf <= x[2] <= 2.1
    # -2.2 <= x[3] <= Inf
    # -2.3 <= x[4] <= 2.4
    x = MOI.add_variables(model, 4)
    set = MOI.Interval.([-Inf, -Inf, -2.2, -2.3], [Inf, 2.1, Inf, 2.4])
    c = MOI.add_constraint.(model, x, set)
    for (dual, obj) in [
        [0.0, 0.0, 0.0, 0.0] => 0.0,
        # d[1]
        [-2.0, 0.0, 0.0, 0.0] => 0.0,
        [-1.0, 0.0, 0.0, 0.0] => 0.0,
        [1.0, 0.0, 0.0, 0.0] => 0.0,
        [2.0, 0.0, 0.0, 0.0] => 0.0,
        # d[2]: -(-2.1) = 2.1
        [0.0, -2.0, 0.0, 0.0] => -4.2,
        [0.0, -1.0, 0.0, 0.0] => -2.1,
        [0.0, 1.0, 0.0, 0.0] => 2.1,
        [0.0, 2.0, 0.0, 0.0] => 4.2,
        # d[3]: -(- -2.2) = -2.2
        [0.0, 0.0, -2.0, 0.0] => 4.4,
        [0.0, 0.0, -1.0, 0.0] => 2.2,
        [0.0, 0.0, 1.0, 0.0] => -2.2,
        [0.0, 0.0, 2.0, 0.0] => -4.4,
        # d[4]: -(- -2.3) = -2.3
        # d[4]: -(- 2.4) = 2.4
        [0.0, 0.0, 0.0, -2.0] => -4.8,
        [0.0, 0.0, 0.0, -1.0] => -2.4,
        [0.0, 0.0, 0.0, 1.0] => -2.3,
        [0.0, 0.0, 0.0, 2.0] => -4.6,
        #
        [1.0, 1.0, 1.0, 1.0] => -2.4,
        [-1.0, -1.0, -1.0, -1.0] => -2.3,
    ]
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set.(model, MOI.ConstraintDual(), c, dual)
        MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, obj)
        MOI.set.(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, -obj)
    end
    return
end

function test_dual_objective_value_open_interval_Interval()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(inner)
    # -Inf <= x[1] - 1.1 <= Inf
    # -Inf <= x[2] - 1.2 <= 2.1
    # -2.2 <= x[3] + 1.3 <= Inf
    # -2.3 <= x[4] + 1.4 <= 2.4
    x = MOI.add_variables(model, 4)
    f = x .+ [-1.1, -1.2, 1.3, 1.4]
    set = MOI.Interval.([-Inf, -Inf, -2.2, -2.3], [Inf, 2.1, Inf, 2.4])
    c = MOI.add_constraint.(model, f, set)
    for (dual, obj) in [
        [0.0, 0.0, 0.0, 0.0] => 0.0,
        # d[1]: -(-1.1) = 1.1
        [-2.0, 0.0, 0.0, 0.0] => -2.2,
        [-1.0, 0.0, 0.0, 0.0] => -1.1,
        [1.0, 0.0, 0.0, 0.0] => 1.1,
        [2.0, 0.0, 0.0, 0.0] => 2.2,
        # d[2]: -(-1.2 - 2.1) = 3.3
        [0.0, -2.0, 0.0, 0.0] => -6.6,
        [0.0, -1.0, 0.0, 0.0] => -3.3,
        [0.0, 1.0, 0.0, 0.0] => 3.3,
        [0.0, 2.0, 0.0, 0.0] => 6.6,
        # d[3]: -(1.3 - -2.2) = -3.5
        [0.0, 0.0, -2.0, 0.0] => 7.0,
        [0.0, 0.0, -1.0, 0.0] => 3.5,
        [0.0, 0.0, 1.0, 0.0] => -3.5,
        [0.0, 0.0, 2.0, 0.0] => -7.0,
        # d[4]: -(1.4 - -2.3) = -3.7
        # d[4]: -(1.4 - 2.4) = 1.0
        [0.0, 0.0, 0.0, -2.0] => -2.0,
        [0.0, 0.0, 0.0, -1.0] => -1.0,
        [0.0, 0.0, 0.0, 1.0] => -3.7,
        [0.0, 0.0, 0.0, 2.0] => -7.4,
        #
        [1.0, 1.0, 1.0, 1.0] => -2.8,
        [-1.0, -1.0, -1.0, -1.0] => -1.9,
    ]
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set.(model, MOI.ConstraintDual(), c, dual)
        MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, obj)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, -obj)
    end
    return
end

function test_dual_objective_value_open_interval_Hyperrectangle_variable_index()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(
        inner;
        eval_variable_constraint_dual = false,
    )
    # -Inf <= x[1] <= Inf
    # -Inf <= x[2] <= 2.1
    # -2.2 <= x[3] <= Inf
    # -2.3 <= x[4] <= 2.4
    x = MOI.add_variables(model, 4)
    set = MOI.HyperRectangle([-Inf, -Inf, -2.2, -2.3], [Inf, 2.1, Inf, 2.4])
    c = MOI.add_constraint(model, MOI.VectorOfVariables(x), set)
    for (dual, obj) in [
        [0.0, 0.0, 0.0, 0.0] => 0.0,
        # d[1]
        [-2.0, 0.0, 0.0, 0.0] => 0.0,
        [-1.0, 0.0, 0.0, 0.0] => 0.0,
        [1.0, 0.0, 0.0, 0.0] => 0.0,
        [2.0, 0.0, 0.0, 0.0] => 0.0,
        # d[2]: -(-2.1) = 2.1
        [0.0, -2.0, 0.0, 0.0] => -4.2,
        [0.0, -1.0, 0.0, 0.0] => -2.1,
        [0.0, 1.0, 0.0, 0.0] => 2.1,
        [0.0, 2.0, 0.0, 0.0] => 4.2,
        # d[3]: -(- -2.2) = -2.2
        [0.0, 0.0, -2.0, 0.0] => 4.4,
        [0.0, 0.0, -1.0, 0.0] => 2.2,
        [0.0, 0.0, 1.0, 0.0] => -2.2,
        [0.0, 0.0, 2.0, 0.0] => -4.4,
        # d[4]: -(- -2.3) = -2.3
        # d[4]: -(- 2.4) = 2.4
        [0.0, 0.0, 0.0, -2.0] => -4.8,
        [0.0, 0.0, 0.0, -1.0] => -2.4,
        [0.0, 0.0, 0.0, 1.0] => -2.3,
        [0.0, 0.0, 0.0, 2.0] => -4.6,
        #
        [1.0, 1.0, 1.0, 1.0] => -2.4,
        [-1.0, -1.0, -1.0, -1.0] => -2.3,
    ]
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ConstraintDual(), c, dual)
        MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, obj)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, -obj)
    end
    return
end

function test_dual_objective_value_open_interval_Hyperrectangle()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(inner)
    # -Inf <= x[1] - 1.1 <= Inf
    # -Inf <= x[2] - 1.2 <= 2.1
    # -2.2 <= x[3] + 1.3 <= Inf
    # -2.3 <= x[4] + 1.4 <= 2.4
    x = MOI.add_variables(model, 4)
    f = MOI.Utilities.vectorize(x .+ [-1.1, -1.2, 1.3, 1.4])
    set = MOI.HyperRectangle([-Inf, -Inf, -2.2, -2.3], [Inf, 2.1, Inf, 2.4])
    c = MOI.add_constraint(model, f, set)
    for (dual, obj) in [
        [0.0, 0.0, 0.0, 0.0] => 0.0,
        # d[1]: -(-1.1) = 1.1
        [-2.0, 0.0, 0.0, 0.0] => -2.2,
        [-1.0, 0.0, 0.0, 0.0] => -1.1,
        [1.0, 0.0, 0.0, 0.0] => 1.1,
        [2.0, 0.0, 0.0, 0.0] => 2.2,
        # d[2]: -(-1.2 - 2.1) = 3.3
        [0.0, -2.0, 0.0, 0.0] => -6.6,
        [0.0, -1.0, 0.0, 0.0] => -3.3,
        [0.0, 1.0, 0.0, 0.0] => 3.3,
        [0.0, 2.0, 0.0, 0.0] => 6.6,
        # d[3]: -(1.3 - -2.2) = -3.5
        [0.0, 0.0, -2.0, 0.0] => 7.0,
        [0.0, 0.0, -1.0, 0.0] => 3.5,
        [0.0, 0.0, 1.0, 0.0] => -3.5,
        [0.0, 0.0, 2.0, 0.0] => -7.0,
        # d[4]: -(1.4 - -2.3) = -3.7
        # d[4]: -(1.4 - 2.4) = 1.0
        [0.0, 0.0, 0.0, -2.0] => -2.0,
        [0.0, 0.0, 0.0, -1.0] => -1.0,
        [0.0, 0.0, 0.0, 1.0] => -3.7,
        [0.0, 0.0, 0.0, 2.0] => -7.4,
        #
        [1.0, 1.0, 1.0, 1.0] => -2.8,
        [-1.0, -1.0, -1.0, -1.0] => -1.9,
    ]
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, MOI.ConstraintDual(), c, dual)
        MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, obj)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, -obj)
    end
    return
end

function test_get_fallback_DualObjectiveValue()
    T = Float64
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        T,
    )
    x = MOI.add_variables(model, 2)
    c1 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(x),
        MOI.HyperRectangle(T[3, -7], T[5, -2]),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.Utilities.vectorize(x .+ T[11, 13]),
        MOI.HyperRectangle(T[-T(6), -T(4)], [T(3), T(2)]),
    )
    attr = MOI.DualObjectiveValue()
    @test_throws(
        MOI.GetAttributeNotAllowed(attr, "No dual solution is available"),
        MOI.Utilities.get_fallback(model, attr, T),
    )
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.ConstraintDual(), c1, T[4, -3])
    MOI.set(model, MOI.ConstraintDual(), c2, T[-2, 5])
    @test MOI.Utilities.get_fallback(model, attr, T) == -53
    MOI.set(model, MOI.ResultCount(), 2)
    attr = MOI.DualObjectiveValue(2)
    @test_throws(
        MOI.GetAttributeNotAllowed(attr, "No dual solution is available"),
        MOI.Utilities.get_fallback(model, attr, T),
    )
    return
end

function test_get_fallback_DualObjectiveValue_complex()
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(
        model,
        (1.0 + 2.0im) * x + (3.0 + 4.0im),
        MOI.EqualTo(1.0 - 2.0im),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.Utilities.vectorize([
            (1.0 + 2.0im) * x + (3.0 + 4.0im),
            -1.0im * x + (-2.0 + 5.0im),
        ]),
        MOI.Zeros(2),
    )
    MOI.set(model, MOI.ConstraintDual(), c1, 2.0 - 3.0im)
    MOI.set(model, MOI.ConstraintDual(), c2, [2.0 - 3.0im, -1.0 + 4.0im])
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    for (sense, expected) in ((MOI.MIN_SENSE, -2.0), (MOI.MAX_SENSE, 2.0))
        MOI.set(model, MOI.ObjectiveSense(), sense)
        @test MOI.Utilities.get_fallback(
            model,
            MOI.DualObjectiveValue(),
            Float64,
        ) == expected
    end
    return
end

function test_get_fallback_DualObjectiveValue_Parameter()
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}());
        eval_variable_constraint_dual = false,
    )
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        1.0 * x + 5.0,
    )
    MOI.set(model, MOI.ConstraintDual(), ci, 3.0)
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    for (sense, expected) in ((MOI.MIN_SENSE, 11.0), (MOI.MAX_SENSE, -1.0))
        MOI.set(model, MOI.ObjectiveSense(), sense)
        @test MOI.Utilities.get_fallback(
            model,
            MOI.DualObjectiveValue(),
            Float64,
        ) == expected
    end
    return
end

function test_get_fallback_DualObjectiveValue_Quadratic()
    T = Float64
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        T,
    )
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1.0 * x * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ResultCount(), 1)
    MOI.set(model, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.VariablePrimal(), x, 0.0)
    msg = "Cannot get the dual objective with an objective function of type `$(typeof(f))`"
    @test_throws(
        MOI.GetAttributeNotAllowed(MOI.DualObjectiveValue(), msg),
        MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), T),
    )
    return
end

function test_get_fallback_scalar_nonlinear()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(inner, Float64)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1.0 * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.ScalarNonlinearFunction(:-, Any[x, 1.0])
    c = MOI.add_constraint(model, g, MOI.GreaterThan(0.0))
    MOI.set(model, MOI.ResultCount(), 1)
    MOI.set(model, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.VariablePrimal(), x, 1.0)
    MOI.set(model, MOI.ConstraintDual(), c, 1.0)
    msg = "Cannot get the dual objective with a constraint function of type `$(typeof(g))`"
    @test_throws(
        MOI.GetAttributeNotAllowed(MOI.DualObjectiveValue(), msg),
        MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64),
    )
    return
end

function test_get_fallback_vector_nonlinear()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.MockOptimizer(inner, Float64)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = 1.0 * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g1 = MOI.ScalarNonlinearFunction(:-, Any[x, 1.0])
    g = MOI.VectorNonlinearFunction([g1])
    c = MOI.add_constraint(model, g, MOI.Nonnegatives(1))
    MOI.set(model, MOI.ResultCount(), 1)
    MOI.set(model, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.DualStatus(), MOI.FEASIBLE_POINT)
    MOI.set(model, MOI.VariablePrimal(), x, 1.0)
    MOI.set(model, MOI.ConstraintDual(), c, 1.0)
    msg = "Cannot get the dual objective with a constraint function of type `$(typeof(g))`"
    @test_throws(
        MOI.GetAttributeNotAllowed(MOI.DualObjectiveValue(), msg),
        MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64),
    )
    return
end

end  # module TestResults

TestResults.runtests()
