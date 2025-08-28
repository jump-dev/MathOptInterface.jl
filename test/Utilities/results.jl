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
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, obj)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        d = MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), Float64)
        @test isapprox(d, -obj)
    end
    return
end

end  # module TestResults

TestResults.runtests()
