# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    test_cpsat_AllDifferent(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-AllDifferent constraint.
"""
function test_cpsat_AllDifferent(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.AllDifferent,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:3]
    x = first.(y)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(3))
    MOI.optimize!(model)
    x_val = MOI.get.(model, MOI.VariablePrimal(), x)
    @test abs(x_val[1] - x_val[2]) > 0.5
    @test abs(x_val[1] - x_val[3]) > 0.5
    @test abs(x_val[2] - x_val[3]) > 0.5
    return
end

function setup_test(
    ::typeof(test_cpsat_AllDifferent),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 1, 2]),
        ),
    )
    return
end

"""
    test_cpsat_CountDistinct(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-CountDistinct constraint.
"""
function test_cpsat_CountDistinct(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.CountDistinct,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:4]
    x = first.(y)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountDistinct(4))
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test length(unique(x_val[2:end])) == x_val[1]
    return
end

function setup_test(
    ::typeof(test_cpsat_CountDistinct),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2, 0, 1, 0]),
        ),
    )
    return
end
