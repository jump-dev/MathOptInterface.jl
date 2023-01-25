# Copyright (c) 2017: Miles Lubin and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function test_multiobjective_vector_of_variables(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VariableIndex
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), x[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) == x[i]
    end
    return
end

function test_multiobjective_vector_of_variables_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VariableIndex
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), x[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) == x[i]
    end
    MOI.delete(model, x[1])
    attr = MOI.ObjectiveFunction{F}(1)
    @test !(attr in MOI.get(model, MOI.ListOfModelAttributesSet()))
    return
end

function test_multiobjective_vector_of_variables_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VariableIndex
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), x[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) == x[i]
    end
    MOI.delete(model, x)
    # ObjectiveFunction no longer set because we deleted all the variables!
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test !(MOI.ObjectiveFunction{F}(1) in attributes)
    @test !(MOI.ObjectiveFunction{F}(2) in attributes)
    return
end



function test_multiobjective_vector_affine_function(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = [T(1) * x[1], T(2) * x[2] + T(3)]
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_vector_affine_function_modify(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = [T(1) * x[1], T(2) * x[2] + T(3)]
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.modify(
        model,
        MOI.ObjectiveFunction{F}(1),
        MOI.ScalarConstantChange(T(4)),
    )
    f[1].constant = T(4)
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_vector_affine_function_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = [T(1) * x[1], T(2) * x[2] + T(3)]
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.delete(model, x[1])
    g = [zero(F), T(2) * x[2] + T(3)]
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ g[i]
    end
    return
end

function test_multiobjective_vector_affine_function_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = [T(1) * x[1], T(2) * x[2] + T(3)]
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.delete(model, x)
    g = [zero(F), zero(F) + T(3)]
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ g[i]
    end
    return
end

function test_multiobjective_vector_quadratic_function(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    f = T(1) .* x .* x
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_vector_quadratic_function_modify(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    f = T(1) .* x .* x
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.modify(
        model,
        MOI.ObjectiveFunction{F}(1),
        MOI.ScalarConstantChange(T(4)),
    )
    f[1].constant = T(4)
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_vector_quadratic_function_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    f = T(1) .* x .* x
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.delete(model, x[1])
    f[1] = zero(F)
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_vector_quadratic_function_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.ScalarQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    f = T(1) .* x .* x
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    MOI.delete(model, x)
    f[1] = zero(F)
    f[2] = zero(F)
    for i in 1:2
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F
        @test MOI.get(model, MOI.ObjectiveFunction{F}(i)) ≈ f[i]
    end
    return
end

function test_multiobjective_mixed(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = Any[
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
    ]
    @requires MOI.supports(model, MOI.ObjectiveFunction{F[1]}(3))
    @requires MOI.supports(model, MOI.ObjectiveFunction{F[2]}(3))
    @requires MOI.supports(model, MOI.ObjectiveFunction{F[3]}(3))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = Any[
        x[1],
        T(1) * x[1] + T(2),
        T(3) * x[1] * x[1] + T(4) * x[2] + T(5)
    ]
    for i in 1:3
        MOI.set(model, MOI.ObjectiveFunction{typeof(f[i])}(i), f[i])
    end
    for i in 1:3
        @test MOI.get(model, MOI.ObjectiveFunctionType(i)) == F[i]
        @test MOI.get(model, MOI.ObjectiveFunction{F[i]}(i)) ≈ f[i]
    end
    MOI.set(model, MOI.ObjectiveFunction{F[2]}(2), nothing)
    attrs = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test MOI.ObjectiveSense() in attrs
    @test MOI.ObjectiveFunction{F[1]}(1) in attrs
    @test !(MOI.ObjectiveFunction{F[2]}(2) in attrs)
    @test MOI.ObjectiveFunction{F[3]}(3) in attrs
    return
end

function test_multiobjective_solve(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    F = MOI.ScalarAffineFunction{T}
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}(2))
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.add_constraint(model, T(1) * x[1] + x[2], MOI.GreaterThan(T(1)))
    MOI.add_constraint(model, T(1//2) * x[1] + x[2], MOI.GreaterThan(T(3//4)))
    MOI.add_constraint(model, x[1], MOI.GreaterThan(T(0)))
    MOI.add_constraint(model, x[2], MOI.GreaterThan(T(1//4)))
    f = [T(2) * x[1] + x[2], T(1) * x[1] + T(3) * x[2]]
    for i in 1:2
        MOI.set(model, MOI.ObjectiveFunction{F}(i), f[i])
    end
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ResultCount()) >= 1
    for i in 1:MOI.get(model, MOI.ResultCount())
        @test MOI.get(model, MOI.PrimalStatus(i)) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.DualStatus(i)) == MOI.NO_SOLUTION
        sol = MOI.get.(model, MOI.VariablePrimal(i), x)
        @test sum(sol) >= T(1) - config.atol
        @test T(1//2) * sol[1] + sol[2] >= T(3//4) - config.atol
        @test sol[1] >= T(0) - config.atol
        @test sol[2] >= T(1//4) - config.atol
        f_val = [T(2) * sol[1] + sol[2], T(1) * sol[1] + T(3) * sol[2]]
        @test ≈(MOI.get(model, MOI.ObjectiveValue(i, 1)), f_val[1], config)
        @test ≈(MOI.get(model, MOI.ObjectiveValue(i, 2)), f_val[2], config)
    end
    return
end

function setup_test(
    ::typeof(test_multiobjective_solve),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, T[1//2, 1//2]),
            )
        end,
    )
    return
end
