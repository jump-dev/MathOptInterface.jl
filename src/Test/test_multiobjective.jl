# Copyright (c) 2017: Miles Lubin and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function test_multiobjective_vector_of_variables(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorOfVariables
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.VectorOfVariables(x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) == f
    return
end

version_added(::typeof(test_multiobjective_vector_of_variables)) = v"1.12.0"

function test_multiobjective_vector_of_variables_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorOfVariables
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.VectorOfVariables(x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) == f
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ==
          MOI.VectorOfVariables([x[2]])
    return
end

function version_added(::typeof(test_multiobjective_vector_of_variables_delete))
    return v"1.12.0"
end

function test_multiobjective_vector_of_variables_delete_all(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorOfVariables
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.VectorOfVariables(x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) == f
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ==
          MOI.VectorOfVariables([x[2]])
    MOI.delete(model, x[2])
    # ObjectiveFunction no longer set because we deleted all the variables
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test !(MOI.ObjectiveFunction{F}() in attributes)
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_of_variables_delete_all),
)
    return v"1.12.0"
end

function test_multiobjective_vector_of_variables_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorOfVariables
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.VectorOfVariables(x)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) == f
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test MOI.ObjectiveFunction{F}() in attributes
    MOI.delete(model, x)
    # ObjectiveFunction no longer set because we deleted all the variables
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test !(MOI.ObjectiveFunction{F}() in attributes)
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_of_variables_delete_vector),
)
    return v"1.12.0"
end

function test_multiobjective_vector_affine_function(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, T(1) * x[1], T(2) * x[2] + T(3))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

version_added(::typeof(test_multiobjective_vector_affine_function)) = v"1.12.0"

function test_multiobjective_vector_affine_function_modify(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, T(1) * x[1], T(2) * x[2] + T(3))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.modify(
        model,
        MOI.ObjectiveFunction{F}(),
        MOI.VectorConstantChange(T[4, 5]),
    )
    f.constants .= T[4, 5]
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_affine_function_modify),
)
    return v"1.12.0"
end

function test_multiobjective_vector_affine_function_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, T(1) * x[1], T(2) * x[2] + T(3))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈
          MOI.Utilities.operate(vcat, T, T(0), T(2) * x[2] + T(3))
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_affine_function_delete),
)
    return v"1.12.0"
end

function test_multiobjective_vector_affine_function_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, T(1) * x[1], T(2) * x[2] + T(3))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.delete(model, x)
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈
          MOI.VectorAffineFunction{T}(MOI.VectorAffineTerm{T}[], T[0, 3])
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_affine_function_delete_vector),
)
    return v"1.12.0"
end

function test_multiobjective_vector_quadratic_function(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, (T(1) .* x .* x)...)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

function version_added(::typeof(test_multiobjective_vector_quadratic_function))
    return v"1.12.0"
end

function test_multiobjective_vector_quadratic_function_modify(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, (T(1) .* x .* x)...)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.modify(
        model,
        MOI.ObjectiveFunction{F}(),
        MOI.VectorConstantChange(T[4, 5]),
    )
    f.constants .= T[4, 5]
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_quadratic_function_modify),
)
    return v"1.12.0"
end

function test_multiobjective_vector_quadratic_function_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, (T(1) .* x .* x)...)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.delete(model, x[1])
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈
          MOI.Utilities.operate(vcat, T, T(0), T(1) * x[2] * x[2])
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_quadratic_function_delete),
)
    return v"1.12.0"
end

function test_multiobjective_vector_quadratic_function_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorQuadraticFunction{T}
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    f = MOI.Utilities.operate(vcat, T, (T(1) .* x .* x)...)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    MOI.delete(model, x)
    new_f = MOI.VectorQuadraticFunction{T}(
        MOI.VectorQuadraticTerm{T}[],
        MOI.VectorAffineTerm{T}[],
        zeros(T, 2),
    )
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ new_f
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_quadratic_function_delete_vector),
)
    return v"1.12.0"
end

function test_multiobjective_vector_nonlinear(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    f = MOI.VectorNonlinearFunction(
        Any[MOI.ScalarNonlinearFunction(:^, Any[x[1], 2]), x[2]],
    )  # [x[1]^2, x[2]]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    return
end

version_added(::typeof(test_multiobjective_vector_nonlinear)) = v"1.19.0"

function test_multiobjective_vector_nonlinear_delete(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    f = MOI.VectorNonlinearFunction(
        Any[MOI.ScalarNonlinearFunction(:^, Any[x[1], 2]), x[2]],
    )  # [x[1]^2, x[2]]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, x[1])
    return
end

version_added(::typeof(test_multiobjective_vector_nonlinear_delete)) = v"1.19.0"

function test_multiobjective_vector_nonlinear_delete_vector(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    f = MOI.VectorNonlinearFunction(
        Any[MOI.ScalarNonlinearFunction(:^, Any[x[1], 2]), x[2]],
    )  # [x[1]^2, x[2]]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, MOI.ObjectiveFunction{F}()) ≈ f
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, x)
    return
end

function version_added(
    ::typeof(test_multiobjective_vector_nonlinear_delete_vector),
)
    return v"1.19.0"
end

function test_multiobjective_vector_nonlinear_modify(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    F = MOI.VectorNonlinearFunction
    attr = MOI.ObjectiveFunction{F}()
    @requires MOI.supports(model, attr)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    f = MOI.VectorNonlinearFunction(
        Any[MOI.ScalarNonlinearFunction(:^, Any[x[1], 2]), x[2]],
    )  # [x[1]^2, x[2]]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, attr, f)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == F
    @test MOI.get(model, attr) ≈ f
    @test_throws(
        MOI.ModifyObjectiveNotAllowed,
        MOI.modify(model, attr, MOI.VectorConstantChange(T[1, 2])),
    )
    return
end

version_added(::typeof(test_multiobjective_vector_nonlinear_modify)) = v"1.19.0"

function test_multi_objective_solve_and_objective_value(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    F = MOI.VectorAffineFunction{T}
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 3)
    MOI.add_constraint.(model, x, MOI.Interval(zero(T), one(T)))
    f = sum(one(T) * x[i] for i in 1:3)
    MOI.add_constraint(model, f, MOI.LessThan(T(2)))
    g = MOI.Utilities.vectorize([
        T(3) * x[1],
        T(3) * x[1] + T(1) * x[2] + T(2) * x[3],
    ])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ResultCount()) > 0
    for i in 1:MOI.get(model, MOI.ResultCount())
        y = MOI.Utilities.eval_variables(model, g) do xi
            return MOI.get(model, MOI.VariablePrimal(i), xi)
        end
        @test isapprox(MOI.get(model, MOI.ObjectiveValue()), y)
    end
    return
end

function setup_test(
    ::typeof(test_multi_objective_solve_and_objective_value),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        mock -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 0, 1]),
        ),
    )
    return
end

function version_added(::typeof(test_multi_objective_solve_and_objective_value))
    return v"1.50.0"
end
