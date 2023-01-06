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
    # ObjectiveFunction no longer set because we deleted all the variables!
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test !(MOI.ObjectiveFunction{F}() in attributes)
    return
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
    # ObjectiveFunction no longer set because we deleted all the variables!
    attributes = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test !(MOI.ObjectiveFunction{F}() in attributes)
    return
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
