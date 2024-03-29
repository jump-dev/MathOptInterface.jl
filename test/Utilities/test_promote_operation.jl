# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestPromoteOperation

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

function test_promote_operation_1a()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(
        (1, 2) => 3,
        (2, 2) => 3,
        (2, 1) => 3,
        (6, 7) => 8,
        (7, 7) => 8,
        (7, 6) => 8,
    )
    for i in 1:5, j in 1:5
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(+, T, F[i], F[j]) == F[k]
    end
    for i in 6:10, j in 6:10
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(+, T, F[i], F[j]) == F[k]
    end
    return
end

function test_promote_operation_2a()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(2 => 3, 7 => 8)
    for i in 1:10
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(-, T, F[i]) == F[j]
    end
    return
end

function test_promote_operation_2b()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(
        (1, 2) => 3,
        (2, 2) => 3,
        (2, 1) => 3,
        (6, 7) => 8,
        (7, 7) => 8,
        (7, 6) => 8,
    )
    for i in 1:5, j in 1:5
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(-, T, F[i], F[j]) == F[k]
    end
    for i in 6:10, j in 6:10
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(-, T, F[i], F[j]) == F[k]
    end
    return
end

function test_promote_operation_3a()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(2 => 3, 7 => 8)
    for i in 1:10
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(*, T, T, F[i]) == F[j]
    end
    return
end

function test_promote_operation_3b()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(2 => 3, 7 => 8)
    for i in 1:10
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(*, T, F[i], T) == F[j]
    end
    return
end

function test_promote_operation_4a()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(2 => 3, 7 => 8)
    for i in 1:10
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(/, T, F[i], T) == F[j]
    end
    return
end

function test_promote_operation_5a()
    T = Int
    F = (
        T,
        MOI.VariableIndex,
        MOI.ScalarAffineFunction{T},
        MOI.ScalarQuadraticFunction{T},
        MOI.ScalarNonlinearFunction,
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
        MOI.VectorNonlinearFunction,
    )
    special_cases = Dict(
        (1, 2) => 8,
        (2, 1) => 8,
        (1, 7) => 8,
        (7, 1) => 8,
        (2, 6) => 8,
        (6, 2) => 8,
        (6, 7) => 8,
        (7, 6) => 8,
    )
    for i in 1:10, j in 1:10
        k = max(i <= 5 ? i + 5 : i, j <= 5 ? j + 5 : j)
        k = get(special_cases, (i, j), k)
        @test MOI.Utilities.promote_operation(vcat, T, F[i], F[j]) == F[k]
    end
    return
end

function test_promote_operation_6a()
    T = Int
    @test MOI.Utilities.promote_operation(imag, T, MOI.VariableIndex) ==
          MOI.ScalarAffineFunction{T}
    @test MOI.Utilities.promote_operation(imag, T, MOI.VectorOfVariables) ==
          MOI.VectorAffineFunction{T}
    return
end

end  # module

TestPromoteOperation.runtests()
