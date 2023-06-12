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
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(
        (1, 2) => 3,
        (2, 2) => 3,
        (2, 1) => 3,
        (5, 6) => 7,
        (6, 6) => 7,
        (6, 5) => 7,
    )
    for i in 1:4, j in 1:4
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(+, T, F[i], F[j]) == F[k]
    end
    for i in 5:8, j in 5:8
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
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(2 => 3, 6 => 7)
    for i in 1:8
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
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(
        (1, 2) => 3,
        (2, 2) => 3,
        (2, 1) => 3,
        (5, 6) => 7,
        (6, 6) => 7,
        (6, 5) => 7,
    )
    for i in 1:4, j in 1:4
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(-, T, F[i], F[j]) == F[k]
    end
    for i in 5:8, j in 5:8
        k = get(special_cases, (i, j), max(i, j))
        @test MOI.Utilities.promote_operation(-, T, F[i], F[j]) == F[k]
    end
    return
end

function test_promote_operation_2c()
    T = Int
    F = (
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(2 => 3)
    for i in 1:4
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(-, T, F[i], F[1]) == F[j]
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
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(2 => 3, 6 => 7)
    for i in 1:8
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(*, T, T, F[i]) == F[j]
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
        Vector{T},
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{T},
        MOI.VectorQuadraticFunction{T},
    )
    special_cases = Dict(2 => 3, 6 => 7)
    for i in 1:8
        j = get(special_cases, i, i)
        @test MOI.Utilities.promote_operation(/, T, F[i], T) == F[j]
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
