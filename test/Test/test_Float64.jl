# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestTestFloat64

using Test
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_runtests()
    # Some tests are excluded because UniversalFallback accepts absolutely
    # everything.
    MOI.Test.runtests(
        MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        ),
        MOI.Test.Config(),
        exclude = [
            r"^test_model_ScalarFunctionConstantNotZero$",
            "test_model_copy_to_UnsupportedAttribute",
            "test_model_copy_to_UnsupportedConstraint",
            "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
            "test_model_supports_constraint_VariableIndex_EqualTo",
            "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
        ],
        warn_unsupported = true,
        verbose = true,
    )
    # Run the previously excluded tests, this time without UniversalFallback.
    MOI.Test.runtests(
        MOI.Utilities.MockOptimizer(
            MOI.Utilities.Model{Float64}(),
            scalar_function_constant_non_zero = true,
        ),
        MOI.Test.Config();
        include = [
            r"^test_model_ScalarFunctionConstantNotZero$",
            "test_model_copy_to_UnsupportedAttribute",
            "test_model_copy_to_UnsupportedConstraint",
            "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
            "test_model_supports_constraint_VariableIndex_EqualTo",
            "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
        ],
        verbose = true,
    )
    return
end

end  # module

TestTestFloat64.runtests()
