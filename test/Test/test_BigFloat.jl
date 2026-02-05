# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestTestBigFloat

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

function test_bigfloat_tests()
    MOI.Test.runtests(
        MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{BigFloat}()),
            BigFloat,
        ),
        MOI.Test.Config(BigFloat),
        exclude = [
            # ========================= Expected failures ======================
            # UniversalFallback supports these tests.
            "test_model_copy_to_UnsupportedAttribute",
            "test_model_copy_to_UnsupportedConstraint",
            "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
            "test_model_supports_constraint_VariableIndex_EqualTo",
            "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
        ],
    )
    return
end

end  # module

TestTestBigFloat.runtests()
