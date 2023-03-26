# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

import MathOptInterface as MOI

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
)

# Run the previously excluded tests, this time without UniversalFallback.

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}(),
        scalar_function_constant_non_zero = true,
    ),
    MOI.Test.Config(),
    include = [
        r"^test_model_ScalarFunctionConstantNotZero$",
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
)

# Test for Issue #1757

MOI.Test.test_model_ScalarFunctionConstantNotZero(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}(),
        scalar_function_constant_non_zero = false,
    ),
    MOI.Test.Config(exclude = Any[MOI.ConstraintFunction]),
)

# Test exclude_tests_after. This should work despite no methods being added for IncompleteOptimizer
# because every test should get skipped.

struct IncompleteOptimizer <: MOI.AbstractOptimizer end

MOI.Test.runtests(
    IncompleteOptimizer(),
    MOI.Test.Config();
    exclude_tests_after = v"0.0.1",
)

# Non-Float64 tests

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{BigFloat}()),
        BigFloat,
    ),
    MOI.Test.Config(BigFloat),
    exclude = [
        # ========================= Expected failures ==========================
        # UniversalFallback supports these tests.
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
)

# Special test for issue #2010

struct _UnsupportedModel <: MOI.ModelLike end

function MOI.supports_constraint(
    ::_UnsupportedModel,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
)
    return true
end

MOI.Test.test_attribute_unsupported_constraint(
    _UnsupportedModel(),
    MOI.Test.Config(),
)

@testset "test_attribute_unsupported_constraint_fallback" begin
    model = _UnsupportedModel()
    F, S = MOI.VariableIndex, MOI.ZeroOne
    attr = MOI.ListOfConstraintIndices{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
    attr = MOI.NumberOfConstraints{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
end
