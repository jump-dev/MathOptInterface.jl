using MathOptInterface
const MOI = MathOptInterface

# Some tests are excluded because UniversalFallback accepts absolutely
# everything.

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    ),
    MOI.Test.Config(),
    exclude = [
        "test_model_ScalarFunctionConstantNotZero",
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
        "test_model_ScalarFunctionConstantNotZero",
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
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

# TODO(odow): fix excluded tests

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{BigFloat}()),
    ),
    MOI.Test.Config(BigFloat),
    include = ["test_constraint_", "test_linear_", "test_variable"],
    exclude = String[
        "test_constraint_qcp_duplicate_off_diagonal",
        "test_linear_VectorAffineFunction_empty_row",
        "test_linear_add_constraints",
        "test_linear_integer_integration",
        "test_linear_integration",
    ],
)
