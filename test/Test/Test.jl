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
        "test_model_supports_constraint_SingleVariable_EqualTo",
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
        "test_model_supports_constraint_SingleVariable_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
)
