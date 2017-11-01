# Functions

"""
    AbstractFunction{V}

Abstract supertype for function objects. The parameter `V` indicates the variable type, usually `VariableReference`.
"""
abstract type AbstractFunction{V} end

"""
    AbstractScalarFunction{V}

Abstract supertype for scalar-valued function objects. The parameter `V` indicates the variable type, usually `VariableReference`.

"""
abstract type AbstractScalarFunction{V} <: AbstractFunction{V} end

"""
    AbstractVectorFunction{V}

Abstract supertype for vector-valued function objects. The parameter `V` indicates the variable type, usually `VariableReference`.

"""
abstract type AbstractVectorFunction{V} <: AbstractFunction{V} end

# These generic functions are not used directly in MOI, but are useful in related interfaces where other objects stand in for VariableReference.

struct GenericSingleVariable{V} <: AbstractScalarFunction{V}
    variable::V
end

struct GenericVectorOfVariables{V} <: AbstractVectorFunction{V}
    variables::Vector{V}
end

struct GenericScalarAffineFunction{V,T} <: AbstractScalarFunction{V}
    variables::Vector{V}
    coefficients::Vector{T}
    constant::T
end

struct GenericVectorAffineFunction{V,T} <: AbstractVectorFunction{V}
    outputindex::Vector{Int}
    variables::Vector{V}
    coefficients::Vector{T}
    constant::Vector{T}
end

struct GenericScalarQuadraticFunction{V,T} <: AbstractScalarFunction{V}
    affine_variables::Vector{V}
    affine_coefficients::Vector{T}
    quadratic_rowvariables::Vector{V}
    quadratic_colvariables::Vector{V}
    quadratic_coefficients::Vector{T}
    constant::T
end

struct GenericVectorQuadraticFunction{V,T} <: AbstractVectorFunction{V}
    affine_outputindex::Vector{Int}
    affine_variables::Vector{V}
    affine_coefficients::Vector{T}
    quadratic_outputindex::Vector{Int}
    quadratic_rowvariables::Vector{V}
    quadratic_colvariables::Vector{V}
    quadratic_coefficients::Vector{T}
    constant::Vector{T}
end

"""
    SingleVariable(variable)

The function that extracts the scalar variable referenced by `variable`, a `VariableReference`.
This function is naturally be used for single variable bounds or integrality constraints.
"""
const SingleVariable = GenericSingleVariable{VariableReference}

"""
    VectorOfVariables(variables)

The function that extracts the vector of variables referenced by `variables`, a `Vector{VariableReference}`.
This function is naturally be used for constraints that apply to groups of variables, such as an "all different" constraint, an indicator constraint, or a complementarity constraint.
"""
const VectorOfVariables = GenericVectorOfVariables{VariableReference}

"""
    ScalarAffineFunction{T}(variables, coefficients, constant)

The scalar-valued affine function ``a^T x + b``, where:
* ``a`` is a sparse vector specified in tuple form by `variables::Vector{VariableReference}` and `coefficients::Vector{T}`
* ``b`` is a scalar specified by `constant::T`

Duplicate variable references in `variables` are accepted, and the corresponding coefficients are summed together.
"""
const ScalarAffineFunction = GenericScalarAffineFunction{VariableReference}

"""
    VectorAffineFunction{T}(outputindex, variables, coefficients, constant)

The vector-valued affine function ``A x + b``, where:
* ``A`` is a sparse matrix specified in triplet form by `outputindex, variables, coefficients`
* ``b`` is a vector specified by `constant`

Duplicate indices in the ``A`` are accepted, and the corresponding coefficients are summed together.
"""
const VectorAffineFunction = GenericVectorAffineFunction{VariableReference}

"""
    ScalarQuadraticFunction{T}(affine_variables, affine_coefficients, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)

The scalar-valued quadratic function ``\\frac{1}{2}x^TQx + a^T x + b``, where:
* ``a`` is a sparse vector specified in tuple form by `affine_variables, affine_coefficients`
* ``b`` is a scalar specified by `constant`
* ``Q`` is a symmetric matrix is specified in triplet form by `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients`

Duplicate indices in ``a`` or ``Q`` are accepted, and the corresponding coefficients are summed together.
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableReferences`) are considered duplicates; only one need be specified.
"""
const ScalarQuadraticFunction = GenericScalarQuadraticFunction{VariableReference}


"""
    VectorQuadraticFunction{T}(affine_outputindex, affine_variables, affine_coefficients, quadratic_outputindex, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)

The vector-valued quadratic function with i`th` component ("output index") defined as ``\\frac{1}{2}x^TQ_ix + a_i^T x + b_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by the subset of `affine_variables, affine_coefficients` for the indices `k` where `affine_outputindex[k] == i`.
* ``b_i`` is a scalar specified by `constant[i]`
* ``Q_i`` is a symmetric matrix is specified in triplet form by the subset of `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients` for the indices `k` where `quadratic_outputindex[k] == i`

Duplicate indices in ``a_i`` or ``Q_i`` are accepted, and the corresponding coefficients are summed together.
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableReferences`) are considered duplicates; only one need be specified.
"""
const VectorQuadraticFunction = GenericVectorQuadraticFunction{VariableReference}


# Function modifications


"""
    AbstractFunctionModification

An abstract supertype for structs which specify partial modifications to functions, to be used for making small modifications instead of replacing the functions entirely. The parameter `V` indicates the variable type, usually `VariableReference`.
"""
abstract type AbstractFunctionModification{V} end

struct GenericScalarConstantChange{V,T} <: AbstractFunctionModification{V}
    new_constant::T
end

struct GenericVectorConstantChange{V,T} <: AbstractFunctionModification{V}
    new_constant::Vector{T}
end

struct GenericScalarCoefficientChange{V,T} <: AbstractFunctionModification{V}
    variable::V
    new_coefficient::T
end

struct GenericMultirowChange{V,T} <: AbstractFunctionModification{V}
    variable::V
    rows::Vector{Int}
    new_coefficients::Vector{T}
end

"""
    ScalarConstantChange{T}(new_constant)

A struct used to request a change in the constant term of a scalar-valued function.
Applicable to `ScalarAffineFunction` and `ScalarQuadraticFunction`.
"""
const ScalarConstantChange = GenericScalarConstantChange{VariableReference}

"""
    VectorConstantChange{T}(new_constant)

A struct used to request a change in the constant vector of a vector-valued function.
Applicable to `VectorAffineFunction` and `VectorQuadraticFunction`.
"""
const VectorConstantChange = GenericVectorConstantChange{VariableReference}

"""
    ScalarCoefficientChange{T}(variable, new_coefficient)

A struct used to request a change in the linear coefficient of a single variable
in a scalar-valued function.
Applicable to `ScalarAffineFunction` and `ScalarQuadraticFunction`.
"""
const ScalarCoefficientChange = GenericScalarCoefficientChange{VariableReference}

"""
    MultirowChange{T}(variable, rows, new_coefficients)

A struct used to request a change in the linear coefficients of a single variable
in a vector-valued function.
Applicable to `VectorAffineFunction` and `VectorQuadraticFunction`.
"""
const MultirowChange = GenericMultirowChange{VariableReference}
