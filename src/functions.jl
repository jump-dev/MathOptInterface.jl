# Functions

"""
    AbstractFunction

Abstract supertype for function objects.
"""
abstract type AbstractFunction end

"""
    ScalarVariablewiseFunction(variable)

The function that extracts the scalar variable referenced by `variable`, a `VariableReference`.
This function would naturally be used for single variable bounds or integrality constraints.
"""
struct ScalarVariablewiseFunction <: AbstractFunction
    variable::VariableReference
end

"""
    VectorVariablewiseFunction(variables)

The function that extracts the vector of variables referenced by `variables`, a `Vector{VariableReference}`.
This function would naturally be used for constraints that apply to groups of variables, such as an "all different" constraint, an indicator constraint, or a complementarity constraint.
"""
struct VectorVariablewiseFunction <: AbstractFunction
    variables::Vector{VariableReference}
end

"""
    ScalarAffineFunction{T}(variables, coefficients, constant)

The scalar-valued affine function ``a^T x + b``, where:
* ``a`` is a sparse vector specified in tuple form by `variables::Vector{VariableReference}` and `coefficients::Vector{T}`
* ``b`` is a scalar specified by `constant::T`

Duplicate variable references in `variables` are accepted, and the corresponding coefficients are summed together.
"""
struct ScalarAffineFunction{T} <: AbstractFunction
    varables::Vector{VariableReference}
    coefficients::Vector{T}
    constant::T
end

"""
    VectorAffineFunction{T}(outputindex, variables, coefficients, constant)

The vector-valued affine function ``A x + b``, where:
* ``A`` is a sparse matrix specified in triplet form by `outputindex, variables, coefficients`
* ``b`` is a vector specified by `constant`

Duplicate indices in the ``A`` are accepted, and the corresponding coefficients are summed together.
"""
struct VectorAffineFunction{T} <: AbstractFunction
    outputindex::Vector{Int}
    variables::Vector{VariableReference}
    coefficients::Vector{T}
    constant::Vector{T}
end

"""
    ScalarQuadraticFunction{T}(affine_variables, affine_coefficients, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)

The scalar-valued quadratic function ``\\frac{1}{2}x^TQx + a^T x + b``, where:
* ``a`` is a sparse vector specified in tuple form by `affine_variables, affine_coefficients`
* ``b`` is a scalar specified by `constant`
* ``Q`` is a symmetric matrix is specified in triplet form by `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients`

Duplicate indices in ``a`` or ``Q`` are accepted, and the corresponding coefficients are summed together.
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableReferences`) are considered duplicates; only one need be specified.
"""
struct ScalarQuadraticFunction{T} <: AbstractFunction
    affine_variables::Vector{VariableReference}
    affine_coefficients::Vector{T}
    quadratic_rowvariables::Vector{VariableReference}
    quadratic_colvariables::Vector{VariableReference}
    quadratic_coefficients::Vector{T}
    constant::T
end


"""
    VectorQuadraticFunction{T}(affine_variables, affine_coefficients, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)

The vector-valued quadratic function with i`th` component ("output index") defined as ``\\frac{1}{2}x^TQ_ix + a_i^T x + b_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by the subset of `affine_variables, affine_coefficients` for the indices `k` where `affine_outputindex[k] == i`.
* ``b_i`` is a scalar specified by `constant[i]`
* ``Q_i`` is a symmetric matrix is specified in triplet form by the subset of `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients` for the indices `k` where `quadratic_outputindex[k] == i`

Duplicate indices in ``a_i`` or ``Q_i`` are accepted, and the corresponding coefficients are summed together.
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableReferences`) are considered duplicates; only one need be specified.
"""
struct VectorQuadraticFunction{T} <: AbstractFunction
    affine_outputindex::Vector{Int}
    affine_variables::Vector{VariableReference}
    affine_coefficients::Vector{T}
    quadratic_outputindex::Vector{Int}
    quadratic_rowvariables::Vector{VariableReference}
    quadratic_colvariables::Vector{VariableReference}
    quadratic_coefficients::Vector{T}
    constant::Vector{T}
end

# Function modifications


"""
    AbstractFunctionModification

An abstract supertype for structs which specify partial modifications to functions, to be used for making small modifications instead of replacing the functions entirely.
"""
abstract type AbstractFunctionModification end

"""
    ScalarConstantChange{T}(new_constant)

A struct used to request a change in the constant term of a scalar-valued function.
Applicable to `ScalarAffineFunction` and `ScalarQuadraticFunction`.
"""
struct ScalarConstantChange{T} <: AbstractFunctionModification
    new_constant::T
end
