# Functions

"""
    AbstractFunction

Abstract supertype for function objects.
"""
abstract type AbstractFunction end

"""
    AbstractScalarFunction

Abstract supertype for scalar-valued function objects.
"""
abstract type AbstractScalarFunction <: AbstractFunction end

"""
    AbstractVectorFunction

Abstract supertype for vector-valued function objects.
"""
abstract type AbstractVectorFunction <: AbstractFunction end


"""
    SingleVariable(variable)

The function that extracts the scalar variable referenced by `variable`, a `VariableIndex`.
This function is naturally be used for single variable bounds or integrality constraints.
"""
struct SingleVariable <: AbstractScalarFunction
    variable::VariableIndex
end

"""
    VectorOfVariables(variables)

The function that extracts the vector of variables referenced by `variables`, a `Vector{VariableIndex}`.
This function is naturally be used for constraints that apply to groups of variables, such as an "all different" constraint, an indicator constraint, or a complementarity constraint.
"""
struct VectorOfVariables <: AbstractVectorFunction
    variables::Vector{VariableIndex}
end

"""
    ScalarAffineFunction{T}(variables, coefficients, constant)

The scalar-valued affine function ``a^T x + b``, where:
* ``a`` is a sparse vector specified in tuple form by `variables::Vector{VariableIndex}` and `coefficients::Vector{T}`
* ``b`` is a scalar specified by `constant::T`

Duplicate variable indices in `variables` are accepted, and the corresponding coefficients are summed together.
"""
struct ScalarAffineFunction{T} <: AbstractScalarFunction
    variables::Vector{VariableIndex}
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
struct VectorAffineFunction{T} <: AbstractVectorFunction
    outputindex::Vector{Int}
    variables::Vector{VariableIndex}
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
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableIndex`es) are considered duplicates; only one need be specified.
"""
struct ScalarQuadraticFunction{T} <: AbstractScalarFunction
    affine_variables::Vector{VariableIndex}
    affine_coefficients::Vector{T}
    quadratic_rowvariables::Vector{VariableIndex}
    quadratic_colvariables::Vector{VariableIndex}
    quadratic_coefficients::Vector{T}
    constant::T
end


"""
    VectorQuadraticFunction{T}(affine_outputindex, affine_variables, affine_coefficients, quadratic_outputindex, quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients, constant)

The vector-valued quadratic function with i`th` component ("output index") defined as ``\\frac{1}{2}x^TQ_ix + a_i^T x + b_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by the subset of `affine_variables, affine_coefficients` for the indices `k` where `affine_outputindex[k] == i`.
* ``b_i`` is a scalar specified by `constant[i]`
* ``Q_i`` is a symmetric matrix is specified in triplet form by the subset of `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients` for the indices `k` where `quadratic_outputindex[k] == i`

Duplicate indices in ``a_i`` or ``Q_i`` are accepted, and the corresponding coefficients are summed together.
"Mirrored" indices `(q,r)` and `(r,q)` (where `r` and `q` are `VariableIndex`es) are considered duplicates; only one need be specified.
"""
struct VectorQuadraticFunction{T} <: AbstractVectorFunction
    affine_outputindex::Vector{Int}
    affine_variables::Vector{VariableIndex}
    affine_coefficients::Vector{T}
    quadratic_outputindex::Vector{Int}
    quadratic_rowvariables::Vector{VariableIndex}
    quadratic_colvariables::Vector{VariableIndex}
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

"""
    VectorConstantChange{T}(new_constant)

A struct used to request a change in the constant vector of a vector-valued function.
Applicable to `VectorAffineFunction` and `VectorQuadraticFunction`.
"""
struct VectorConstantChange{T} <: AbstractFunctionModification
    new_constant::Vector{T}
end

"""
    ScalarCoefficientChange{T}(variable, new_coefficient)

A struct used to request a change in the linear coefficient of a single variable
in a scalar-valued function.
Applicable to `ScalarAffineFunction` and `ScalarQuadraticFunction`.
"""
struct ScalarCoefficientChange{T} <: AbstractFunctionModification
    variable::VariableIndex
    new_coefficient::T
end

"""
    MultirowChange{T}(variable, rows, new_coefficients)

A struct used to request a change in the linear coefficients of a single variable
in a vector-valued function.
Applicable to `VectorAffineFunction` and `VectorQuadraticFunction`.
"""
struct MultirowChange{T} <: AbstractFunctionModification
    variable::VariableIndex
    rows::Vector{Int}
    new_coefficients::Vector{T}
end

# Implementation of comparison for MOI functions
import Base: ==

==(f::VectorOfVariables, g::VectorOfVariables) = f.variables == g.variables

# For affine and quadratic functions, terms are compressed in a dictionary using `_dicts` and then the dictionaries are compared with `dict_isapprox`
function dict_isapprox(d1::Dict, d2::Dict{<:Any, T}; kwargs...) where T
    all(kv -> isapprox(kv.second, Base.get(d2, kv.first, zero(T)); kwargs...), d1)
end

# Build a dictionary where the duplicate keys are summed
function sum_dict(kvs::Vector{Pair{K, V}}) where {K, V}
    d = Dict{K, V}()
    for kv in kvs
        key = kv.first
        d[key] = kv.second + Base.get(d, key, zero(V))
    end
    d
end

_dicts(f::ScalarAffineFunction) = sum_dict(Pair.(f.variables, f.coefficients))
_dicts(f::VectorAffineFunction) = sum_dict(Pair.(tuple.(f.outputindex, f.variables), f.coefficients))

# For quadratic terms, x*y == y*x
_sort(v1::VariableIndex, v2::VariableIndex) = VariableIndex.(extrema((v1.value, v2.value)))

_dicts(f::ScalarQuadraticFunction) = (sum_dict(Pair.(f.affine_variables, f.affine_coefficients)),
                                      sum_dict(Pair.(_sort.(f.quadratic_rowvariables, f.quadratic_colvariables), f.quadratic_coefficients)))
_dicts(f::VectorQuadraticFunction) = (sum_dict(Pair.(tuple.(f.affine_outputindex, f.affine_variables), f.affine_coefficients)),
                                      sum_dict(Pair.(tuple.(f.quadratic_outputindex, _sort.(f.quadratic_rowvariables, f.quadratic_colvariables)), f.quadratic_coefficients)))

function Base.isapprox(f::F, g::G; kwargs...) where {F<:Union{ScalarAffineFunction, ScalarQuadraticFunction, VectorAffineFunction, VectorQuadraticFunction},
                                                     G<:Union{ScalarAffineFunction, ScalarQuadraticFunction, VectorAffineFunction, VectorQuadraticFunction}}
    isapprox(f.constant, g.constant; kwargs...) && all(dict_isapprox.(_dicts(f), _dicts(g); kwargs...))
end
